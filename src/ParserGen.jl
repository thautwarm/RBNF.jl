AST = Any

struct PContext
    lexer_table :: Vector{Tuple{Symbol, Function}}
    ignores :: Vector{AST}
    tokens :: Vector{Tuple{Symbol, AST}}
    grammars :: Vector{Tuple{Symbol, AST}}
end

const r_str_v = Symbol("r_str")
function collect_lexer!(lexers, name, node)
    @match node begin
        c::Union{Char, String, Regex} => push!(lexers, (name, LexerSpec(c)))
        Expr(:macrocall, &r_str_v, ::LineNumberNode, s) => push!(lexers, (name, LexerSpec(Regex(s))))
        Expr(head, tail...) => foreach(x -> collect_lexer!(lexers, name, x), tail)
        _ => nothing
    end
    nothing
end

@active IsMacro{s}(x) begin
    @match x begin
        Expr(:macrocall, &(Symbol("@", s)), ::LineNumberNode) => true
        _ => false
    end
end


@active MacroSplit{s}(x) begin
    @match x begin
        Expr(:macrocall, &(Symbol("@", s)), ::LineNumberNode, arg) => arg
        _ => nothing
    end
end

function collect_context(node)
    stmts = @match node begin
        quote
            $(stmts...)
        end => stmts
    end
    ignores = []
    tokens = []
    grammars = []
    collector = nothing
    unnamed_lexers = OrderedSet{Tuple{Symbol, LexerSpec{T}} where T}()
    for stmt in stmts
        @match stmt begin
            ::LineNumberNode => ()
            IsMacro{:token} =>
                begin
                    collector = @λ begin
                        :($name := $node) ->  push!(tokens, (name, node))
                    end
                end
            IsMacro{:grammar} =>
                begin
                    collector = @λ begin
                        :($name := $node) ->
                            begin
                                collect_lexer!(unnamed_lexers, :unnamed, node)
                                push!(grammars, (name, node))
                            end
                    end
                end
            :(ignore{$(ignores_...)}) =>
                begin
                    append!(ignores, ignores_)
                end
            ::String => nothing # document?
            a => collector(a)
        end
    end
    lexers = OrderedSet{Tuple{Symbol, LexerSpec{T}} where T}()
    for (k, v) in tokens
        collect_lexer!(lexers, k, v)
    end
    union!(lexers, unnamed_lexers)
    lexer_table :: Vector{Tuple{Symbol, Function}} = [(k, mklexer(v)) for (k, v) in lexers]
    PContext(lexer_table, ignores, tokens, grammars)
end

function parser_gen(pc :: PContext)
    decl_seqs = []
    for (each, _) in  pc.tokens
        push!(decl_seqs, quote
            function $each()
                f(::$Token{$(QuoteNode(each))}) = true
                f(_) = false
                $tokenparser(f)
            end
        end)
    end

    for (each, _) in pc.grammars
        push!(decl_seqs, quote
            function $each
            end
        end)
    end
    struct_defs = []
    parser_defs = []
    for (k, v) in pc.grammars
        (struct_def, parser_def) = grammar_gen(k, v)
        push!(struct_defs, struct_def)
        push!(parser_defs, parser_def)
    end

    append!(decl_seqs, struct_defs)
    append!(decl_seqs, parser_defs)

    # make lexer
    lexer_table = pc.lexer_table
    push!(decl_seqs, :(runlexer_no_ignore = $(genlex(lexer_table))))
    names = :($Set([$(map(QuoteNode, pc.ignores))...]))
    push!(decl_seqs, :(runlexer(str) = $filter(x -> !(x in $names),  runlexer_no_ignore(str))))

    Expr(:block, decl_seqs...)
end


function analyse_closure!(vars, node)
    @match node begin
        :($a :: $t = $n) || :($a :: $t << $n)  =>
            begin
                vars[a] = t
                analyse_closure!(vars, n)
            end
        :($a = $n) || :($a << $n) =>
            begin
                get!(vars, a) do
                    Any
                end
            end
        Expr(head, args...) =>
            for each in args
                analyse_closure!(vars, each)
            end
        _ => nothing
    end
end

to_struct_name(name) = Symbol("__struct__", name)

function grammar_gen(name::Symbol, def_body)
    vars = Dict{Symbol, Any}()
    analyse_closure!(vars, def_body)
    struct_name = to_struct_name(name)
    parser_skeleton_name = gensym()
    state_name = gensym()
    let vars = collect(vars)
        struct_def = quote
            struct $struct_name
                $([Expr(:(::), k, v) for (k, v) in vars]...)
            end
        end

        parser_def = quote
            $parser_skeleton_name = $(make(def_body, name))
            function $name(ctx_in)
                $state_name = $struct_name($([:($crate($v)) for v in values(vars)]...))
                ctx = $update_state(ctx_in, $state_name)
                res = $parser_skeleton_name(ctx)
                if res[1] !== nothing
                    (ctx.state, ctx[2])
                else
                    ctx
                end
            end
        end
        (struct_def, parser_def)
    end
end


function make(node, top::Symbol)
    makerec(node) = make(node, top)
    @match node begin
        MacroSplit{:r_str}(s) =>
            let r = Regex(s)
                :($tokenparser(x -> $match($r, x.str) !== nothing))
            end
        s::String => :($tokenparser(x -> x.str == $s))
        c::Char =>
            let s = String([c])
                :($tokenparser(x -> x.str == $s))
            end
        :[$(elts...)] =>
            let elt_ps = map(makerec, elts)
                :($hlistparser($(elt_ps...)))
            end
        :($(elts...), ) =>
            let elt_ps = map(makerec, elts)
                :($htupleparser($(elt_ps...)))
            end
        :($a | $b) =>
            let pa = makerec(a), pb = makerec(pb)
                :($orparser($pa, $pb))
            end
        MacroSplit{:or}(quote $(nodes...) end) =>
            foldl(map(makerec, filter(x -> !(x isa LineNumberNode), nodes))) do prev, each
                :($orparser($prev, $each))
            end

        :(Many($a)) =>
            let pa = makerec(a)
                :($manyparser($pa))
            end
        :($a => $f) =>
            let pa = makerec(a)
                :($trans($f, $pa))
            end
        :(Option($a)) =>
            let pa = makerec(a)
                :($optparser($pa))
            end
        :($var = $a) || :($var :: $_ = $a) =>
            let pa = makerec(a)
                quote
                $updateparser($pa,
                    let fields = $fieldnames($(to_struct_name(top)))
                        (old_ctx, new_var) -> $runlens(old_ctx, new_var, Lens{$top, fields, var})
                    end)
                end
            end
        :($var << $a) || :($var :: $_ << $a) =>
            let pa = makerec(a)
                quote
                $updateparser($pa,
                    let fields = $fieldnames($(to_struct_name(top)))
                        (old_ctx, new_var) -> $runlens(old_ctx, cons(new, old_ctx.$var), Lens{$top, fields, var})
                    end)
                end
            end
        a :: Symbol => a
    end
end


macro parser(node)
    parser_gen(collect_context(node)) |> esc
end