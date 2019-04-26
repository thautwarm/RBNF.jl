AST = Any

function runlexer
end

function runparser
end

struct PContext
    reserved_words :: Set{String}
    lexer_table :: Vector{Tuple{Symbol, Expr}}
    ignores :: Vector{AST}
    tokens :: Vector{Tuple{Symbol, AST}}
    grammars :: Vector{Tuple{Symbol, Any, AST, Bool}}
end

struct AliasContext
end

to_struct_name(name) = Symbol("Struct__", name)

const r_str_v = Symbol("@r_str")
function collect_lexer!(lexers, name, node)
    @match node begin
        Expr(:macrocall, &r_str_v, ::LineNumberNode, s) => push!(lexers, (name, LexerSpec(Regex(s))))
        c::Union{Char, String, Regex} => push!(lexers, (name, LexerSpec(c)))
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
    reserved_words :: Set{String}                     = Set(String[])
    ignores  :: Vector{AST}                           = []
    tokens   :: Vector{Tuple{Symbol, AST}}            = []
    grammars :: Vector{Tuple{Symbol, Any, AST, Bool}} = []
    collector = nothing
    unnamed_lexers = OrderedSet{Tuple{Symbol, LexerSpec{T}} where T}()

    for stmt in stmts
        @match stmt begin
            ::LineNumberNode => ()
            IsMacro{:token} =>
                begin
                    collector = @λ begin
                        :($name := $node) ->  push!(tokens, (name, node))
                        a -> throw(a)
                    end
                end
            IsMacro{:grammar} =>
                begin
                    collector = x -> @match x begin
                        :($name = $node) =>
                            begin
                                collect_lexer!(unnamed_lexers, :unnamed, node)
                                named = (name=name, typ=nothing, def=node, is_grammar_node=true)
                                push!(grammars,  Tuple(named))
                            end
                        :($name :: $t := $node) ||
                        :($name := $node) && Do(t=nothing) =>
                            begin
                                collect_lexer!(unnamed_lexers, :unnamed, node)
                                named = (name=name, typ=t, def=node, is_grammar_node=false)
                                push!(grammars, Tuple(named))
                            end
                        a => throw(a)
                    end
                end
            :(ignore{$(ignores_...)}) =>
                begin
                    append!(ignores, ignores_)
                end
            :(reserved = [$(reserved_words_to_append...)]) =>
                begin
                    union!(reserved_words, map(string, reserved_words))
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
    lexer_table :: Vector{Tuple{Symbol, Expr}} = [(k, mklexer(v)) for (k, v) in lexers]
    PContext(reserved_words, lexer_table, ignores, tokens, grammars)
end

function parser_gen(lang, pc :: PContext)
    decl_seqs = []
    for (each, _) in  pc.tokens
        push!(decl_seqs, quote
            $each = let
                f(::$Token{$(QuoteNode(each))}) = true
                f(_) = false
                $tokenparser(f)
            end
        end)
    end

    for (each, _, _, _) in pc.grammars
        push!(decl_seqs, quote
            function $each
            end
        end)
    end
    struct_defs = []
    parser_defs = []
    for (k, t, v, is_grammar_node) in pc.grammars
        (struct_def, parser_def) = grammar_gen(k, t, v, is_grammar_node)
        push!(struct_defs, struct_def)
        push!(parser_defs, parser_def)
    end

    append!(decl_seqs, struct_defs)
    append!(decl_seqs, parser_defs)

    # make lexer
    lexer_table = pc.lexer_table
    reserved_words = pc.reserved_words
    runlexer_no_ignored_sym = gensym("lexer")
    push!(decl_seqs, :($runlexer_no_ignored_sym = $(genlex(lexer_table, reserved_words))))
    names = :($Set([$(map(QuoteNode, pc.ignores)...)]))
    push!(decl_seqs, :(
        $RBNF.runlexer(::$Type{$lang}, str) =
            $filter(
                function (:: $Token{k}, ) where k
                    !(k in $names)
                end,
                $runlexer_no_ignored_sym(str))
            )
    )
    for (each, struct_name, _, is_grammar_node) in pc.grammars
        # in fact, !is_grammar_node is redundant, but for readability, emm..
        if !is_grammar_node && struct_name === nothing
            struct_name = to_struct_name(each)
        end
        push!(decl_seqs, quote
            $RBNF.runparser(::$typeof($each), tokens :: $Vector{$Token}) =
                let ctx = $CtxTokens{$struct_name}($TokenView(tokens))
                    $each(ctx)
                end
        end)
    end
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

function grammar_gen(name::Symbol, struct_name, def_body, is_grammar_node)
    vars = OrderedDict{Symbol, Any}()
    analyse_closure!(vars, def_body)
    if struct_name === nothing
        struct_name = to_struct_name(name)
        struct_def = quote
            struct $struct_name
                $([Expr(:(::), k, v) for (k, v) in vars]...)
            end
            $RBNF.crate(::Type{$struct_name}) = $struct_name($([:($crate($v)) for v in values(vars)]...))
        end
    end
    parser_skeleton_name = gensym()
    return_expr = is_grammar_node ? :result : :(returned_ctx.state)
    parser_def = quote
        $parser_skeleton_name = $(make(def_body, struct_name))
        function $name(ctx_in)
            cur_state = $crate($struct_name)
            old_state = ctx_in.state
            inside_ctx = $update_state(ctx_in, cur_state)
            (result, returned_ctx) = $parser_skeleton_name(inside_ctx)
            resume_ctx = $update_state(returned_ctx, old_state)
            (result === nothing ? nothing : $return_expr,  resume_ctx)
        end
    end
    (struct_def, parser_def)
end


function make(node, top)
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
                :($hlistparser([$(elt_ps...)]))
            end
        :($(elts...), ) =>
            let elt_ps = map(makerec, elts)
                :($htupleparser([$(elt_ps...)]))
            end
        :($a | $b) =>
            let pa = makerec(a), pb = makerec(b)
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

        :($a.?) =>
            let pa = makerec(a)
                :($optparser($pa))
            end

        :($var :: $_ = $a) || :($var = $a) =>
            let pa = makerec(a)
                quote
                $updateparser($pa,
                    let top_struct = $top, fields = $fieldnames(top_struct)
                        (old_ctx, new_var) -> $runlens(old_ctx, new_var, $Lens{top_struct, fields, $(QuoteNode(var))})
                    end)
                end
            end
        :($var :: $_ << $a) || :($var << $a) =>
            let pa = makerec(a)
                quote
                $updateparser($pa,
                    let top_struct = $top, fields = $fieldnames(top_struct)
                        (old_ctx, new_var) -> $runlens(old_ctx, cons(new, old_ctx.$var), $Lens{top_struct, fields, $(QuoteNode(var))})
                    end)
                end
            end
        a :: Symbol => a

        MacroSplit{:direct_recur}(quote $(args...) end) =>
            begin
                init = nothing
                prefix = nothing
                for each in args
                    @match each begin
                        :(init = $init_) => (init = init_)
                        :(prefix = $prefix_) => (prefix = prefix_)
                        _ => ()
                    end
                end
                reducer, trailer = @match prefix begin
                    :[recur, $(trailer...)] => (:((prev, now) -> [prev, now...]), trailer)
                    :(recur, $(trailer...), ) => (:((prev, now) -> (prev, now...)), trailer)
                    _ => throw("invalid syntax for direct_recur")
                end
                if isempty(trailer)
                    throw("malformed left recursion found: `a := a`")
                end
                let initp = makerec(init), trailerp = makerec(:[$(trailer...)])
                    :($direct_lr($initp, $trailerp, $reducer))
                end
            end
        z => throw(z)
    end
end

rmlines = @λ begin
           e :: Expr           -> Expr(e.head, filter(x -> x !== nothing, map(rmlines, e.args))...)
             :: LineNumberNode -> nothing
           a                   -> a
end


macro parser(lang, node)
    ex = parser_gen(lang, collect_context(node))
    esc(ex)
end