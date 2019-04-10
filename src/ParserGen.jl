AST = Any

struct PContext
    lexer_tables :: Vector{Tuple{Symbol, Function}}
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
                    for ignore in ignores
                        collect_lexer!(unnamed_lexers, :unnamed, ignore)
                    end
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
    lexer_tables = [(k, mklexer(v)) for (k, v) in lexers]
    PContext(lexer_tables, ignores, tokens, grammars)
end

