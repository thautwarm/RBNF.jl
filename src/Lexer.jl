AsocList{K, V} = AbstractArray{Tuple{K, V}}

_genlex(argsym, retsym, lexer_table) =
    @match lexer_table begin
        [] => quote
                $throw((SubString($argsym, offset), "Token NotFound"))
              end
        [(k, v), tl...] =>
            quote
                s = $v($argsym, offset)
                if s !== nothing
                    line_inc = count(x -> x === '\n', s)
                    n = length(s)
                    push!($retsym, $Token{$(QuoteNode(k))}(lineno, colno, offset, s, n))
                    if  line_inc === 0
                        colno += n
                    else
                        lineno += line_inc
                        colno =  n - findlast(x -> x === '\n', s) + 1
                    end
                    offset += n
                    continue
                end
                $(_genlex(argsym, retsym, tl))
            end
    end

rmlines = @λ begin
    e :: Expr           -> Expr(e.head, filter(x -> x !== nothing, map(rmlines, e.args))...)
      :: LineNumberNode -> nothing
    a                   -> a
end

function genlex(lexer_table)
    lexer_table = [(k, v) for (k, v) in eval(lexer_table)]
    ex = quote
        function (tokens)
            lineno :: Int64 = 1
            colno  :: Int32 = 1
            offset :: Int64  = 1
            ret = Token[]
            N :: Int64 = length(tokens)
            while offset <= N
                $(_genlex(:tokens, :ret, lexer_table))
            end
            ret
        end
    end
    # println(rmlines(ex))
    ex
end

macro genlex(lexer_table)
    genlex(lexer_table) |> esc
end

struct LexerSpec{K}
    a :: K
end

function mklexer(a :: LexerSpec{Char})
    node = quote (chars, i) -> chars[i] === $(a.a) ? String([$(a.a)]) : nothing end
    (@__MODULE__).eval(node)
end

function mklexer(a :: LexerSpec{String})
    let str = a.a, n = length(str),
        node = quote (chars, i) ->
            startswith(SubString(chars, i), $str) ? $str : nothing
        end
        (@__MODULE__).eval(node)
    end
end

function mklexer(a :: LexerSpec{Regex})
    let regex = a.a,
        node = quote
            function (chars, i)
                v = match($regex, SubString(chars, i))
                v === nothing ? nothing : v.match
            end
        end
        (@__MODULE__).eval(node)
    end
end
