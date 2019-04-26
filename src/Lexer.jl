AsocList{K, V} = AbstractArray{Tuple{K, V}}

function _genlex(argsym, retsym, lexer_table, reserved_words)
    gen_many_lexers = map(lexer_table) do (k, v)
        lnode = LineNumberNode(1, "lexing rule: $k")

        token_type = reserved_words === nothing ? QuoteNode(k) :
                        :(s in $reserved_words ? :reserved : $(QuoteNode(k)))
        quote
            $lnode
            s = $v($argsym, offset)
            if s !== nothing
            line_inc = count(x -> x === '\n', s)
            n = length(s)
            push!($retsym, $Token{$token_type}(lineno, colno, offset, s, n))
            if  line_inc === 0
                colno += n
            else
                lineno += line_inc
                colno =  n - findlast(x -> x === '\n', s) + 1
            end
            offset += n
            continue
            end
        end
    end
    final = :($throw((SubString($argsym, offset), "Token NotFound")))
    Expr(:block, gen_many_lexers..., final)
end

rmlines = @λ begin
    e :: Expr           -> Expr(e.head, filter(x -> x !== nothing, map(rmlines, e.args))...)
      :: LineNumberNode -> nothing
    a                   -> a
end

struct LexerSpec{K}
    a :: K
end

function genlex(lexer_table, reserved_words)
    ex = quote
        function (tokens)
            lineno :: $Int64 = 1
            colno  :: $Int32 = 1
            offset :: $Int64  = 1
            ret = $Token[]
            N :: $Int64 = $length(tokens)
            while offset <= N
                $(_genlex(:tokens, :ret, lexer_table, reserved_words))
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



function mklexer(a :: LexerSpec{Char})
    quote (chars, i) -> chars[i] === $(a.a) ? String([$(a.a)]) : nothing end
end

function mklexer(a :: LexerSpec{String})
    let str = a.a, n = length(str)
        quote (chars, i) ->
            startswith(SubString(chars, i), $str) ? $str : nothing
        end
    end
end

function mklexer(a :: LexerSpec{Regex})
    let regex = a.a
        quote
            function (chars, i)
                v = match($regex, SubString(chars, i))
                v === nothing ? nothing : v.match
            end
        end
    end
end
