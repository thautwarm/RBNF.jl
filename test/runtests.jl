using RBNF
using MLStyle
rmlines = @λ begin
           e :: Expr           -> Expr(e.head, filter(x -> x !== nothing, map(rmlines, e.args))...)
             :: LineNumberNode -> nothing
           a                   -> a
       end


node = quote
RBNF.@parser TestLang begin
    @token
    a := r"\d"
    @grammar
    b := [(k = Option(k)), (seqa :: Vector = Many(a => parse_int))]
end
end

@info :so rmlines(macroexpand(Main, node))

struct TestLang end
parse_int(x :: RBNF.Token) = parse(Int, x.str)

RBNF.@parser TestLang begin
    @token
    a := r"\G\d"
    k := "kkk"
    @grammar
    b := [(k = Option(k)), (seqa :: Vector = Many(a => parse_int))]
end

res1, _ = RBNF.runparser(b, RBNF.runlexer(TestLang, "123"))
println(res1)

res2, _ = RBNF.runparser(b, RBNF.runlexer(TestLang, "kkk123"))
println(res2)


