using RBNF
using MLStyle

# rmlines = @λ begin
#            e :: Expr           -> Expr(e.head, filter(x -> x !== nothing, map(rmlines, e.args))...)
#              :: LineNumberNode -> nothing
#            a                   -> a
# end


# node = quote
# RBNF.@parser TestLang begin
#     @token
#     a := r"\d"
#     @grammar
#     b := [(k = k.?), (seqa :: Vector = Many(a => parse_int))]
# end
# end

# @info :show rmlines(macroexpand(Main, node))

# struct TestLang end
# parse_int(x :: RBNF.Token) = parse(Int, x.str)

# RBNF.@parser TestLang begin
#     @token
#     a := r"\G\d"
#     k := "kkk"
#     @grammar
#     b := [(k = k.?), (seqa :: Vector = Many(a => parse_int))]
# end

# res1, _ = RBNF.runparser(b, RBNF.runlexer(TestLang, "123"))
# println(res1)

# res2, _ = RBNF.runparser(b, RBNF.runlexer(TestLang, "kkk123"))
# println(res2)


# struct TestLang2 end

# RBNF.@parser TestLang2 begin
#     @token
#     num := r"\G\d+"
#     space := r"\G\s+"
#     @grammar
#     manynumbers := @direct_recur begin
#         init = num
#         prefix = [recur, space, num]
#     end
# end

# res3, _ = RBNF.runparser(manynumbers, RBNF.runlexer(TestLang2, "52 123 123 14312 213"))
# println(res3)
include("qasm.jl")
include("reml.jl")