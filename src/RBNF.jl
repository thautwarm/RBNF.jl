module RBNF
using MLStyle
using DataStructures
using PrettyPrint

include("Token.jl")
include("Lexer.jl")
include("Lens.jl")
include("NaiveParserc.jl")
include("ParserGen.jl")

PrettyPrint.pp_impl(io, tk::Token{T}, indent::Int) where T = begin
    s = "Token{$T}(str=$(tk.str), lineno=$(tk.lineno), colno=$(tk.lineno))"
    print(io, s)
    return length(s) + indent
end
end # module
