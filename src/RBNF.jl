module RBNF
using MLStyle
using DataStructures
using PrettyPrint

include("Token.jl")
include("Lexer.jl")
include("Lens.jl")
include("NaiveParserc.jl")
include("ParserGen.jl")

PrettyPrint.pp_impl(io, tk::Token{T}, indent) where T = begin
    print(io, "Token{$T}(str=$(tk.str), lineno=$(tk.lineno), colno=$(tk.lineno))")
end
end # module
