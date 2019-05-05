module RBNF
using MLStyle
using DataStructures
using PrettyPrint

include("Token.jl")
include("Lexer.jl")
include("Lens.jl")
include("NaiveParserc.jl")
include("ParserGen.jl")

PrettyPrint.pprint_impl(io, tk::Token{T}, indent, newline) where T = begin
    print(io, "Token{$T}(str=$(tk.str), lineno=$(tk.lineno), colno=$(tk.lineno))")
end
end # module
