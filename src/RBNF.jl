module RBNF
using MLStyle
using DataStructures

include("Token.jl")
include("Lexer.jl")
include("Lens.jl")
include("NaiveParserc.jl")
include("ParserGen.jl")
include("PFormat.jl")

PFormat.pprint_impl(io, tk::Token{T}, indent, newline) where T = begin
    print(io, "Token{$T}(str=$(tk.lineno), lineno=$(tk.lineno), colno=$(tk.lineno))")
end
end # module
