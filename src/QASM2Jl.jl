module QASM2Jl
using MLStyle
using DataStructures

include("Token.jl")
include("Lexer.jl")
include("ParserGen.jl")

greet() = print("Hello World!")

include("QASMAST.jl") |> println

end # module
