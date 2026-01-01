using Test
using RBNF

println("Running RBNF test suite...")

# Run all test files
@testset "RBNF Tests" begin
    include("test_token.jl")
    include("test_lens.jl")
    include("test_parser_combinators.jl")
    include("test_parser_gen.jl")
end

# Keep example tests as demonstrations
println("\n=== Example: QASM Parser ===")
include("qasm.jl")

println("\n=== Example: ReML Parser ===")
include("reml.jl")
