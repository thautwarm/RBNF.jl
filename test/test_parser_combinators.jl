using Test
using RBNF

@testset "Parser Combinators" begin
    @testset "TokenView" begin
        tokens = RBNF.Token{:a}(1, Int32(1), 1, "a", Int32(1))
        token_vec = RBNF.Token[tokens]
        tv = RBNF.TokenView(token_vec)
        @test tv.current == 1
        @test tv.length == 1
        @test tv.source == token_vec
    end

    @testset "tokenparser" begin
        tokens = [
            RBNF.Token{:number}(1, Int32(1), 1, "42", Int32(2)),
            RBNF.Token{:identifier}(1, Int32(3), 3, "x", Int32(1)),
        ]
        token_vec = RBNF.Token[tokens...]

        ctx = RBNF.CtxTokens{Nothing}(RBNF.TokenView(token_vec))

        # Test matching number
        pred_number(x::RBNF.Token{:number}) = true
        pred_number(_) = false
        parser_number = RBNF.tokenparser(pred_number)

        result, new_ctx = parser_number(ctx)
        @test result !== nothing
        @test result.str == "42"
        @test new_ctx.tokens.current == 2

        # Test matching identifier
        pred_id(x::RBNF.Token{:identifier}) = true
        pred_id(_) = false
        parser_id = RBNF.tokenparser(pred_id)

        result, new_ctx = parser_number(ctx)
        result2, new_ctx2 = parser_id(new_ctx)
        @test result2 !== nothing
        @test result2.str == "x"

        # Test non-matching
        pred_string(x::RBNF.Token{:string}) = true
        pred_string(_) = false
        parser_string = RBNF.tokenparser(pred_string)

        result3, ctx3 = parser_string(ctx)
        @test result3 === nothing
    end

    @testset "orparser" begin
        tokens = [RBNF.Token{:number}(1, Int32(1), 1, "42", Int32(2))]
        token_vec = RBNF.Token[tokens...]
        ctx = RBNF.CtxTokens{Nothing}(RBNF.TokenView(token_vec))

        pred_num(x::RBNF.Token{:number}) = true
        pred_num(_) = false
        parser_num = RBNF.tokenparser(pred_num)

        pred_str(x::RBNF.Token{:string}) = true
        pred_str(_) = false
        parser_str = RBNF.tokenparser(pred_str)

        parser_or = RBNF.orparser(parser_str, parser_num)

        result, new_ctx = parser_or(ctx)
        @test result !== nothing
        @test result.str == "42"
    end

    @testset "tupleparser" begin
        tokens = [
            RBNF.Token{:a}(1, Int32(1), 1, "a", Int32(1)),
            RBNF.Token{:b}(1, Int32(2), 2, "b", Int32(1)),
        ]
        token_vec = RBNF.Token[tokens...]
        ctx = RBNF.CtxTokens{Nothing}(RBNF.TokenView(token_vec))

        pred_a(x::RBNF.Token{:a}) = true
        pred_a(_) = false
        parser_a = RBNF.tokenparser(pred_a)

        pred_b(x::RBNF.Token{:b}) = true
        pred_b(_) = false
        parser_b = RBNF.tokenparser(pred_b)

        parser_tuple = RBNF.tupleparser(parser_a, parser_b)

        result, new_ctx = parser_tuple(ctx)
        @test result !== nothing
        @test result isa Tuple
        @test result[1].str == "a"
        @test result[2].str == "b"
    end

    @testset "hlistparser" begin
        tokens = [
            RBNF.Token{:a}(1, Int32(1), 1, "a", Int32(1)),
            RBNF.Token{:b}(1, Int32(2), 2, "b", Int32(1)),
            RBNF.Token{:c}(1, Int32(3), 3, "c", Int32(1)),
        ]
        token_vec = RBNF.Token[tokens...]
        ctx = RBNF.CtxTokens{Nothing}(RBNF.TokenView(token_vec))

        pred_a(x::RBNF.Token{:a}) = true
        pred_a(_) = false
        parser_a = RBNF.tokenparser(pred_a)

        pred_b(x::RBNF.Token{:b}) = true
        pred_b(_) = false
        parser_b = RBNF.tokenparser(pred_b)

        pred_c(x::RBNF.Token{:c}) = true
        pred_c(_) = false
        parser_c = RBNF.tokenparser(pred_c)

        parser_list = RBNF.hlistparser([parser_a, parser_b, parser_c])

        result, new_ctx = parser_list(ctx)
        @test result !== nothing
        @test result isa Vector
        @test length(result) == 3
        @test result[1].str == "a"
        @test result[2].str == "b"
        @test result[3].str == "c"
    end

    @testset "manyparser" begin
        # Multiple tokens
        tokens = [
            RBNF.Token{:number}(1, Int32(1), 1, "1", Int32(1)),
            RBNF.Token{:number}(1, Int32(2), 2, "2", Int32(1)),
            RBNF.Token{:number}(1, Int32(3), 3, "3", Int32(1)),
        ]
        token_vec = RBNF.Token[tokens...]
        ctx = RBNF.CtxTokens{Nothing}(RBNF.TokenView(token_vec))

        pred_num(x::RBNF.Token{:number}) = true
        pred_num(_) = false
        parser_num = RBNF.tokenparser(pred_num)

        parser_many = RBNF.manyparser(parser_num)

        result, new_ctx = parser_many(ctx)
        @test result !== nothing
        @test length(result) == 3
        @test result[1].str == "1"
        @test result[2].str == "2"
        @test result[3].str == "3"
    end

    @testset "optparser" begin
        tokens = [RBNF.Token{:number}(1, Int32(1), 1, "42", Int32(2))]
        token_vec = RBNF.Token[tokens...]
        ctx = RBNF.CtxTokens{Nothing}(RBNF.TokenView(token_vec))

        pred_num(x::RBNF.Token{:number}) = true
        pred_num(_) = false
        parser_num = RBNF.tokenparser(pred_num)

        parser_opt = RBNF.optparser(parser_num)

        result, new_ctx = parser_opt(ctx)
        @test result !== nothing
        @test result isa Some
        @test result.value !== nothing
        @test result.value.str == "42"
    end

    @testset "trans" begin
        tokens = [RBNF.Token{:number}(1, Int32(1), 1, "42", Int32(2))]
        token_vec = RBNF.Token[tokens...]
        ctx = RBNF.CtxTokens{Nothing}(RBNF.TokenView(token_vec))

        pred_num(x::RBNF.Token{:number}) = true
        pred_num(_) = false
        parser_num = RBNF.tokenparser(pred_num)

        to_int(x) = parse(Int, x.str)
        parser_trans = RBNF.trans(to_int, parser_num)

        result, new_ctx = parser_trans(ctx)
        @test result !== nothing
        @test result == 42
        @test result isa Int
    end

    @testset "crate" begin
        @testset "Vector creation" begin
            v = RBNF.crate(Vector{Int})
            @test v isa Vector{Int}
            @test isempty(v)
        end

        @testset "Number creation" begin
            @test RBNF.crate(Int) === 0
            @test RBNF.crate(Float64) === 0.0
        end

        @testset "String creation" begin
            s = RBNF.crate(String)
            @test s == ""
        end

        @testset "Bool creation" begin
            @test RBNF.crate(Bool) === false
        end

        @testset "Nothing creation" begin
            @test RBNF.crate(Nothing) === nothing
        end
    end
end
