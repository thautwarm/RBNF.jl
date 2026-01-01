using Test
using RBNF: Token
using RBNF

@testset "Token" begin
    @testset "Token construction with keyword arguments" begin
        tk = Token{:test}("hello", lineno=1, colno=5, offset=10, span=5)
        @test tk.str == "hello"
        @test tk.lineno == 1
        @test tk.colno == 5
        @test tk.offset == 10
        @test tk.span == 5
        @test isa(tk, Token{:test})
    end

    @testset "Token construction with positional arguments" begin
        tk = Token{:identifier}(1, Int32(1), 0, "x", Int32(1))
        @test tk.str == "x"
        @test tk.lineno == 1
        @test tk.colno == 1
        @test tk.offset == 0
        @test tk.span == 1
        @test isa(tk, Token{:identifier})
    end

    @testset "Token with default values" begin
        tk = Token{:number}("42")
        @test tk.str == "42"
        @test tk.lineno == 0
        @test tk.colno == 0
        @test tk.offset == 0
        @test tk.span == 0
    end

    @testset "Token type parameter" begin
        tk1 = Token{:keyword}("if")
        tk2 = Token{:string}("hello")
        tk3 = Token{:number}("123")

        @test isa(tk1, Token{:keyword})
        @test isa(tk2, Token{:string})
        @test isa(tk3, Token{:number})

        @test !isa(tk1, Token{:string})
        @test !isa(tk2, Token{:number})
        @test !isa(tk3, Token{:keyword})
    end

    @testset "Token with special characters" begin
        tk1 = Token{:string}("\n\t", lineno=2, colno=3, offset=10, span=2)
        @test tk1.str == "\n\t"
        @test length(tk1.str) == 2

        tk2 = Token{:unicode}("你好", lineno=1, colno=1, offset=0, span=6)
        @test tk2.str == "你好"
        @test tk2.span == 6
    end

    @testset "Token immutability" begin
        tk = Token{:test}("hello", lineno=1, colno=1, offset=0, span=5)
        @test_throws ErrorException tk.str = "world"
    end
end
