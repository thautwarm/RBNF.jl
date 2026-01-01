using Test
using RBNF

@testset "Lens" begin
    @testset "Basic struct field update" begin
        struct TestStruct1
            a::Int
            b::String
        end

        obj = TestStruct1(1, "hello")
        new_obj = RBNF.runlens(obj, 42, RBNF.Lens{TestStruct1, fieldnames(TestStruct1), :a})

        @test new_obj.a == 42
        @test new_obj.b == "hello"
    end

    @testset "Update second field" begin
        struct TestStruct2
            x::Int
            y::Float64
            z::String
        end

        obj = TestStruct2(1, 2.0, "test")
        new_obj = RBNF.runlens(obj, 3.14, RBNF.Lens{TestStruct2, fieldnames(TestStruct2), :y})

        @test new_obj.x == 1
        @test new_obj.y == 3.14
        @test new_obj.z == "test"
    end

    @testset "Update last field" begin
        struct TestStruct3
            a::Int
            b::Int
            c::String
        end

        obj = TestStruct3(1, 2, "old")
        new_obj = RBNF.runlens(obj, "new", RBNF.Lens{TestStruct3, fieldnames(TestStruct3), :c})

        @test new_obj.a == 1
        @test new_obj.b == 2
        @test new_obj.c == "new"
    end

    @testset "Preserve immutability" begin
        struct TestStruct4
            value::Int
        end

        obj = TestStruct4(10)
        new_obj = RBNF.runlens(obj, 20, RBNF.Lens{TestStruct4, fieldnames(TestStruct4), :value})

        @test obj.value == 10  # Original object unchanged
        @test new_obj.value == 20
        @test obj !== new_obj
    end

    @testset "Update with different types" begin
        struct TestStruct5
            num::Int
            text::String
            flag::Bool
        end

        obj = TestStruct5(1, "hello", true)

        # Update num
        obj1 = RBNF.runlens(obj, 99, RBNF.Lens{TestStruct5, fieldnames(TestStruct5), :num})
        @test obj1.num == 99
        @test obj1.text == "hello"
        @test obj1.flag === true

        # Update text
        obj2 = RBNF.runlens(obj1, "world", RBNF.Lens{TestStruct5, fieldnames(TestStruct5), :text})
        @test obj2.num == 99
        @test obj2.text == "world"
        @test obj2.flag === true

        # Update flag
        obj3 = RBNF.runlens(obj2, false, RBNF.Lens{TestStruct5, fieldnames(TestStruct5), :flag})
        @test obj3.num == 99
        @test obj3.text == "world"
        @test obj3.flag === false
    end
end
