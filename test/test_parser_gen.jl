using Test
using RBNF

# Test 1: Basic token definition
module BasicTokenTest
using Test
using RBNF
struct BasicTokenLangTest1 end
RBNF.@parser BasicTokenLangTest1 begin
    ignore{space}
    @token
    space := r"\G\s+"
    number := r"\G\d+"
    identifier := r"\G[a-zA-Z_][a-zA-Z0-9_]*"
end

@testset "Basic Token Definition" begin
    tokens = RBNF.runlexer(BasicTokenLangTest1, "123 abc_456")
    @test length(tokens) == 2
    @test tokens[1].str == "123"
    @test tokens[1] isa RBNF.Token{:number}
    @test tokens[2].str == "abc_456"
    @test tokens[2] isa RBNF.Token{:identifier}
end
end

# Test 2: String and char tokens
module StringCharTest
using Test
using RBNF
struct StringCharLangTest end
RBNF.@parser StringCharLangTest begin
    @token
    left_paren := "("
    right_paren := ")"
    plus := '+'
end

@testset "String and Char Tokens" begin
    tokens = RBNF.runlexer(StringCharLangTest, "()+")
    @test length(tokens) == 3
    @test tokens[1].str == "("
    @test tokens[2].str == ")"
    @test tokens[3].str == "+"
end
end

# Test 3: Ignore tokens
module IgnoreTokenTest
using Test
using RBNF
struct IgnoreLangTest end
RBNF.@parser IgnoreLangTest begin
    ignore{space}
    @token
    space := r"\G\s+"
    number := r"\G\d+"
end

@testset "Ignore Tokens" begin
    tokens = RBNF.runlexer(IgnoreLangTest, "1 2   3")
    @test length(tokens) == 3
    @test all(t -> t isa RBNF.Token{:number}, tokens)
    @test tokens[1].str == "1"
    @test tokens[2].str == "2"
    @test tokens[3].str == "3"
end
end

# Test 4: Reserved keywords
module ReservedKeywordTest
using Test
using RBNF
struct ReservedLangTest end
RBNF.@parser ReservedLangTest begin
    ignore{space}
    reserved = ["if", "then", "else"]
    @token
    space := r"\G\s+"
    identifier := r"\G[a-zA-Z_][a-zA-Z0-9_]*"
end

@testset "Reserved Keywords" begin
    tokens = RBNF.runlexer(ReservedLangTest, "if then else iffy")
    @test length(tokens) == 4
    @test tokens[1] isa RBNF.Token{:reserved}
    @test tokens[1].str == "if"
    @test tokens[2] isa RBNF.Token{:reserved}
    @test tokens[2].str == "then"
    @test tokens[3] isa RBNF.Token{:reserved}
    @test tokens[3].str == "else"
    @test tokens[4] isa RBNF.Token{:identifier}
    @test tokens[4].str == "iffy"
end
end

# Test 5: Quoted string with escape
module QuotedStringTest
using Test
using RBNF
struct QuotedStringLangTest end
RBNF.@parser QuotedStringLangTest begin
    ignore{space}
    @token
    space := r"\G\s+"
    str := @quote ("\"", "\\\"", "\"")
end

@testset "Quoted String with Escape" begin
    tokens = RBNF.runlexer(QuotedStringLangTest, "\"hello\" \"world\\\"escaped\"")
    @test length(tokens) == 2
    @test tokens[1].str == "\"hello\""
    @test tokens[2].str == "\"world\\\"escaped\""
end
end

# Test 6: Simple grammar rule
module SimpleGrammarTest
using Test
using RBNF
struct SimpleGrammarLangTest end
RBNF.@parser SimpleGrammarLangTest begin
    @token
    a := 'a'
    b := 'b'
    @grammar
    ab = [a, b]
end

@testset "Simple Grammar Rule" begin
    tokens = RBNF.runlexer(SimpleGrammarLangTest, "ab")
    result, ctx = RBNF.runparser(ab, tokens)
    @test result !== nothing
    @test result isa Vector
    @test length(result) == 2
    @test result[1].str == "a"
    @test result[2].str == "b"
end
end

# Test 7: Grammar with fields
module FieldGrammarTest
using Test
using RBNF
struct FieldGrammarLangTest end
RBNF.@parser FieldGrammarLangTest begin
    @token
    number := r"\G\d+"
    plus := '+'
    @grammar
    addition := [left=number, op=plus, right=number]
end

@testset "Grammar with Fields" begin
    tokens = RBNF.runlexer(FieldGrammarLangTest, "1+2")
    result, ctx = RBNF.runparser(addition, tokens)
    @test result !== nothing
    @test result.left.str == "1"
    @test result.op.str == "+"
    @test result.right.str == "2"
end
end

# Test 8: Optional parser
module OptionalParserTest
using Test
using RBNF
struct OptionalLangTest end
RBNF.@parser OptionalLangTest begin
    @token
    a := 'a'
    b := 'b'
    @grammar
    opt_a = [a.?, b]
end

@testset "Optional Parser" begin
    # With 'a'
    tokens1 = RBNF.runlexer(OptionalLangTest, "ab")
    result1, _ = RBNF.runparser(opt_a, tokens1)
    @test result1 !== nothing
    @test result1[1] isa Some
    @test result1[1].value !== nothing
    @test result1[1].value.str == "a"
    @test result1[2].str == "b"

    # Without 'a'
    tokens2 = RBNF.runlexer(OptionalLangTest, "b")
    result2, _ = RBNF.runparser(opt_a, tokens2)
    @test result2 !== nothing
    @test result2[1] isa Some
    @test result2[1].value === nothing
    @test result2[2].str == "b"
end
end

# Test 9: Repeat parser
module RepeatParserTest
using Test
using RBNF
struct RepeatLangTest end
RBNF.@parser RepeatLangTest begin
    @token
    a := 'a'
    @grammar
    many_as = a{*}
end

@testset "Repeat Parser" begin
    tokens = RBNF.runlexer(RepeatLangTest, "aaa")
    result, _ = RBNF.runparser(many_as, tokens)
    @test result !== nothing
    @test length(result) == 3
    @test all(t -> t.str == "a", result)
end
end

# Test 10: Or parser
module OrParserTest
using Test
using RBNF
struct OrLangTest end
RBNF.@parser OrLangTest begin
    @token
    a := 'a'
    b := 'b'
    @grammar
    a_or_b = a | b
end

@testset "Or Parser" begin
    # Test 'a'
    tokens1 = RBNF.runlexer(OrLangTest, "a")
    result1, _ = RBNF.runparser(a_or_b, tokens1)
    @test result1 !== nothing
    @test result1.str == "a"

    # Test 'b'
    tokens2 = RBNF.runlexer(OrLangTest, "b")
    result2, _ = RBNF.runparser(a_or_b, tokens2)
    @test result2 !== nothing
    @test result2.str == "b"
end
end

# Test 11: Transform with %
module TransformParserTest
using Test
using RBNF
struct TransformLangTest end
to_int_test(x) = parse(Int, x.str)
RBNF.@parser TransformLangTest begin
    @token
    number := r"\G\d+"
    @grammar
    num = number % to_int_test
end

@testset "Transform with %" begin
    tokens = RBNF.runlexer(TransformLangTest, "42")
    result, _ = RBNF.runparser(num, tokens)
    @test result !== nothing
    @test result == 42
    @test result isa Int
end
end

# Test 12: Line number tracking
module LineTrackingTest
using Test
using RBNF
struct LineTrackingLangTest end
RBNF.@parser LineTrackingLangTest begin
    @token
    word := r"\G[a-zA-Z]+"
    newline := r"\G\n"
end

@testset "Line Number Tracking" begin
    input = "hello\nworld"
    tokens = RBNF.runlexer(LineTrackingLangTest, input)
    @test length(tokens) >= 2
    word_tokens = filter(t -> t isa RBNF.Token{:word}, tokens)
    @test length(word_tokens) == 2
    @test word_tokens[1].lineno == 1
    @test word_tokens[2].lineno == 2
end
end
