# Restructured BNF

The Restructured BNF(RBNF) aims at the generating parsers without requiring redundant coding from programmers.

RBNF is designed for

- Maintainability: unlike Regex, RBNF's good readability makes more sense in the syntax level.
- Conciseness: avoid self-repeating you did with other parser generators.
- Efficiency: RBNF just specifies the semantics, we could use customizable back ends/parsing algorithms here.
- Extensibility: mix Julia meta-programming with the notations to define parsers/lexers.

Taking advantage of a BNF source block, **lexers** and **parsers** are generated as well as
some data type definitions representing tokenizers and ASTs.

Some modern facilities of parsing are introduced.

The notations of dedicated escaping lexers makes it super convenient to implement lexers for nested comments, string literals and so on.

```julia
str = @quote ("\"" ,"\\\"", "\"")
```

The notations of grammar macros makes it super easy to achieve code reuse for RBNF.

```julia
join(separator, rule) = :[$rule, ($separator, $rule){*}]
# `join(',', 'a')` generates a parser to parse something like "a" or "a,a,a"
```

# About Implementation and Efficiency

The rudimentary implementation(back end) is the Parser Combinator,
thus the direct left recursions are not supported implicitly,
plus the indirect left recursions are not supported.

Note that currently the lack of further analyses and optimizations may lead
to some problems in expressiveness and performance, however it's not that severe
in many use cases not concerned to real time applications.

We're to figure out a solid way to compile the parser definitions to bottom-up parsers
(thus left recursions wouldn't be a problem) with the capability of processing context sensitive cases.

You can check following projects to see
what I've been achieved and, what I'm now researching.

- https://github.com/thautwarm/RBNF
- https://github.com/thautwarm/rbnfrbnf
- https://github.com/thautwarm/RBNF.hs

# Basic Usage

P.S: rules can be mutually referenced by each other.

## Structures of Defining A Language

Firstly we need an immutable object to denote your language.

```julia
using RBNF

struct YourLang end
RBNF.@parser YourLang begin

    ignore{#= tokenizer names to be ignored =#}
    # e.g.,  `ignore{mystring, mychar}`

    reserved = [#= reserved identifiers =#]
    # strings, symbols and even bool literals are allowed here,
    # and their strings will be regarded as reserved keywords.
    # e.g., reserved = [true, "if", :else]

    @grammar
    # define grammar rules

    # following ':=' statement defines a grammar node.
    # note, a structure named "Struct_node" will be defined as well.
    node := #= a combination of rules =#

    # following '=' statement defines an alias for a combination of rules.
    alias = #= a combination of rules =#

    @token
    # define tokenizers as well as their corresponding lexers

    mystring  := "abc"
    mychar    := 'k'
    myregex   := r"\G\s+"
    myquote   := @quote ("begin_string" ,"\\end_string", "end_string")
end

tokens = RBNF.runlexer(YourLang, source_code)

ast, ctx = RBNF.runparser(parser_defined_in_grammar_section, tokens)
```

## Tokenizer

Unlike many other parser generators(yes, I'm talking to you, **** of Rust),
RBNF provides rich information with tokenizers.

```julia
struct Token{T}
    lineno :: Int64
    colno  :: Int32
    offset :: Int64
    str    :: String
    span   :: Int32
end
```

The type parameter `T` is used to denote which class the tokenizer belongs to.

For instance, if some tokenizer denotes the reserved keywords, their type will
be `Token{:reserved}`.

If a tokenizer is handled with such a lexer:

```julia
@token
identifier = r"\G[A-Za-z_]{1}[A-Za-z0-9_]*"
```
It'll then be of type `Token{:identifier}`.


## Sequence

```julia
@grammar
c = ['a', 'b']
```

`c` parses `[Token(str="a", ...), Token(str="b", ...)]`,
and outputs a list `[Token(str="a", ...), Token(str="b", ...)]`.

## Fields

```julia
@grammar
c := [a='a', b='b']
```

`c` parses `[Token(str="a", ...), Token(str="b", ...)]`,
and outputs `Struct_c(a=Token(str="a", ...), b=Token(str="b", ...))`.

## Custom Data Types

Firstly you just define your own data type in the global scope of current module.

```julia

struct C
    a :: Token
    b :: Token
end

...
@grammar
c :: C := [a='a', b='b']
...
```

`c` parses `[Token(str="a", ...), Token(str="b", ...)]`,
and outputs `C(a=Token(str="a", ...), b=Token(str="b", ...))`.

## Tuple


```julia
@grammar
c = ('a', 'b')
```

`c` parses `[Token(str="a", ...), Token(str="b", ...)]`,
and outputs a tuple `(Token(str="a", ...), Token(str="b", ...))`.

## Not

Currently **Not** parser is only allowed on literals.

```julia
@grammar
A = !'a'
```

`A` can parse `[Token(str=not_a)]` for all `not_a != "a"`.

## Optional

```julia
...
@grammar
a    = "a"
b    = "b"
c    = [a.?, b]
...
```

`c` can parse tokenizers `[Token(str="a", ...), Token(str="b", ...)]` or
`[Token(str="a", ...)]`,
and outputs `[Token(str="a", ...), Token(str="b", ...)]` or
`[Some(nothing), Token(str="b", ...)]`, respectively.

## Repeat

```julia
@grammar
a = b{*}
```

`a` can parse one or more `b`.

## Or(Alternative)

```julia
@grammar
a = b | c
```

`a` can parse `b` or `c`.


## Keyword

```julia
@grammar
If  := [hd=:if, cond=Exp, :then,
                br1=Exp,
            :else,
        br2=Exp]
```

Note that `:if`, `:then` and `:else` should parse
`Token{:reserved}(str=...)`. **Their type should be
`Token{:reserved}`!**

However you always don't have to aware of this.
Further, you just should aware that any other lexer(hereafter `L`)
matching `"if", "then", "else"` won't produce a tokenizer typed `Token{L}`,
but that typed `Token{:reserved}`.


## Rewrite

```julia
@grammar
ab_no_a = ['a', 'b'] % (x -> x[1])
```

`ab_no_a` produces `Token(str="b")` if it gets parsed successfully.

## Paramterised Polymorphisms

Firstly define such a function in the global scope:

```julia
join(separator, rule) = :[$rule, ($separator, $rule){*}]
```

Then, inside `RBNF.@parser Lang begin ... end`, we write down

```julia
@grammar
list = ['[', [join(',', expression)].?, ']']
```

`list` can parse a list of expressions separated by ',',
but you should define the `expression` yourself.

## @direct_recur

TODO. you can check the "tests" directory for the examples.
