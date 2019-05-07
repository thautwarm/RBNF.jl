using MLStyle



struct TokenView
    current :: Int
    length  :: Int
    source  :: Vector{Token}
end

TokenView(src :: Vector{Token}) = TokenView(1, length(src), src)

struct CtxTokens{State}
    tokens :: TokenView
    maxfetched :: Int
    state :: State
end

CtxTokens{A}(tokens :: TokenView) where A = CtxTokens(tokens, 1, crate(A))

function inherit_effect(ctx0 :: CtxTokens{A}, ctx1 :: CtxTokens{B}) where {A, B}
    CtxTokens(ctx0.tokens, max(ctx1.maxfetched, ctx0.maxfetched), ctx0.state)
end

orparser(pa, pb) = function (ctx_tokens)
    @match pa(ctx_tokens) begin
        (nothing, ctx_tokens) => pb(ctx_tokens)
        _ && a =>  a
    end
end

tupleparser(pa, pb) = function (ctx_tokens)
    @match pa(ctx_tokens) begin
        (nothing, _) && a => a
            (a, remained) =>
            @match pb(remained) begin
                (nothing, _) && a => a
                (b, remained) => ((a, b), remained)
            end
    end
end

manyparser(p) = function (ctx_tokens)
    res = []
    remained = ctx_tokens
    while true
        (elt, remained) = p(remained)
        if elt === nothing
            break
        end
        push!(res, elt)
    end

    (res, remained)
end

hlistparser(ps) = function (ctx_tokens)
    hlist = Vector{Union{T, Nothing} where T}(nothing, length(ps))
    done = false
    remained = ctx_tokens
    for (i, p) in enumerate(ps)
        @match p(remained) begin
            (nothing, a) =>
                begin
                    hlist = nothing
                    remained = a
                    done = true
                end
            (elt, a) =>
                begin
                    hlist[i] = elt
                    remained = a
                end
        end
        if done
            break
        end
    end
    (hlist, remained)
end

# what is a htuple? ... In fact a tuple with multielements is called a hlist,
# but in dynamic lang things get confusing..
htupleparser(ps) = function (ctx_tokens)
    hlist = Vector{Union{T, Nothing} where T}(nothing, length(ps))
    done = false
    remained = ctx_tokens
    for (i, p) in enumerate(ps)
        @match p(remained) begin
            (nothing, a) =>
                begin
                    hlist = nothing
                    remained = a
                    done = true
                end
            (elt, a) =>
                begin
                    hlist[i] = elt
                    remained = a
                end
        end
        if done
            break
        end
    end
    (hlist !== nothing ? Tuple(hlist) : nothing, remained)
end

trans(f, p) = function (ctx_tokens)
    @match p(ctx_tokens) begin
        (nothing, _) && a => a
        (arg, remained) => (f(arg), remained)
    end
end

optparser(p) = function (ctx_tokens)
    @match p(ctx_tokens) begin
        (nothing, _) => (Some(nothing), ctx_tokens)
        (a, remained) => (Some(a), remained)
    end
end

"""
State.put
"""
updateparser(p, f) = function (ctx_tokens)
    @match p(ctx_tokens) begin
        (nothing, _) && a =>  a
        (a, remained) =>
            let state = f(remained.state, a)
                (a, update_state(remained, state))
            end
    end
end

"""
State.get
"""
getparser(p, f) = function (ctx_tokens)
    @match p(ctx_tokens) begin
        (nothing, _) && a =>  a
        (_, remained) => (f(remained.state), remained)
    end
end

tokenparser(f) = function (ctx_tokens)
    let tokens = ctx_tokens.tokens
        if tokens.current <= tokens.length
            token = tokens.source[tokens.current]
            if f(token)
                new_tokens = TokenView(tokens.current + 1, tokens.length, tokens.source)
                max_fetched = max(new_tokens.current, ctx_tokens.maxfetched)
                new_ctx = CtxTokens(new_tokens, max_fetched, ctx_tokens.state)
                return (token, new_ctx)
            end
        end
        (nothing, ctx_tokens)
    end
end

function direct_lr(init, trailer, reducer)
    function (ctx_tokens)
        res = nothing
        remained = ctx_tokens
        res, remained = init(remained)
        if res === nothing
            return (nothing, remained)
        end

        while true
            miscellaneous, remained = trailer(remained)
            if miscellaneous === nothing
                break
            end
            res = reducer(res, miscellaneous)
        end
        (res, remained)
    end
end

function update_state(ctx:: CtxTokens{A}, state::B) where {A, B}
    CtxTokens(ctx.tokens, ctx.maxfetched, state)
end

function crate
end


function crate(::Type{Vector{T}}) where T
    T[]
end


function crate(::Type{Vector{T} where T})
    []
end

function crate(::Type{Nothing})
    nothing
end

function crate(::Type{Union{T, Nothing}}) where T
    nothing
end

function crate(::Type{Bool}) where T
    false
end

function crate(::Type{T}) where T <: Number
    zero(T)
end

function crate(::Type{String})
    ""
end

function crate(::Type{Any}) where T
    nothing
end

function crate(::Type{LinkedList})
    nil(Any)
end

function crate(::Type{LinkedList{T}}) where T
    nil(T)
end


# number = mklexer(LexerSpec(r"\G\d+"))
# dio = mklexer(LexerSpec("dio"))
# a = mklexer(LexerSpec('a'))

# tables = [:number => number, :dio => dio, :a => a]
# lexer = @genlex tables


# preda(::Token{:a}) = true
# preda(_) = false

# prednum(::Token{:number}) = true
# prednum(_) = false


# preddio(::Token{:dio}) = true
# preddio(_) = false

# p = hlistparser([tokenparser(preddio), tokenparser(prednum), tokenparser(preda)])
# source = lexer("dio111a")
# println(source)
    # tokens = TokenView(source)
    # ctx = CtxTokens{Nothing}(tokens)
    # println(p(ctx))
