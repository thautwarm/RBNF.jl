using RBNF
using MLStyle
using PrettyPrint
using DataStructures
struct ReML
end

second((a, b)) = b
second(vec::V) where V <: AbstractArray = vec[2]

escape(a) = @match a begin
    '\\'  => '\\'
    '"'   => '"'
    'a'   => '\a'
    'n'   => '\n'
    'r'   => '\r'
    a     => throw(a)
end

join_token_as_str = xs -> join(x.value for x in xs)

maybe_to_bool(::Some{Nothing}) = false
maybe_to_bool(::Some{T}) where T = true

"""
nonempty sequence
"""
struct GoodSeq{T}
    head :: T
    tail :: Vector{T}
end

join_rule(sep, x) = begin
    :([$x, ([$sep, $x] % second){*}] % x -> GoodSeq(x[1], x[2]))
end

RBNF.typename(name:: Symbol, ::Type{ReML}) = Symbol(:R, name)

RBNF.@parser ReML begin
    # define the ignores
    ignore{space}

    # define keywords
    reserved = [true, false]

    @grammar
    # necessary
    Str       :=  value=str

    Bind      := [name=id, '=', value=Exp]
    Let       := [hd=:let, rec=:rec.? % maybe_to_bool,
                        binds=join_rule(',', Bind),
                     :in, body=Exp]

    Fun       := [hd=:fn, args=id{*}, "->", body=Exp]
    Import    := [hd=:import, paths=join_rule(',', id)]
    Match     := [hd=:match, sc=Exp, :with,
                        cases=(['|', Comp, "->", Exp] % ((_, case, _, body), ) -> (case, body)){*},
                    :end.?] # end for nested match
    If        := [hd=:if, cond=Exp, :then,
                        br1=Exp,
                     :else,
                        br2=Exp]
    Num       := [[neg="-"].? % maybe_to_bool, (int=integer) | (float=float)]
    Boolean   := value=("true" | "false")
    NestedExpr = ['(', value=Exp, ')'] => _.value
    Var       := value=id
    Block     := [hd=:begin, stmts=Stmt{*}, :end]
    Atom      =  NestedExpr | Num | Str | Boolean | Var
    Annotate  := [value=Atom, [':', Exp].?]
    Call      := [fn=Annotate, args=Annotate{*}]
    Comp      := [[forall=Forall, '.'].?, value=(Call | Let | Fun | Match | If | Block)]
    Op        := ['`', name=_, '`'] | [is_typeop :: Bool = "->" => true]
    Exp       := [hd=Comp, tl=[Op, Comp]{*}]

    Decl      := [hd=:val, name=id, ':', typ=Exp]
    Define    := [hd=:def, name=id, '=', value=Exp]
    Stmt      = Data | Import | Class | Instance | Decl |  Define | Exp
    Module     := [hd=:module, name=id, :where, stmts=Stmt{*}, :end.?]

    # forall
    Constaints:= [hd=:that, elts=join_rule(',', Call)]
    Forall    := [:forall, fresh=id{*}, (constraints=Constaints).?]

    # advanced
    Data      := [hd=:data, name=id, ":", kind=Exp, :where, stmts=Stmt{*}, :end]
    Class     := [hd=:class, name=id, ids=id{*},  (constrains=Constaints).?, :where,
                     interfaces=Decl{*},
                 :end]
    Instance  := [hd=:instance, name=id, vars=Exp{*}, :where, impls=Stmt{*}, :end]

    @token
    id        := r"\G[A-Za-z_]{1}[A-Za-z0-9_]*"
    str       := @quote ("\"" ,"\\\"", "\"")
    float     := r"\G([0-9]+\.[0-9]*|[0-9]*\.[0.9]+)([eE][-+]?[0-9]+)?"
    integer   := r"\G([1-9]+[0-9]*|0)"
    space     := r"\G\s+"
end


src1 = """
module Poly where
def a = "12\\"3"

class Monad m that Functor m where
    val bind : forall a b . m a -> (a -> b) -> m b
end

val a : Int
def a = "12345"

val f : Int -> Int
def f = fn a -> a `add` 1

val g : Int
def g = match 1 with
        | 1 -> 2
        | _ -> 0
        end

def z = fn x -> if x `equals` 1 then 1 else 2
"""

tokens = RBNF.runlexer(ReML, src1)
ast, ctx = RBNF.runparser(Module, tokens)
pprint(ast)