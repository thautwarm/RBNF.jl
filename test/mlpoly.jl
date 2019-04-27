using RBNF
using MLStyle

struct MLPolyLang
end
second((a, b)) = b
escape(a) = @match a begin
    '\\'  => '\\'
    '"'   => '"'
    'a'   => '\a'
    'n'   => '\n'
    'r'   => '\r'
    a     => throw(a)
end
join_token_as_str =  xs -> join(x.value for x in xs)
RBNF.@parser MLPolyLang begin
    # define ignorances
    ignore{space}

    # define keywords
    reserved = [true, false]

    @grammar
    Str       :=  value=['"', Escape{*} => join_token_as_str, '"']
    Escape    :=  (('\\', _) => x -> escape(x[2])) | !'"'
    Let       := [hd=:let, rec=:rec.?, binds=(LetBind, ((',', LetBind) => second){*}), :in, body=Exp]
    LetBind   := [bind=id, '=', value=Exp]
    Fun       := [hd=:fn, args=id{*}, "->", Exp]
    Data      := [hd=:data, name=id, ":", kind=Exp, :where, stmts=Stmt{*}, :end]
    Import    := [hd=:import, paths=(id, [",", id]{*})]
    Match     := [hd=:match, sc=Exp, :with,
                        cases=(['|', Exp, "->", Exp] => (_, case, _, body) -> (case, body)){*},
                    :end.?] # end for nested match
    If        := [hd=:if, cond=Exp, :then,
                        br1=Exp,
                     :else,
                        br2=Exp]
    Class     := [hd=:class, (constraint=Call, "=>").?, name=id, ":", kind=Exp, :where,
                     interfaces=Annotate{*},
                 :end]
    Instance  := [hd=:instance, name=id, vars=Exp{*}, :where, impls=Stmt{*}, :end]
    NestedExpr:= [hd='(', value=Expr, ')']
    Var       := value=id
    Block     := [hd=:begin, stmts=Stmt{*}, :end]
    Atom      := value=(NestedExpr | Num | Str | Boolean | Var | Block)
    Num       := [[neg="-"].?, (int=integer) | (float=float)]
    Boolean   := value=("true" | "false")
    Call      := [fn=Atom, args=Atom{*}]
    Typing    := [[:forall, fresh=(["{", Call, "}"] => x -> x[2]){*}].?, from=Call, ("->", to=Typing).?]
    Comp      := value=(Typing | Let | Fun | Match | If | Quote | Insert)
    Exp       := value=@direct_recur begin
        init = Comp
        prefix = [recur, Op, Comp]

    end
    Op        := ['`', name=_, '`']
    Annotate  := [:val, name=id, ":", typ=Exp]
    Stmt      = Data | Import | Class | Instance | Annotate | Exp
    Quote     := [hd=:quote, stmts=Exp{*}, :end]
    Insert    := [hd='$', exp=Exp]
    Module    := [hd=:module, name=id, :where, stmts=Stmt{*}, :end.?]

    @token
    id        := r"\G[A-Za-z_]{1}[A-Za-z0-9_]*"
    float     := r"\G([0-9]+\.[0-9]*|[0-9]*\.[0.9]+)([eE][-+]?[0-9]+)?"
    integer   := r"\G([1-9]+[0-9]*|0)"
    space     := r"\G\s+"
end


src1 = """
module Poly where
let _ = 1 in 2
class Bi : Type -> Type where
    val a : Int
    val b : Int
end
"""

tokens = RBNF.runlexer(MLPolyLang, src1)
ast, ctx = RBNF.runparser(Module, tokens)
RBNF.PFormat.pprint(ast)