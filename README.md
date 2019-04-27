# RBNF.jl

(尝试写一篇中文README， 稍微介绍一下现代科技)

Julia语言的Restructurable BNF。

样例 [MLPoly.jl](./test/mlpoly.jl)
```julia
using RBNF
using MLStyle

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

# 一个不可变的对象，如MLPolyLang这个DataType, 作为语言标志
RBNF.@parser MLPolyLang begin

    # 定义要过滤的token名. space在token定义处给出。
    ignore{space}

    # 定义保留字， 可以是字符串，或者符号
    reserved = [true, false]

    # 定义文法
    # := 表示语法节点， parser会生成一个对应的结构体,
    #                   其各字段如对应文法定义。
    # = 表示非语法节点， parser输出如对应文法对应。
    @grammar
    # '?' 和 "?" 表示对应的token应该对应的字面量
    # r"?" 表示应该对应的正则
    Str       :=  value=['"', Escape{*} => join_token_as_str, '"']

    # => 表示重写规则
    Escape    :=  (('\\', _) => x -> escape(x[2])) | !'"'

    # a.? 表示a可选。 a{*} 表示重复解析a 0到无穷次
    Let       := [hd=:let, rec=:rec.?, binds=(LetBind, ((',', LetBind) => second){*}), :in, body=Exp]
    LetBind   := [bind=id, '=', value=Exp]
    Fun       := [hd=:fn, args=id{*}, "->", Exp]
    Data      := [hd=:data, name=id, ":", kind=Exp, :where, stmts=Stmt{*}, :end]
    Import    := [hd=:import, paths=(id, [",", id]{*})]
    Match     := [hd=:match, sc=Exp, :with,
                        cases=(['|', Exp, "->", Exp] => (_, case, _, body) -> (case, body)){*},
                    :end.?] # end for nested match
    # 规则写得尽量像这个语言用起来的样子。
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

    # 显式处理直接左递归, recur表示自己。不懂请百科。。
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

    # 定义token
    @token
    id        := r"\G[A-Za-z_]{1}[A-Za-z0-9_]*"
    float     := r"\G([0-9]+\.[0-9]*|[0-9]*\.[0.9]+)([eE][-+]?[0-9]+)?"
    integer   := r"\G([1-9]+[0-9]*|0)"
    space     := r"\G\s+"
end
```

以上文法经过分析， 自动生成高效的parser和lexer(将字符串重整为tokens的东西)。

使用方法如下:
```
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
```

然后得到结果

```
Struct_Module(
  hd=Token{reserved}(str=module, lineno=1, colno=1),
  name=Token{id}(str=Poly, lineno=1, colno=1),
  stmts=[
    Struct_Exp(
      value=Struct_Comp(
        value=Struct_Let(
          hd=Token{reserved}(str=let, lineno=2, colno=2),
          rec=Some{Nothing}(
            value=nothing,
          ),
          binds=(
            Struct_LetBind(
              bind=Token{id}(str=_, lineno=2, colno=2),
              value=Struct_Exp(
                value=Struct_Comp(
                  value=Struct_Typing(
                    fresh=nothing,
                    from=Struct_Call(
                      fn=Struct_Atom(
                        value=Struct_Num(
                          neg=nothing,
                          int=Token{integer}(str=1, lineno=2, colno=2),
                          float=nothing,
                        ),
                      ),
                      args=[],
                    ),
                    to=nothing,
                  ),
                ),
              ),
            ),
            [],
          ),
          body=Struct_Exp(
            value=Struct_Comp(
              value=Struct_Typing(
                fresh=nothing,
                from=Struct_Call(
                  fn=Struct_Atom(
                    value=Struct_Num(
                      neg=nothing,
                      int=Token{integer}(str=2, lineno=2, colno=2),
                      float=nothing,
                    ),
                  ),
                  args=[],
                ),
                to=nothing,
              ),
            ),
          ),
        ),
      ),
    ),
    Struct_Class(
      hd=Token{reserved}(str=class, lineno=3, colno=3),
      constraint=nothing,
      name=Token{id}(str=Bi, lineno=3, colno=3),
      kind=Struct_Exp(
        value=Struct_Comp(
          value=Struct_Typing(
            fresh=nothing,
            from=Struct_Call(
              fn=Struct_Atom(
                value=Struct_Var(
                  value=Token{id}(str=Type, lineno=3, colno=3),
                ),
              ),
              args=[],
            ),
            to=Struct_Typing(
              fresh=nothing,
              from=Struct_Call(
                fn=Struct_Atom(
                  value=Struct_Var(
                    value=Token{id}(str=Type, lineno=3, colno=3),
                  ),
                ),
                args=[],
              ),
              to=nothing,
            ),
          ),
        ),
      ),
      interfaces=[
        Struct_Annotate(
          name=Token{id}(str=a, lineno=4, colno=4),
          typ=Struct_Exp(
            value=Struct_Comp(
              value=Struct_Typing(
                fresh=nothing,
                from=Struct_Call(
                  fn=Struct_Atom(
                    value=Struct_Var(
                      value=Token{id}(str=Int, lineno=4, colno=4),
                    ),
                  ),
                  args=[],
                ),
                to=nothing,
              ),
            ),
          ),
        ),
        Struct_Annotate(
          name=Token{id}(str=b, lineno=5, colno=5),
          typ=Struct_Exp(
            value=Struct_Comp(
              value=Struct_Typing(
                fresh=nothing,
                from=Struct_Call(
                  fn=Struct_Atom(
                    value=Struct_Var(
                      value=Token{id}(str=Int, lineno=5, colno=5),
                    ),
                  ),
                  args=[],
                ),
                to=nothing,
              ),
            ),
          ),
        ),
      ],
    ),
  ],
)
```


还有一个QASM的Parser实例见: [QASM.jl](./test/QASM.jl):