# RBNF.jl

Several examples are presented via tests.

Last afternoon Old Luo told me to help him with the QASM parser.

However I found something incredibly interesting so I just spent about 13 hours to achieve a handy parser generator in Julia, bringing about some useful ideas like
[Lens implemented in generated functions](https://github.com/thautwarm/RBNF.jl/blob/master/src/Lens.jl) and, more static ast rewriting compared to
[corresponding Python implementation](https://github.com/thautwarm/RBNF).

However, for I have TOEFL exams in very recent days, there're a large number of optimizations and specializations I haven't performed yet.

What's more, the syntax of this DSL could be more convenient. Currently, the indirect left recursions are not permitted, and you have to use `@direct_recur` to explicitly use direct left recursions. I also doubt that people sometimes might not need ast rewriters, but now rewritting is mandatory.


[QASM.jl](./test/QASM.jl):

The grammar is based on https://arxiv.org/pdf/1707.03429.pdf and tiny modified.

```julia
using RBNF

struct QASMLang end
second((a, b)) = b

RBNF.@parser QASMLang begin
    # define ignorances
    ignore{space}

    @grammar
    # define grammars
    mainprogram := ["OPENQASM", ver=real, ';', prog=program]
    program     := stmts=Many(statement)
    statement   := value=(decl | gate | opaque | qop | ifstmt | barrier)

    # stmts
    ifstmt      := ["if", '(', l=id, "==", r=nninteger, ')', body=qop]
    opaque      := ["opaque", id=id, ['(', [arglist1=idlist].?, ')'].? , arglist2=idlist, ';']
    barrier     := ["barrier", value=anylist]
    decl        := [regtype="qreg" | "creg", id=id, '[', int=nninteger, ']', ';']

    # gate
    gate        := [decl=gatedecl, [goplist=goplist].?, '}']
    gatedecl    := ["gate", id=id, ['(', [arglist1=idlist].?, ')'].?, arglist2=idlist, '{']

    goplist     := elts=Many(uop |barrier_ids)
    barrier_ids := ["barrier", ids=idlist, ';']
    # qop
    qop         := value=(uop | measure | reset)
    reset       := ["reset", arg=argument, ';']
    measure     := ["measure", arg1=argument, "->", arg2=argument, ';']

    uop         := value=(iduop | u | cx)
    iduop      := [op=id, ['(', [lst1=explist].?, ')'].?, lst2=anylist, ';']
    u          := ['U', '(', exprs=explist, ')', arg=argument, ';']
    cx         := ["CX", arg1=argument, ',', arg2=argument, ';']

    anylist    := value=(idlist | mixedlist)
    idlist     := value=@direct_recur begin
        init = id
        prefix = (recur, (',', id) => second)
    end

    mixeditem   := [id=id, ['[', arg=nninteger, ']'].?]
    mixedlist   := value=@direct_recur begin
        init = mixeditem
        prefix = (recur, (',', mixeditem) => second)
    end

    argument   := [id=id, ['[', (arg=nninteger), ']'].?]

    explist    := value=@direct_recur begin
        init = exp
        prefix = (recur,  (',', exp) => second)
    end

    atom       := (value=(real | nninteger | "pi" | id | fnexp)) | ['(', (value=exp), ')'] | (value=neg)
    fnexp      := [fn=fn, '(', arg=exp, ')']
    neg        := ['-', value=exp]
    exp        := value=@direct_recur begin
        init = atom
        prefix = (recur, binop, atom)
    end
    fn         := name=("sin" | "cos" | "tan" | "exp" | "ln" | "sqrt")
    binop      := name=('+' | '-' | '*' | '/')

    # define tokens
    @token
    id        := r"\G[a-z]{1}[A-Za-z0-9_]*"
    real      := r"\G([0-9]+\.[0-9]*|[0-9]*\.[0.9]+)([eE][-+]?[0-9]+)?"
    nninteger := r"\G([1-9]+[0-9]*|0)"
    space     := r"\G\s+"
end
```

