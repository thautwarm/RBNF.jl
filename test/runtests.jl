using RBNF
using MLStyle

# rmlines = @λ begin
#            e :: Expr           -> Expr(e.head, filter(x -> x !== nothing, map(rmlines, e.args))...)
#              :: LineNumberNode -> nothing
#            a                   -> a
# end


# node = quote
# RBNF.@parser TestLang begin
#     @token
#     a := r"\d"
#     @grammar
#     b := [(k = k.?), (seqa :: Vector = Many(a => parse_int))]
# end
# end

# @info :show rmlines(macroexpand(Main, node))

# struct TestLang end
# parse_int(x :: RBNF.Token) = parse(Int, x.str)

# RBNF.@parser TestLang begin
#     @token
#     a := r"\G\d"
#     k := "kkk"
#     @grammar
#     b := [(k = k.?), (seqa :: Vector = Many(a => parse_int))]
# end

# res1, _ = RBNF.runparser(b, RBNF.runlexer(TestLang, "123"))
# println(res1)

# res2, _ = RBNF.runparser(b, RBNF.runlexer(TestLang, "kkk123"))
# println(res2)


# struct TestLang2 end

# RBNF.@parser TestLang2 begin
#     @token
#     num := r"\G\d+"
#     space := r"\G\s+"
#     @grammar
#     manynumbers := @direct_recur begin
#         init = num
#         prefix = [recur, space, num]
#     end
# end

# res3, _ = RBNF.runparser(manynumbers, RBNF.runlexer(TestLang2, "52 123 123 14312 213"))
# println(res3)

module QASM
using RBNF

struct QSAMLang end
second((a, b)) = b

RBNF.@parser QSAMLang begin
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

"""
"""
src1 = """
OPENQASM 2.0;

gate cu1(lambda) a,b
{
    U(0,0,theta/2) a;
    CX a,b;
    U(0,0,-theta/2) b;
    CX a,b;
    U(0,0,theta/2) b;
}

qreg q[3];
qreg a[2];
creg c[3];
creg syn[2];
cu1(pi/2) q[0],q[1]
"""
ast, ctx = RBNF.runparser(mainprogram, RBNF.runlexer(QSAMLang, src1))
@info :qasmparsing ast
end