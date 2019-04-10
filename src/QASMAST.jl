# Check https://arxiv.org/pdf/1707.03429.pdf for grammar specification

@parser begin
    # define ignorances
    ignore{space}

    @grammar
    # define grammars
    mainprogram := ["OPENQASM", real, ';', program]
    program     := [statement, '|', program]
    statement   := @or begin
        decl
        [gatedecl, Option(goplist), '}']
        ["opaque", id, Option(('(', Option(idlist), ')')), idlist, ';']
        qop
        ["if", '(', id, "==", nninteger, ')', qop]
        ["barrier", anylist]
    end

    decl        := ["qreg" | "creg", id, '[', nninetger, ']', ';']
    gatedecl    := ["gate", id, Option(('(', Option(idlist), ')')), idlist, '{']

    goplist     := Many(uop | ["barrier", idlist, ';'])

    qop         := uop | ["measure", argument, "->", argument, ';'] | ["reset", argument, ';']
    uop         := @or begin
        ['U', '(', explist, ')', argument, ';']
        ["CX", argument, ',', argument, ';']
        [id, Option(('(', Option(explist), ')')) + anylist + ';']
    end
    anylist    := idlist | mixedlist
    idlist     := @direct_recur begin
        init = id
        Prefix = (recur, ',', id)
    end

    mixeditem   := (id, Option(('[', nninteger, ']')))
    mixedlist   := @direct_recur begin
        init = mixeditem
        prefix = (recur, ',', mixeditem)
    end

    argument   := (id, Option(('[', nninteger, ']')))

    explist    := @direct_recur begin
        init = exp
        prefix = (recur,  ',', exp)
    end

    atom       := real | nninteger | "pi" | id | + fn + '(' + exp + ')' | '(' + exp + ')' | '-' + exp
    exp        := @direct_recur begin
        init = atom
        prefix = (recur, binop, atom)
    end
    fn         := "sin" | "cos" | "tan" | "exp" | "ln" | "sqrt"
    binop      := '+' | '-' | '*' | '/'

    # define tokens
    @token
    id        := r"\G[a-z]{1}[A-Za-z0-9_]*"
    real      := r"\G([0-9]+\.[0-9]*|[0-9]*\.[0.9]+)([eE][-+]?[0-9]+)?"
    nninteger := r"\G([1-9]+[0-9]*|0)"
    space     := r"\G\s+"
end
