"""
Check https://arxiv.org/pdf/1707.03429.pdf for grammar specification
"""
module Parser

using MLStyle

macro parserc(x)
    @match x begin
        quote
            $(args...)
        end =>
            for (i, each) in enumerate(args)
                @info i each
            end
    end

end

@parserc begin
    # define ignorances
    ignore{r"\s"}

    # define grammars
    mainprogram := "OPENQASM" + real + ';' + program
    program     := statement + '|' + program
    statement   := @or begin
        decl
        gatedecl + [goplist] + '}'
        "opaque" + id + ['(' + [idlist] + ')'] + idlist + ';'
        qop
        "if" + '(' + id + "==" + nninteger + ')' + qop
        "barrier" anylist
    end

    decl        := ("qreg" | "creg") + id + '[' + nninetger + ']' + ';'
    gatedecl    := "gate" + id + ['(' + [idlist] + ')'] + idlist + '{'

    goplist     := @direct_recur begin
        init   = uop | "barrier" + idlist + ';'
        prefix = recur
    end

    qop         := uop | "measure" + argument + "->" + argument + ';' | "reset" + argument + ';'
    uop         := @or begin
        'U' + '(' + explist + ')' + argument + ';'
        "CX" + argument + ',' + argument + ';'
        id + ['(' + [explist] + ')'] + anylist + ';'
    end
    anylist    := idlist | mixedlist
    idlist     := id + [',' + idlist]
    mixedlist  := @direct_recur begin
        init = id + ['[' + nninteger + ']']
        prefix = recur + ','
    end

    argument   := id + ['[' + nninteger + ']']

    explist    := @direct_recur begin
        init = exp
        prefix = recur +  ','
    end

    exp        := @direct_recur begin
        init = real | nninteger | "pi" | id | + fn + '(' + exp + ')' | '(' + exp + ')' | '-' + exp
        trailer = binop
    end
    fn         := "sin" | "cos" | "tan" | "exp" | "ln" | "sqrt"
    binop      := '+' | '-' | '*' | '/'

    # define tokens

    id        === r"\G[a-z]{1}[A-Za-z0-9_]*"
    real      === r"\G([0-9]+\.[0-9]*|[0-9]*\.[0.9]+)([eE][-+]?[0-9]+)?"
    nninteger === r"\G([1-9]+[0-9]*|0)"
end


end
