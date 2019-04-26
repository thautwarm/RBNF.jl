module MLPy
using RBNF

struct QASMLang end
second((a, b)) = b

RBNF.@parser QASMLang begin
    # define ignorances
    ignore{space}

    # define keywords
    reserved = [
        :keywords,
        :end,
        :fn,
        :match,
        :data,
        :where,
        :import
    ]
end
end