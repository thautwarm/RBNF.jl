using QASM2Jl

@parser begin
    @token
    a := r"\d"
    @grammar
    b := Many(a)
end

runlexer("123")

