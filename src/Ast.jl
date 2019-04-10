"""
migrated from https://github.com/QCGPU/qasm-rust/blob/master/src/ast.rs
"""
module AST

using MLStyle

I32 = Int32

@data Argument begin
    Qubit(String, I32)
    Register(String)
end

@data ASTNode begin
    QReg(String, I32)
    CReg(String, I32)
    Barrier(Argument)
    Reset(Argument)
    Measure(Argument, Argument)
    ApplyGate(String, Vector{Argument}, Vector{String})
    Opaque(String, Vector{Argument}, Vector{String})
    Gate(String, Vector{String}, Vector{String}, Vector{ASTNode})
    If(String, I32, ASTNode)
end

end

