"""
T is for dispatch.
"""

struct Token{T}
    lineno :: Int64
    colno  :: Int32
    offset :: Int64
    str    :: String
    span   :: Int32
end

"""
    Token{T}(str::String; lineno::Int=0, colno::Int=0, offset::Int=0, span::Int=0)

Create a Token of type `T` with content of given `str::String`.
"""
Token{T}(str; lineno::Int=0, colno::Int=0, offset::Int=0, span::Int=0) where T =
    Token{T}(lineno, colno, offset, String(str), span)
