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