struct Lens{T, Fields, Field}
end

@generated function runlens(main, new_field, lens::Type{Lens{T, Fields, Field}}) where {T, Fields, Field}
    quote
        $T($([field !== Field ? :(main.$field) : :new_field for field in Fields]...))
    end
end

