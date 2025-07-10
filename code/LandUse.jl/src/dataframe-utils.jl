
# dataframe utilities

# shortcut anonymous function for comparisons
ieq(z) = (x -> x .== z)
neq(z) = (x -> x .!= z)
leq(z) = (x -> x .<= z)
le(z) = (x -> x .< z)
geq(z) = (x -> x .>= z)
ge(z) = (x -> x .> z)

# normalizer functions
nsum() = (x -> x ./ sum(x))


# other macros

macro timeout(seconds, expr, fail)
    quote
        tsk = @task $expr
        schedule(tsk)
        Timer($seconds) do timer
            istaskdone(tsk) || Base.throwto(tsk, InterruptException())
        end
        try
            fetch(tsk)
        catch _
            $fail
        end
    end
end

