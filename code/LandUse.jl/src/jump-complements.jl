using JuMP, Ipopt, Complementarity

function c1()
    m = Model(Ipopt.Optimizer)
    @variable(m, x>=0)
    @variable(m, y>=0)
    @variable(m, l[1:3])

    @NLobjective(m, Min, (x - 5)^2 + (2*y + 1)^2)

    @NLconstraint(m, 2*(y-1) - 1.5*x + l[1] - l[2]*0.5 + l[3] == 0)

    @complements(m, 0 <= 3*x - y - 3,        l[1] >= 0)
    @complements(m, 0 <= - x + 0.5*y + 4,    l[2] >= 0)
    @complements(m, 0 <= - x - y + 7,        l[3] >= 0)

    optimize!(m)

    @show objective_value(m)
    @assert isapprox(objective_value(m), 17.0000, atol=1e-4)
end

function c2()
    m = Model(Ipopt.Optimizer)
    @variable(m, x>=0)
    @variable(m, y>=0)
    @variable(m, l[1:3])

    @NLobjective(m, Min, (x - 5)^2 + (2*y + 1)^2)

    @NLconstraint(m, 2*(y-1) - 1.5*x + l[1] - l[2]*0.5 + l[3] == 0)

    # @complements(m, 0 <= 3*x - y - 3,        l[1] >= 0)
    @NLconstraint(m, (3*x - y - 3) * l[1] <= 1e-4)
    @NLconstraint(m, 3*x - y - 3 >= 0)
    
    @complements(m, 0 <= - x + 0.5*y + 4,    l[2] >= 0)
    @complements(m, 0 <= - x - y + 7,        l[3] >= 0)

    optimize!(m)

    @show objective_value(m)
    @assert isapprox(objective_value(m), 17.0000, atol=1e-4)
end