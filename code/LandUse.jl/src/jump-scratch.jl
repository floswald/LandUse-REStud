
using JuMP, GLPK

function tt(a)

	my_square(x) = x^2
	my_f(x,y) = (x - 1)^2 + (y - 2)^2

	model = Model(Ipopt.Optimizer)

	register(model, :my_f, 2, my_f, autodiff=true)
	register(model, :my_square, 1, my_square, autodiff=true)

	@variable(model, x[1:2] >= 0.5)
	@NLexpression(model, tt, my_f(x[1],x[2]))
	@NLconstraint(model, tt <= a)
	@NLobjective(model, Min, my_f(x[1], my_square(x[2])))
	optimize!(model)
	value.(x)

end


function jm(p::LandUse.Param,mo::LandUse.Region,x0::NamedTuple)

	# p = LandUse.Param()
	# mo = LandUse.Region(p)
	# x0 = stmodel2(p)  # starting value

	# setup Model object
	m = Model(Ipopt.Optimizer)

	# variables
	@variable(m, 0.001 <= ρr , start = x0.ρr)
	@variable(m, 0.001 <= ϕ <= p.S , start = x0.ϕ )
	@variable(m, 0.001 <= r  , start = x0.r )
	@variable(m, 0.001 <= Lr <= p.L , start = x0.Lr)
	@variable(m, 0.001 <= pr , start = x0.pr)
	@variable(m, 0.001 <= Sr <= p.S , start = x0.Sr)

	# nonlinear expressions
	@NLexpression(m, wu0, p.Ψ * p.θu * (p.L - Lr)^p.λ)
	@NLexpression(m, wr , wu0 - p.a * (p.θu^(p.taum)) * (ϕ^(p.taul)) )
	@NLexpression(m, qr , ((1+p.ϵr) * ρr)^(1.0/(1+p.ϵr)) )
	@NLexpression(m, r_pr_csbar, r - pr * p.cbar + p.sbar )
	@NLexpression(m, xsr, wr + r_pr_csbar )
	@NLexpression(m, Srh, Lr * (xsr * p.γ / (1.0 + p.ϵr)) / ρr )
	@NLexpression(m, cur, (1.0 - p.γ)*(1.0 - p.ν)*(wr + r_pr_csbar) - p.sbar)
	@NLexpression(m, cu_inputr,  (qr^(1 + p.ϵr)) * p.ϵr / (1.0+p.ϵr) )

	# expressions indexed at location l
	@NLexpression(m, nodes[i = 1:p.int_nodes], ϕ / 2 + ϕ / 2 * mo.inodes[i] )
	@NLexpression(m, τ[i = 1:p.int_nodes], p.a * p.θu^(p.taum) * nodes[i]^(p.taul) )
	@NLexpression(m, w[i = 1:p.int_nodes], wu0 - τ[i] )
	@NLexpression(m, q[i = 1:p.int_nodes], qr * ((w[i] + r_pr_csbar) / xsr)^(1.0/p.γ))
	@NLexpression(m, H[i = 1:p.int_nodes], q[i]^p.ϵr)
	@NLexpression(m, h[i = 1:p.int_nodes], p.γ * (w[i] + r_pr_csbar) / q[i])
	@NLexpression(m, ρ[i = 1:p.int_nodes], (q[i]^(1.0 + p.ϵr)) / (1.0 + p.ϵr) )
	@NLexpression(m, cu[i = 1:p.int_nodes], (1.0 - p.γ)*(1.0 - p.ν)*(w[i] + r_pr_csbar) - p.sbar)
	@NLexpression(m, D[i = 1:p.int_nodes] , H[i] / h[i])
	@NLexpression(m, cu_input[i = 1:p.int_nodes], q[i] * H[i] * p.ϵr / (1.0+p.ϵr) )

	# integrals
	@NLexpression(m, iDensity,  (ϕ/2) * sum(mo.iweights[i] * D[i] for i in 1:p.int_nodes))
	@NLexpression(m, iρ,        (ϕ/2) * sum(mo.iweights[i] * ρ[i] for i in 1:p.int_nodes))
	@NLexpression(m, icu,       (ϕ/2) * sum(mo.iweights[i] * cu[i] * D[i] for i in 1:p.int_nodes))
	@NLexpression(m, icu_input, (ϕ/2) * sum(mo.iweights[i] * cu_input[i] for i in 1:p.int_nodes))
	@NLexpression(m, iτ,        (ϕ/2) * sum(mo.iweights[i] * τ[i] * D[i] for i in 1:p.int_nodes))

	# objective
	@objective(m, Max, 1.0)

	# nonlinear constraints (they are actually linear but contain nonlinear expressions - which means we need the nonlinear setup)
	# F[1] = m.wr - foc_Lr(m.Lr / m.Sr , m.pr, p)
	@NLconstraint(m, wr - p.α * pr * p.θr * (p.α + (1-p.α)*( Sr / Lr )^((p.σ-1)/p.σ))^(1.0 / (p.σ-1)) == 0.0)

	# F[2] = m.ρr - foc_Sr(m.Lr / m.Sr , m.pr, p)
	@NLconstraint(m, ρr - (1-p.α)* pr * p.θr * (p.α * ( Lr / Sr )^((p.σ-1)/p.σ) + (1-p.α))^(1.0 / (p.σ-1)) == 0.0)

	# F[3] = m.Lu - m.iDensity
	@NLconstraint(m, p.L - Lr == iDensity)

	# F[4] = m.iρ + m.ρr * (m.Sr + m.Srh) - m.r * p.L
	@NLconstraint(m, iρ + ρr * (Sr + Srh) == r * p.L)

	# F[5] = p.S - p.useless - m.ϕ - m.Sr - m.Srh
	@NLconstraint(m, p.S - p.useless == ϕ + Sr + Srh)

	# F[6] = m.Lr * cur(p,m) + m.icu + m.Srh * cu_input(m.ϕ,p,m) + m.icu_input + m.wu0 * m.iτ - wu0(m.Lu, p)*m.Lu
	@NLconstraint(m, Lr * cur + icu + Srh * cu_inputr + icu_input + wu0 * iτ ==  wu0 * (p.L - Lr))

	optimize!(m)

	# check termination status
	if termination_status(m) != MOI.LOCALLY_SOLVED
		println("Termination status: $(termination_status(m))")
		error("model not locally solved")
	else
		println("rhor = $(value(ρr))")
		println("ϕ    = $(value(ϕ ))")
		println("r    = $(value(r ))")
		println("Lr   = $(value(Lr))")
		println("pr   = $(value(pr))")
		println("Sr   = $(value(Sr))")

		(ρr = value(ρr), ϕ = value(ϕ), r = value(r), Lr = value(Lr), pr = value(pr), Sr = value(Sr))

	end

end

function stmodel2(p::LandUse.Param)
	x00 = nocommute(p)
	# x00 = nlsolve((F,x) -> nocommute!(F,x,p),x0)
	gamma2 = p.γ / (1 + p.ϵr)

	ρ  = x00.ρ
	Lr = x00.Lr
	Lu = p.L - Lr
	wu = p.θu*Lu^p.λ
	wr = wu
	Sr = (((1 - p.α)/ p.α) * wr / ρ)^p.σ * Lr # farm land input
	r  = ρ * (p.S - p.useless) / p.L
	pr = wr / (p.α * p.θr) * (p.α + (1-p.α) * (Lr / Sr)^((1-p.σ)/p.σ))^(1/(1-p.σ))
	ϕ  = gamma2 * (wu + r - pr * p.cbar + p.sbar ) * Lu / (ρ)
	Srh= gamma2 * (wr + r - pr * p.cbar + p.sbar ) * Lr / (ρ)

	(ρr = ρ, ϕ = ϕ/10, r = r, Lr = Lr, pr = pr, Sr = Sr)

end

function nocommute(p::LandUse.Param)

    model = Model(Ipopt.Optimizer)

    # user-defined functions
    # Sr(Lr,ρ,wr,α) = (((1 - α)/ α) * wr / ρ)^σ * Lr)
    # register(model, :Sr, 4, Sr, autodiff=true)

    @variable(model, 0.001 <= ρ ,  start = 1.0)
    @variable(model, 0.001 <= Lr <= p.L, start = 0.5)

    # nonlinear expressions
    @NLexpression(model, wu, p.θu * (p.L - Lr)^p.λ)
    @NLexpression(model, Sr, (((1 - p.α)/ p.α) * wu / ρ)^p.σ * Lr)
    @NLexpression(model, pr, wu / (p.α * p.θr) * (p.α + (1-p.α) * (Lr / Sr)^((1-p.σ)/p.σ))^(1/(1-p.σ)))

    @NLexpression(model, ϕ,   p.γ / (1 + p.ϵr) * (wu + (ρ * (p.S - p.useless) / p.L) - pr * p.cbar + p.sbar) * (p.L - Lr) / ρ)
    @NLexpression(model, Srh, p.γ / (1 + p.ϵr) * (wu + (ρ * (p.S - p.useless) / p.L) - pr * p.cbar + p.sbar) * Lr / ρ)


    # objective
    @objective(model, Max, 1.0)

    # constraints

    @NLconstraint(model, (1 - p.γ) * (1 - p.ν) * (wu + (ρ * (p.S - p.useless) / p.L) - pr * p.cbar + p.sbar ) + p.ϵr * ρ * (Srh + ϕ) - p.sbar * p.L - (p.L - Lr) * p.θu == 0.0)
    @NLconstraint(model, Sr + Srh + ϕ - (p.S - p.useless) == 0.0)

    optimize!(model)

    if termination_status(model) != MOI.LOCALLY_SOLVED
        error("non-optimal exit")
    else
        return (ρ = value(ρ), Lr = value(Lr))
    end
end

function param_test()
    p = (a = 4.0,)
    function f(x,a)
        (x-a)^2
    end

    m = Model(Ipopt.Optimizer)
    JuMP.register(m,:myf, 2, f, autodiff=true)

    @variable(m,x)
    @NLobjective(m, Min, myf(x,p.a))
    optimize!(m)

end


function j_test(a)


    m = Model(Ipopt.Optimizer)
	@variable(m,x >= 1.0)
	@variable(m,y)

	@NLexpression(m, q, x^a + log(x + a) )

    @NLobjective(m, Min, y^2 + q)
    optimize!(m)

	(x = value(x), y = value(y))

end
