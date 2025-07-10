raw_index(v::MOI.VariableIndex) = v.value


"""
solve model at current [`Param`](@ref) `p` and starting at point `x0`

## Keyword:

* `estimateθ`: whether the sequence of ``\\theta_u`` should be a choice variable or not. default `false`.

"""
function jm(p::Param,x0::NamedTuple; estimateθ = false)

	# setup Model object
	m = JuMP.Model(Ipopt.Optimizer)
	set_optimizer_attribute(m, MOI.Silent(), true)

	# variables
	@variable(m, ρr >=         0.01 * x0.ρr  , start = x0.ρr)
	@variable(m, 0.01 * x0.ϕ <= ϕ <= p.S     , start = x0.ϕ )
	@variable(m, r >=      0.1 * x0.r        , start = x0.r )
	@variable(m, 0.1 * x0.Lr <= Lr <= p.L    , start = x0.Lr)
	@variable(m, pr >=        0.05 * x0.pr   , start = x0.pr)
	@variable(m, 0.05 * x0.Sr <= Sr <= p.S   , start = x0.Sr)
	
	if estimateθ
		@variable(m, θu , start = x0.θu)
		@variable(m, θr , start = x0.θr)
	else
		θu = p.θu
		θr = p.θr
	end

	# nonlinear expressions
	@NLexpression(m, P, (p.ν * pr ^ (1-p.σc) + (1 - p.ν))^(1/(1 - p.σc) ))


	@NLexpression(m, Lu, p.L - Lr)
	@NLexpression(m, wu0, θu * (p.L - Lr)^p.λ)
	@NLexpression(m, wr , p.Ψ * (wu0 - (p.a * Lu^p.μ) * (wu0^(p.ξw)) * ( (p.d1 * ϕ + ϕ / ( 1 + p.d2 * ϕ ) )^(p.ξl))) )
	@NLexpression(m, qr , ((1+p.ϵr) * ρr)^(1.0/(1+p.ϵr)) )
	@NLexpression(m, r_pr_csbar, r - pr * p.cbar + p.sbar )
	@NLexpression(m, xsr, wr + r_pr_csbar )
	@NLexpression(m, hr, p.γ * xsr / qr )
	@NLexpression(m, Hr, qr^p.ϵr )
	# @NLexpression(m, Srh, Lr * (xsr * p.γ / (1.0 + p.ϵr)) / ρr )
	@NLexpression(m, Srh, Lr * hr / Hr )
	@NLexpression(m, cur, (1.0 - p.γ)*(1.0 - p.ν)* (1/P)^(1-p.σc) *(wr + r_pr_csbar) - p.sbar)
	@NLexpression(m, cu_inputr,  (qr^(1 + p.ϵr)) * p.ϵr / (1.0+p.ϵr) )

	# expressions indexed at location l
	@NLexpression(m, nodes[i = 1:p.int_nodes], ϕ / 2 + ϕ / 2 * p.inodes[i] )
	if p.ϵflat
		@NLexpression(m, ϵ[i = 1:p.int_nodes], p.ϵr)
	else
		@NLexpression(m, ϵ[i = 1:p.int_nodes], p.ϵr * nodes[i] / ϕ + p.ϵs * (ϕ - nodes[i])/ϕ)
	end
	@NLexpression(m, dnodes[i = 1:p.int_nodes], p.d1 * ϕ + nodes[i] / ( 1 + p.d2 * ϕ ) )
	@NLexpression(m, τ[i = 1:p.int_nodes], (p.a * Lu^p.μ) * wu0^(p.ξw) * dnodes[i]^(p.ξl) )
	@NLexpression(m, w[i = 1:p.int_nodes], wu0 - τ[i] )
	# @warn "hard coding abs() for q function" maxlog=1
	# @NLexpression(m, q[i = 1:p.int_nodes], qr * (abs((w[i] + r_pr_csbar) / xsr))^(1.0/p.γ))
	@NLexpression(m, q[i = 1:p.int_nodes], qr * ((w[i] + r_pr_csbar) / xsr)^(1.0/p.γ))
	@NLexpression(m, H[i = 1:p.int_nodes], q[i]^ϵ[i])
	@NLexpression(m, h[i = 1:p.int_nodes], p.γ * (w[i] + r_pr_csbar) / q[i])
	@NLexpression(m, ρ[i = 1:p.int_nodes], (q[i]^(1.0 + ϵ[i])) / (1.0 + ϵ[i]) )
	@NLexpression(m, cu[i = 1:p.int_nodes], (1.0 - p.γ)*(1.0 - p.ν) * (1/P)^(1-p.σc) * (w[i] + r_pr_csbar) - p.sbar)
	@NLexpression(m, D[i = 1:p.int_nodes] , H[i] / h[i])
	@NLexpression(m, cu_input[i = 1:p.int_nodes], q[i] * H[i] * ϵ[i] / (1.0+ϵ[i]) )


	# integrals
	@NLexpression(m, iDensity,  (ϕ/2) * sum(p.iweights[i] * 2π * nodes[i] * D[i] for i in 1:p.int_nodes))
	@NLexpression(m, iρ,        (ϕ/2) * sum(p.iweights[i] * 2π * nodes[i] * ρ[i] for i in 1:p.int_nodes))
	@NLexpression(m, icu,       (ϕ/2) * sum(p.iweights[i] * 2π * nodes[i] * cu[i] * D[i] for i in 1:p.int_nodes))
	@NLexpression(m, icu_input, (ϕ/2) * sum(p.iweights[i] * 2π * nodes[i] * cu_input[i] for i in 1:p.int_nodes))
	@NLexpression(m, iτ,        (ϕ/2) * sum(p.iweights[i] * 2π * nodes[i] * τ[i] * D[i] for i in 1:p.int_nodes))


	@objective(m, Min, 1.0)
	# @objective(m, Min, (pr - p.moments[p.it,:P_rural])^2)
	# @objective(m, Min, (Lr / p.Lt[p.it] - p.moments[p.it,:Employment_rural])^2 + (pr - p.moments[p.it,:P_rural])^2)

	# nonlinear constraints (they are actually linear but contain nonlinear expressions - which means we need the nonlinear setup)
	# F[1] = m.wr - foc_Lr(m.Lr / m.Sr , m.pr, p)
	@NLconstraint(m, wr - p.α * pr * θr * (p.α + (1-p.α)*( Sr / Lr )^((p.σ-1)/p.σ))^(1.0 / (p.σ-1)) == 0.0)
	# @complements(m, wr - p.α * pr * θr * (p.α + (1-p.α)*( Sr / Lr )^((p.σ-1)/p.σ))^(1.0 / (p.σ-1)), 0 <= Lr <= p.L)

	# F[2] = m.ρr - foc_Sr(m.Lr / m.Sr , m.pr, p)
	# transforming into a complementary slackness contraint: set this to >=
	# and uncomment next 2 liens
	@NLconstraint(m, ρr - (1-p.α)* pr * θr * (p.α * ( Lr / Sr )^((p.σ-1)/p.σ) + (1-p.α))^(1.0 / (p.σ-1)) == 0.0)
	# @NLconstraint(m, ρr - (1-p.α)* pr * θr * (p.α * ( Lr / Sr )^((p.σ-1)/p.σ) + (1-p.α))^(1.0 / (p.σ-1)) >= 0.0)
	# @NLconstraint(m, (ρr - (1-p.α)* pr * θr * (p.α * ( Lr / Sr )^((p.σ-1)/p.σ) + (1-p.α))^(1.0 / (p.σ-1))) * Lr <= 0.00001)
	# @NLconstraint(m, ρr - (1-p.α)* pr * θr * (p.α * ( Lr / Sr )^((p.σ-1)/p.σ) + (1-p.α))^(1.0 / (p.σ-1)) >= 0.0)

	# F[3] = m.Lu - m.iDensity
	@NLconstraint(m, p.L - Lr == iDensity)

	# F[4] = m.iρ + m.ρr * (m.Sr + m.Srh) - m.r * p.L
	@NLconstraint(m, iρ + ρr * (Sr + Srh) == r * p.L)

	# F[5] = p.S - p.useless - m.ϕ - m.Sr - m.Srh
	@NLconstraint(m, p.S - p.useless == ϕ^2 * π + Sr + Srh)

	# F[6] = m.Lr * cur(p,m) + m.icu + m.Srh * cu_input(m.ϕ,p,m) + m.icu_input + m.wu0 * m.iτ - wu0(m.Lu, p)*m.Lu
	@NLconstraint(m, Lr * cur + icu + Srh * cu_inputr + icu_input + iτ ==  wu0 * (p.L - Lr))

	@NLconstraint(m, cur >= 0)
	# @NLconstraint(m, cui[i = 1:p.int_nodes], cu[i] >= 0)
	# @NLconstraint(m, xsr >= 0)

	if p.it == 1 && estimateθ
		@constraint(m, θu == θr)
	end

	JuMP.optimize!(m)

	# check termination status
	if termination_status(m) != MOI.LOCALLY_SOLVED
		error("model not locally solved")
	else
		(ρr = value(ρr), 
		ℙ = NaN, 
		ϕ = value(ϕ), 
		r = value(r), 
		Lr = value(Lr), 
		pr = value(pr), 
		Sr = value(Sr), 
		θu = estimateθ ? value(θu) : θu, 
		θr = estimateθ ? value(θr) : θr)
	end
end



"""
solve a `Country` at point x0

## keywords:

* `estimateθ`: whether the sequence of ``\\theta_u`` should be a choice variable
* `solve` : solve the model (`true`) or return the model object for debugging?
* `fit_allyears` : try to match the population distribution in all available years or just a single year.
"""
function jc(C::Country,x0::NamedTuple; estimateθ = false, solve = true, fit_allyears = true)

	pp = C.pp   # vector of params
	p  = pp[1]   # param of region 1
	K = C.K
	# println("fit all years = $fit_allyears")
	# if K > 20 
	# 	error("city data only for top 20 prepared")
	# else
		# prepare population weights and relative urban populations
		data = subset(p.citylist)#, :rank => x -> x .<= K)
		# find closest year in data
		datayears = unique(data.year)
		modelyears = p.T
		i1920 = argmin( abs.(modelyears .- 1920) )
		itx = fit_allyears ? p.it : i1920
		iyear = argmin( abs.(datayears .- modelyears[itx]) )
		yyear = datayears[iyear]
		ydata = subset(data, :year => x -> x .== yyear)
		sort!(ydata, :rank)
		# println(ydata)
		popwgt = ydata[!,:pop_data]  ./ sum(ydata[!,:pop_data])
		relpop = ydata[!,:pop_data]  ./  ydata[1,:pop_data]
	# end


	# setup Model object
	m = JuMP.Model(Ipopt.Optimizer)
	set_optimizer_attribute(m, MOI.Silent(), true)
	# set_optimizer_attribute(m, "constr_viol_tol", 1e-9)
	# lbs = [x0...] .* 0.3

	# variables
	@variable(m, LS >=   0.1 * x0.LS  , start = x0.LS)
	@variable(m, r  >=   0.1 * x0.r   , start = x0.r )
	@variable(m, pr >=  0.05 * x0.pr  , start = x0.pr)

	@variable(m, 0.1  * x0.Sr[ik] <= Sr[ik = 1:K] <= C.Sk[ik], start = x0.Sr[ik])
	@variable(m, 0.05 * x0.Lu[ik] <= Lu[ik = 1:K] <= C.L     , start = x0.Lu[ik])

	if estimateθ
		@variable(m, θu[ik = 1:K] , start = p.θu)  # everybody starts with θu of city 1
	else
		θu = [pp[ik].θu for ik in 1:K]
	end

	# all θs are in p[ik].θ
	σ1 = (p.σ - 1) / p.σ
	σ2 = 1 / (p.σ - 1)

	# constant expressions in each country's rural part
	# TODO find a simple way to generate θr[k] exogenously different from θr series.
	# TODO θr[k] will mean wr[k] etc
	# TODO Lr[k] is an additional K choice variables, LS disappears
	@NLexpression(m, wr , p.α * pr * p.θr * (p.α + (1-p.α)*( 1.0 / LS )^(σ1))^(σ2) )
	@NLexpression(m, ρr , (1-p.α)* pr * p.θr * (p.α * (LS)^(σ1) + (1-p.α))^σ2)
	@NLexpression(m, qr , ((1+p.ϵr) * ρr)^(1.0/(1+p.ϵr)) )

	@NLexpression(m, r_pr_csbar, r - pr * p.cbar + p.sbar )
	@NLexpression(m, xsr, wr + r_pr_csbar )
	@NLexpression(m, hr, p.γ * xsr / qr )
	@NLexpression(m, Hr, qr^p.ϵr )
	@NLexpression(m, cur, (1.0 - p.γ)*(1.0 - p.ν)*(wr + r_pr_csbar) - p.sbar)
	@NLexpression(m, cu_inputr,  (qr^(1 + p.ϵr)) * p.ϵr / (1.0+p.ϵr) )

	# indexed by only k
	@NLexpression(m, Lr[ik = 1:K], LS * Sr[ik])  # rural pop from labor to land share LS
	@NLexpression(m, Srh[ik = 1:K], Lr[ik] * hr / Hr )   # housing space for rural pop
	@NLexpression(m, wu0[ik = 1:K], θu[ik] * Lu[ik]^p.λ)  # urban wage in each city center

	# getting the fringe of each city

	# approach 1: using equation τ(d(ϕ)) = wu - wr => d(ϕ) = τ^-1(wu - wr)
	# @variable(m, dϕ[ik=1:K] >= 0.0	)
	if (p.d1 > 0) || (p.d2 > 0)
		@NLexpression(m,        dϕ[ik = 1:K], ( (wu0[ik] - wr) / ((p.a * Lu[ik]^p.μ) * wu0[ik]^(p.ξw)) )^(1.0/p.ξl)) 
		# @NLconstraint(m,  constr_ϕ[ik = 1:K], dϕ[ik] == p.d1 * ϕ[ik] + (ϕ[ik] / (1 + p.d2 * ϕ[ik])) )  # add constraint that pins down ϕ via the transformation from residence location to commuting distance
		@NLexpression(m,  ϕ[ik = 1:K], (-(1 + p.d1 - p.d2 * dϕ[ik]) + sqrt( (1 + p.d1 - p.d2 * dϕ[ik])^2 + 4 * p.d1 * p.d2 * dϕ[ik])) / (2 * p.d1 * p.d2) )  # closed form solution to 2nd order polynomial equation
		@NLexpression(m,  nodes[i = 1:p.int_nodes, ik = 1:K], ϕ[ik] / 2 + ϕ[ik] / 2 * p.inodes[i] ) 
		@NLexpression(m, dnodes[i = 1:p.int_nodes, ik = 1:K], p.d1 * ϕ[ik] + nodes[i,ik] / (1 + p.d2 * ϕ[ik]) )
		@NLexpression(m, τ[i = 1:p.int_nodes,ik = 1:K], (p.a * Lu[ik]^p.μ) * wu0[ik]^(p.ξw) * dnodes[i,ik]^(p.ξl) )
	else
		@NLexpression(m,             ϕ[ik = 1:K], ( (wu0[ik] - wr) / ((p.a * Lu[ik]^p.μ) * wu0[ik]^(p.ξw)) )^(1.0/p.ξl)) 
		@NLexpression(m, nodes[i = 1:p.int_nodes, ik = 1:K], ϕ[ik] / 2 + ϕ[ik] / 2 * p.inodes[i] ) 
		@NLexpression(m, τ[i = 1:p.int_nodes,ik = 1:K], (p.a * Lu[ik]^p.μ) * wu0[ik]^(p.ξw) * nodes[i,ik]^(p.ξl) )
	end
	# @NLconstraint(m, dϕ_con[ik = 1:K], p.d1 * ϕ[ik] + (ϕ[ik] / (1 + p.d2 * ϕ[ik])) == ( (wu0[ik] - wr) / ((p.a * Lu[ik]^p.μ) * wu0[ik]^(p.ξw)) )^(1.0/p.ξl))  

	# approach 2: using wage indifference condition in each city
	# @NLexpression(m, dϕ[ik = 1:K], p.d1 * ϕ[ik] + ϕ[ik] / (1 + p.d2 * ϕ[ik]) )
	# @NLconstraint(m, wr_con[ik = 1:K] , wr == wu0[ik] - (p.a * Lu[ik]^p.μ) * wu0[ik]^(p.ξw) * dϕ[ik]^(p.ξl) )

	# approach 3: like before - get d(phi) from inverse moving cost
	
	# defining nodes, dnodes and commuting cost(dnodes)
	# @NLexpression(m, nodes[i = 1:p.int_nodes, ik = 1:K], ϕ[ik] / 2 + ϕ[ik] / 2 * p.inodes[i] ) 
	# @NLexpression(m, dnodes[i = 1:p.int_nodes, ik = 1:K], p.d1 * ϕ[ik] + nodes[i,ik] / (1 + p.d2 * ϕ[ik]) )
	# @NLexpression(m, τ[i = 1:p.int_nodes,ik = 1:K], (p.a * Lu[ik]^p.μ) * wu0[ik]^(p.ξw) * dnodes[i,ik]^(p.ξl) )


	# expressions indexed at location l in each k

	@NLexpression(m, ϵ[i = 1:p.int_nodes, ik = 1:K], p.ϵr * nodes[i,ik] / ϕ[ik] + p.ϵs * (ϕ[ik] - nodes[i,ik])/ϕ[ik])
	@NLexpression(m, w[i = 1:p.int_nodes,ik = 1:K], wu0[ik] - τ[i,ik] )
	@NLexpression(m,        q[i = 1:p.int_nodes,ik = 1:K], qr * ((w[i,ik] + r_pr_csbar) / xsr)^(1.0/p.γ))
	@NLexpression(m,        H[i = 1:p.int_nodes,ik = 1:K], q[i,ik]^ϵ[i,ik])
	@NLexpression(m,        h[i = 1:p.int_nodes,ik = 1:K], p.γ * (w[i,ik] + r_pr_csbar) / q[i,ik])
	@NLexpression(m,        ρ[i = 1:p.int_nodes,ik = 1:K], (q[i,ik]^(1.0 + ϵ[i,ik])) / (1.0 + ϵ[i,ik]) )
	@NLexpression(m,       cu[i = 1:p.int_nodes,ik = 1:K], (1.0 - p.γ)*(1.0 - p.ν)*(w[i,ik] + r_pr_csbar) - p.sbar)
	@NLexpression(m,        D[i = 1:p.int_nodes,ik = 1:K] , H[i,ik] / h[i,ik])
	@NLexpression(m, cu_input[i = 1:p.int_nodes,ik = 1:K], q[i,ik] * H[i,ik] * ϵ[i,ik] / (1.0+ϵ[i,ik]) )

	# integrals for each region ik
	@NLexpression(m, iDensity[ik = 1:K],  (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * D[i,ik] for i in 1:p.int_nodes))
	@NLexpression(m, iρ[ik = 1:K],        (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * ρ[i,ik] for i in 1:p.int_nodes))
	@NLexpression(m, icu[ik = 1:K],       (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * cu[i,ik] * D[i,ik] for i in 1:p.int_nodes))
	@NLexpression(m, icu_input[ik = 1:K], (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * cu_input[i,ik] for i in 1:p.int_nodes))
	@NLexpression(m, iτ[ik = 1:K],        (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * τ[i,ik] * D[i,ik] for i in 1:p.int_nodes))

	# constraints
	@NLconstraint(m, C.L == sum(Lr[ik] + Lu[ik] for ik in 1:K))   # agg labor market clearing
	@NLconstraint(m, land_clearing[ik = 1:K], C.Sk[ik] == Sr[ik] + ϕ[ik]^2 * π + Srh[ik])   # land market clearing in each region
	@NLconstraint(m, r * C.L == sum(iρ[ik] + ρr * (Sr[ik] + Srh[ik]) for ik in 1:K))   # agg land rent definition
	# input of urban good equal urban good production
	@NLconstraint(m, sum(Lr[ik] * cur + icu[ik] + Srh[ik] * cu_inputr + icu_input[ik] + iτ[ik] for ik in 1:K) == sum(wu0[ik] * Lu[ik] for ik in 1:K))
	@NLconstraint(m, city_size[ik = 1:K], Lu[ik] == iDensity[ik])


	# choose urban productivities so that their population-weighted sum reproduces the data series for average θu
	if estimateθ
		@NLconstraint(m, uwage[ik = 1:K], wu0[ik] >= wr)
		@NLconstraint(m, sum( popwgt[ik] * θu[ik] for ik in 1:K ) == p.θu)   # weighted average productivity is equal to aggregate
		@NLobjective(m, Min, sum( ((Lu[ik] / Lu[1]) - relpop[ik])^2 for ik in 2:K))

	else
		@objective(m, Min, 1.0)  # constant function
	end

	if solve 
		JuMP.optimize!(m) 

		out = (ρr = value(ρr), 
		       ϕ = value.(ϕ), 
			   ℙ = fill(NaN,K), 
			   r = value(r), 
		       Lr = value.(Lr), 
		       Lu = value.(Lu), 
		       LS = value(LS), 
			   pr = value(pr), 
			   Sr = value.(Sr), 
			   θu = estimateθ ? value.(θu) : θu,
			   θr = [pp[ik].θr for ik in 1:K])

		if termination_status(m) == MOI.LOCALLY_SOLVED

			if (p.d1 > 0.0) || (p.d2 > 0.0)
				return (out,value.(ϕ),m)
			else 
				return (out,value.(ϕ),m)
			end
		else		
			println("period = $(p.it)")
			println(termination_status(m))  # error
			println(out)
			error()
		end
		
	else
		# return model, no solving done
		# prepare variable index
		allvars = JuMP.all_variables(m)
		i = Dict(zip(JuMP.name.(allvars),raw_index.(JuMP.index.(allvars))))
		m,i
	end
end





# archive

function jjc()
	m = runm()
	x0 = m[2][1]
	C = country()
	x = Float64[]
	push!(x, x0.Lr / x0.Sr)
	push!(x, x0.r)
	push!(x, x0.pr)
	for ik in 1:C.K
		push!(x,x0.Sr)
	end
	for ik in 1:C.K
		push!(x,x0.Lu)
	end
	jc(C,x)
end


function stmodel2(p::Param)
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

function nocommute(p::Param)

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

