


"""
solve a `HetCountry` at point x0

## keywords:

* `estimateθr`: whether the sequence of ``\\theta_r`` should be a choice variable
* `estimateθu`: whether the sequence of ``\\theta_u`` should be a choice variable
"""
function jc(C::HetCountry,x0::NamedTuple; estimateθr = true, estimateθu = true, lbL = 0.05, constr_viol_tol = nothing, irrelevantλ = true, LUwgt_data = true, ρr_given = nothing, λSr = nothing)

	# println(λSr)
	pp = C.pp   # vector of params for each region
	p  = C.p0   # macro param of entire country : constant parameters across regions. 
	K = C.K
	edata = areaprice_estim(p)

	# setup Model object
	m = JuMP.Model(Ipopt.Optimizer)
	set_optimizer_attribute(m, MOI.Silent(), true)
	if !isnothing(constr_viol_tol)
		set_optimizer_attribute(m, "constr_viol_tol", constr_viol_tol)
	end

	# variables
	@variable(m, r  >=   0.1 * x0.r   , start = x0.r )
	@variable(m, pr >=  0.05 * x0.pr  , start = x0.pr)
	if isnothing(ρr_given)
		@variable(m, 0.1  * x0.Sr[ik] <= Sr[ik = 1:K] <= C.Sk[ik], start = x0.Sr[ik])
	else
		@variable(m, 0.05  * x0.Sr[ik] <= Sr[ik = 1:K] <= C.Sk[ik], start = x0.Sr[ik])
	end
	
	@variable(m, lbL * x0.Lu[ik] <= Lu[ik = 1:K] <= C.L     , start = x0.Lu[ik])
	@variable(m, 0.05 * x0.Lr[ik] <= Lr[ik = 1:K] <= C.L     , start = x0.Lr[ik])


	# theta's are choice variables or not:
	if estimateθu
		@variable(m, θu[ik = 1:K] , start = p.θu)  # everybody starts with aggregate θu
	else
		θu = [pp[ik].θu for ik in 1:K]
	end
	if estimateθr
		@variable(m, θr[ik = 1:K] , start = p.θr)  
	else
		θr = [pp[ik].θr for ik in 1:K]
	end

	# all θs are in p[ik].θ
	σ1 = (p.σ - 1) / p.σ
	σ2 = 1 / (p.σ - 1)

	# price index
	@NLexpression(m, P, (p.ν * pr ^ (1-p.σc) + (1 - p.ν))^(1/(1 - p.σc) ))
	# @NLexpression(m, P, 1.0 )

	# only constant expressions in each country's rural part
	@NLexpression(m, r_pr_csbar, r - pr * p.cbar + p.sbar )

	# indexed by only k
	@NLexpression(m, wr[ik = 1:K] , p.α * pr * θr[ik] * (p.α + (1-p.α)*( Sr[ik] / Lr[ik] )^(σ1))^(σ2) )
	@NLexpression(m, ρr[ik = 1:K] , (1-p.α)* pr * θr[ik] * (p.α * (Lr[ik]/Sr[ik])^(σ1) + (1-p.α))^σ2)
	@NLexpression(m, qr[ik = 1:K] , ((1+p.ϵr) * ρr[ik])^(1.0/(1+p.ϵr)) )

	@NLexpression(m, xsr[ik = 1:K], wr[ik] + r_pr_csbar )
	@NLexpression(m, hr[ik = 1:K], p.γ * xsr[ik] / qr[ik] )
	@NLexpression(m, Hr[ik = 1:K], qr[ik]^p.ϵr )
	@NLexpression(m, cur[ik = 1:K], (1.0 - p.γ)*(1.0 - p.ν) * (1/P)^(1-p.σc) *(wr[ik] + r_pr_csbar) - p.sbar)
	                               
	@NLexpression(m, cu_inputr[ik = 1:K],  (qr[ik]^(1 + p.ϵr)) * p.ϵr / (1.0+p.ϵr) )

	@NLexpression(m, Srh[ik = 1:K], Lr[ik] * hr[ik] / Hr[ik] )   # housing space for rural pop
	@NLexpression(m, wu0[ik = 1:K], θu[ik] * (Lu[ik])^p.λ)  # urban wage in each city center
	@NLexpression(m, LR, sum(Lr[ik] for ik in 1:K)) # aggregate rural labor
	@NLexpression(m, LU, sum(Lu[ik] for ik in 1:K)) # aggregate urban labor


	# getting the fringe of each city

	# approach 1: using equation τ(d(ϕ)) = wu - wr => d(ϕ) = τ^-1(wu - wr)
	# @variable(m, dϕ[ik=1:K] >= 0.0	)
	if (p.d1 > 0) || (p.d2 > 0)
		@NLexpression(m,        dϕ[ik = 1:K], ( abs(wu0[ik] - wr[ik]) / ((p.a * (Lu[ik])^p.μ) * wu0[ik]^(p.ξw)) )^(1.0/p.ξl)) 
		# @NLconstraint(m,  constr_ϕ[ik = 1:K], dϕ[ik] == p.d1 * ϕ[ik] + (ϕ[ik] / (1 + p.d2 * ϕ[ik])) )  # add constraint that pins down ϕ via the transformation from residence location to commuting distance
		@NLexpression(m,  ϕ[ik = 1:K], (-(1 + p.d1 - p.d2 * dϕ[ik]) + sqrt( (1 + p.d1 - p.d2 * dϕ[ik])^2 + 4 * p.d1 * p.d2 * dϕ[ik])) / (2 * p.d1 * p.d2) )  # closed form solution to 2nd order polynomial equation
		@NLexpression(m,  nodes[i = 1:p.int_nodes, ik = 1:K], ϕ[ik] / 2 + ϕ[ik] / 2 * p.inodes[i] ) 
		@NLexpression(m, dnodes[i = 1:p.int_nodes, ik = 1:K], p.d1 * ϕ[ik] + nodes[i,ik] / (1 + p.d2 * ϕ[ik]) )
		@NLexpression(m, τ[i = 1:p.int_nodes,ik = 1:K], (p.a * (Lu[ik])^p.μ) * wu0[ik]^(p.ξw) * dnodes[i,ik]^(p.ξl) )
	else
		@NLexpression(m,             ϕ[ik = 1:K], ( abs(wu0[ik] - wr[ik]) / ((p.a * (Lu[ik])^p.μ) * wu0[ik]^(p.ξw)) )^(1.0/p.ξl)) 
		@NLexpression(m, nodes[i = 1:p.int_nodes, ik = 1:K], ϕ[ik] / 2 + ϕ[ik] / 2 * p.inodes[i] ) 
		@NLexpression(m, τ[i = 1:p.int_nodes,ik = 1:K], (p.a * (Lu[ik])^p.μ) * wu0[ik]^(p.ξw) * nodes[i,ik]^(p.ξl) )
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

	if p.ϵflat 
		@NLexpression(m, ϵ[i = 1:p.int_nodes, ik = 1:K], p.ϵr)
	else
		@NLexpression(m, ϵ[i = 1:p.int_nodes, ik = 1:K], p.ϵr * nodes[i,ik] / ϕ[ik] + p.ϵs * (ϕ[ik] - nodes[i,ik])/ϕ[ik])
	end
	@NLexpression(m, w[i = 1:p.int_nodes,ik = 1:K], wu0[ik] - τ[i,ik] )
	@NLexpression(m,        q[i = 1:p.int_nodes,ik = 1:K], qr[ik] * ((w[i,ik] + r_pr_csbar) / xsr[ik])^(1.0/p.γ))
	@NLexpression(m,        H[i = 1:p.int_nodes,ik = 1:K], q[i,ik]^ϵ[i,ik])
	@NLexpression(m,        h[i = 1:p.int_nodes,ik = 1:K], p.γ * (w[i,ik] + r_pr_csbar) / q[i,ik])
	@NLexpression(m,        ρ[i = 1:p.int_nodes,ik = 1:K], (q[i,ik]^(1.0 + ϵ[i,ik])) / (1.0 + ϵ[i,ik]) )
	@NLexpression(m,       cu[i = 1:p.int_nodes,ik = 1:K], (1.0 - p.γ)*(1.0 - p.ν) * (1/P)^(1-p.σc) * (w[i,ik] + r_pr_csbar) - p.sbar)
	@NLexpression(m,        D[i = 1:p.int_nodes,ik = 1:K] , H[i,ik] / h[i,ik])
	@NLexpression(m, cu_input[i = 1:p.int_nodes,ik = 1:K], q[i,ik] * H[i,ik] * ϵ[i,ik] / (1.0+ϵ[i,ik]) )

	# utility in location 1 (all identical)
	ii = 1
	@NLexpression(m,       cr[ik = 1:K], (1.0 - p.γ) * p.ν   * (pr/P)^(1-p.σc)  *((w[ii,ik] + r_pr_csbar) / pr) + p.cbar)
	
	@NLexpression(m, ℂ[ik = 1:K], (p.ν ^ (1/p.σc) * (cr[ik] - p.cbar)^((p.σc-1)/p.σc) + (1-p.ν) ^ (1/p.σc) * (cu[ii,ik] + p.sbar)^((p.σc-1)/p.σc) )^(p.σc/(p.σc-1)) )

	@NLexpression(m, utility[ik = 1:K], ℂ[ik]^(1-p.γ) * h[ii,ik]^p.γ )

	# integrals for each region ik
	@NLexpression(m, iDensity[ik = 1:K],  (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * D[i,ik] for i in 1:p.int_nodes))
	@NLexpression(m, iρ[ik = 1:K],        (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * ρ[i,ik] for i in 1:p.int_nodes))
	@NLexpression(m, icu[ik = 1:K],       (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * cu[i,ik] * D[i,ik] for i in 1:p.int_nodes))
	@NLexpression(m, icu_input[ik = 1:K], (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * cu_input[i,ik] for i in 1:p.int_nodes))
	@NLexpression(m, iτ[ik = 1:K],        (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * τ[i,ik] * D[i,ik] for i in 1:p.int_nodes))

	# constraints
	# @NLconstraint(m, poscr[ik = 1:K], cr[ik] - p.cbar >= 0)ƒjumps

	@NLconstraint(m, labor_market, C.L == sum(Lr[ik] + Lu[ik] for ik in 1:K))   # agg labor market clearing
	if isnothing(ρr_given)
		@NLconstraint(m, land_clearing[ik = 1:K], C.Sk[ik] == Sr[ik] + ϕ[ik]^2 * π + Srh[ik])   # land market clearing in each region
	else
		@NLconstraint(m, aux_ρ[ik = 1:K], ρr[ik] == ρr_given[ik])
		# additional constraint to help solver pick one solution of many possible:
		@NLconstraint(m, aux_Sr[ik = 1:K], Sr[ik] <= λSr[ik] * sum(Sr[k] for k in 1:K))
	end
	@NLconstraint(m, land_rent, r * C.L == sum(iρ[ik] + ρr[ik] * (Sr[ik] + Srh[ik]) for ik in 1:K))   # agg land rent definition
	# input of urban good equal urban good production
	@NLconstraint(m, urban_good, sum(Lr[ik] * cur[ik] + icu[ik] + Srh[ik] * cu_inputr[ik] + icu_input[ik] + iτ[ik] for ik in 1:K) == sum(wu0[ik] * Lu[ik] for ik in 1:K))
	@NLconstraint(m, city_size[ik = 1:K], Lu[ik] == iDensity[ik])
	# utility is the same everywhere
	@NLconstraint(m, spatial_eq[ik = 2:K], utility[1] == utility[ik])
	@NLconstraint(m, uwage[ik = 1:K], wu0[ik] >= wr[ik])


	# positive consumption
	# @NLconstraint(m, poscu[i = 1:p.int_nodes,ik = 1:K], cu[i,ik] >= 0)


	# choose urban productivities so that their population-weighted sum reproduces the data series for average θu
	
	# use output share Yr[ik] / sum(Yr[ik] all ik) or
	# use employment share Lr[ik] / sum(Lr[ik] all ik)
	if estimateθu && estimateθr
		if irrelevantλ
			# allows for experiment number 1 in sensi-agglo.jl
			if LUwgt_data
				# (default) this takes the urban population shares from data as weights. obj function below implies those are the same in model and data.
				@NLconstraint(m, sum( edata.popwgt[ik] * θu[ik] * (Lu[ik])^p.λ for ik in 1:K ) == p.θu)   # weighted average productivity is equal to aggregate
			else
				# this takes the model-implied urban population shares as weights.
				@NLconstraint(m, sum( (Lu[ik] / LU) * θu[ik] * (Lu[ik])^p.λ for ik in 1:K ) == p.θu)   # weighted average productivity is equal to aggregate
			end
		else
			@NLconstraint(m, sum( (edata.popwgt[ik])^(1 + p.λ) * θu[ik] for ik in 1:K ) == p.θu)   # weighted average productivity is equal to aggregate
		end
		@NLconstraint(m, sum( (Lr[ik] / LR) * θr[ik] for ik in 1:K ) == p.θr) 
  
		@NLobjective(m, Min, 
		      sum( ((Lu[ik] / Lu[1]) - edata.relpop[ik] )^2 for ik in 2:K) + 
			  p.pweight[p.it] * sum( (((ρr[ik] + p.𝕊[p.it,ik]) / (ρr[1] + p.𝕊[p.it,1] )) - edata.s_price_data[ik])^2 for ik in 2:K)
			  )
	elseif estimateθu && !estimateθr
		@NLconstraint(m, sum( edata.popwgt[ik] * θu[ik] for ik in 1:K ) == p.θu)   # weighted average productivity is equal to aggregate
  
		@NLobjective(m, Min, 
		      sum( ((Lu[ik] / Lu[1]) - edata.relpop[ik] )^2 for ik in 2:K) 
			  )
	elseif !estimateθu && !estimateθr
		@objective(m, Min, 1.0)  # constant function
	end

	JuMP.optimize!(m) 
		
	out = (ρr = value.(ρr), 
			ℙ = p.it < length(p.T) - 1 ? value.(ρr) .+ p.𝕊[p.it,:] : fill(NaN, K), 
			ϕ = value.(ϕ), 
			r = value(r), 
			Lr = value.(Lr), 
			Lu = value.(Lu), 
			pr = value(pr), 
			Sr = value.(Sr), 
			θu = estimateθu ? value.(θu) : θu,
			θr = estimateθr ? value.(θr) : θr
			)

	
	# if verbose
	# 	println("error in aggregate θu constraint:")
	# 	println(sum( edata.popwgt[ik] * out.θu[ik] * (out.Lu[ik])^p.λ for ik in 1:K ) - p.θu)
	# end

	if termination_status(m) == MOI.LOCALLY_SOLVED || termination_status(m) == MOI.ALMOST_LOCALLY_SOLVED
		return (out,value.(ϕ),m,edata)
	else		
		println(termination_status(m))
		println("not solved")
		for i in 1:K 
			println("Lu[$i] = $(value(Lu[i]))")
		end
		for i in 1:K 
			println("ϕ[$i] = $(value(ϕ[i]))")
		end
		for i in 1:K 
			println("Lr[$i] = $(value(Lr[i]))")
		end
		for i in 1:K 
			println("Sr[$i] = $(value(Sr[i]))")
		end
		for i in 1:K 

		end
		return m

	end
end

"""
del_idx is the set of regions where we set Lu = 0. 
"""
function jumpsystem(x0::NamedTuple,p::Param,L, θu::Vector, θr::Vector; constr_viol_tol = nothing)

	K = p.K
	Sk = ones(K)  # hard wire total area of each region to 1

	m = JuMP.Model(Ipopt.Optimizer)
	set_optimizer_attribute(m, MOI.Silent(), true)
	if !isnothing(constr_viol_tol)
		set_optimizer_attribute(m, "constr_viol_tol", constr_viol_tol)
	end
	# set_optimizer_attribute(m, "max_iter", 500)
	# lbs = [x0...] .* 0.3

	# @debug "input1" Lu=x0.Lu
	# @debug "input2" Lr=x0.Lr
	# @debug "input3" Sr=x0.Sr
	# @debug "input4" L=L
	# @debug "input5" r=x0.r
	# @debug "input6" pr=x0.pr

	kidx = 2:K
	idx = 1:K # default

	# variables
	@variable(m, r  >= 0.0  , start = x0.r )
	@variable(m, pr >= 0.0  , start = x0.pr)

	@variable(m, 0.0 <= Sr[ik = idx] <= Sk[ik], start = x0.Sr[ik])
	@variable(m, 0.0 <= Lu[ik = idx] <= L     , start = x0.Lu[ik])
	@variable(m, 0.0 <= Lr[ik = idx] <= L     , start = x0.Lr[ik])

	# fixing Lu to zero?



	# all θs are in p[ik].θ
	σ1 = (p.σ - 1) / p.σ
	σ2 = 1 / (p.σ - 1)

	# only constant expressions in each country's rural part
	@NLexpression(m, r_pr_csbar, r - pr * p.cbar + p.sbar )

		# indexed by only k
	@NLexpression(m, wr[ik = idx] , p.α * pr * θr[ik] * (p.α + (1-p.α)*( Sr[ik] / Lr[ik] )^(σ1))^(σ2) )
	@NLexpression(m, ρr[ik = idx] , (1-p.α)* pr * θr[ik] * (p.α * (Lr[ik]/Sr[ik])^(σ1) + (1-p.α))^σ2)
	@NLexpression(m, qr[ik = idx] , ((1+p.ϵr) * ρr[ik])^(1.0/(1+p.ϵr)) )

	@NLexpression(m, xsr[ik = idx], wr[ik] + r_pr_csbar )
	@NLexpression(m, hr[ik = idx], p.γ * xsr[ik] / qr[ik] )
	@NLexpression(m, Hr[ik = idx], qr[ik]^p.ϵr )
	@NLexpression(m, cur[ik = idx], (1.0 - p.γ)*(1.0 - p.ν)*(wr[ik] + r_pr_csbar) - p.sbar)
	                               
	@NLexpression(m, cu_inputr[ik = idx],  (qr[ik]^(1 + p.ϵr)) * p.ϵr / (1.0+p.ϵr) )

	@NLexpression(m, Srh[ik = idx], Lr[ik] * hr[ik] / Hr[ik] )   # housing space for rural pop
	@NLexpression(m, wu0[ik = idx], θu[ik] * (Lu[ik])^p.λ)  # urban wage in each city center
	@NLexpression(m, LR, sum(Lr[ik] for ik in idx)) # aggregate rural labor
	@NLexpression(m, LU, sum(Lu[ik] for ik in idx)) # aggregate urban labor


	# getting the fringe of each city

	# approach 1: using equation τ(d(ϕ)) = wu - wr => d(ϕ) = τ^-1(wu - wr)
	# @variable(m, dϕ[ik=idx] >= 0.0	)
	if (p.d1 > 0) || (p.d2 > 0)
		@NLexpression(m,        dϕ[ik = idx], ( (wu0[ik] - wr[ik]) / ((p.a * (Lu[ik])^p.μ) * wu0[ik]^(p.ξw)) )^(1.0/p.ξl)) 
		# @NLconstraint(m,  constr_ϕ[ik = idx], dϕ[ik] == p.d1 * ϕ[ik] + (ϕ[ik] / (1 + p.d2 * ϕ[ik])) )  # add constraint that pins down ϕ via the transformation from residence location to commuting distance
		@NLexpression(m,  ϕ[ik = idx], (-(1 + p.d1 - p.d2 * dϕ[ik]) + sqrt( (1 + p.d1 - p.d2 * dϕ[ik])^2 + 4 * p.d1 * p.d2 * dϕ[ik])) / (2 * p.d1 * p.d2) )  # closed form solution to 2nd order polynomial equation
		@NLexpression(m,  nodes[i = 1:p.int_nodes, ik = idx], ϕ[ik] / 2 + ϕ[ik] / 2 * p.inodes[i] ) 
		@NLexpression(m, dnodes[i = 1:p.int_nodes, ik = idx], p.d1 * ϕ[ik] + nodes[i,ik] / (1 + p.d2 * ϕ[ik]) )
		@NLexpression(m, τ[i = 1:p.int_nodes,ik = idx], (p.a * (Lu[ik])^p.μ) * wu0[ik]^(p.ξw) * dnodes[i,ik]^(p.ξl) )
	else
		@NLexpression(m,             ϕ[ik = idx], ( abs(wu0[ik] - wr[ik]) / ((p.a * (Lu[ik])^p.μ) * wu0[ik]^(p.ξw)) )^(1.0/p.ξl)) 
		@NLexpression(m, nodes[i = 1:p.int_nodes, ik = idx], ϕ[ik] / 2 + ϕ[ik] / 2 * p.inodes[i] ) 
		@NLexpression(m, τ[i = 1:p.int_nodes,ik = idx], (p.a * (Lu[ik])^p.μ) * wu0[ik]^(p.ξw) * nodes[i,ik]^(p.ξl) )
	end

	# expressions indexed at location l in each k

	if p.ϵflat 
		@NLexpression(m, ϵ[i = 1:p.int_nodes, ik = idx], p.ϵr)
	else
		@NLexpression(m, ϵ[i = 1:p.int_nodes, ik = idx], p.ϵr * nodes[i,ik] / ϕ[ik] + p.ϵs * (ϕ[ik] - nodes[i,ik])/ϕ[ik])
	end
	@NLexpression(m, w[i = 1:p.int_nodes,ik = idx], wu0[ik] - τ[i,ik] )
	@NLexpression(m,        q[i = 1:p.int_nodes,ik = idx], qr[ik] * ((w[i,ik] + r_pr_csbar) / xsr[ik])^(1.0/p.γ))
	@NLexpression(m,        H[i = 1:p.int_nodes,ik = idx], q[i,ik]^ϵ[i,ik])
	@NLexpression(m,        h[i = 1:p.int_nodes,ik = idx], p.γ * (w[i,ik] + r_pr_csbar) / q[i,ik])
	@NLexpression(m,        ρ[i = 1:p.int_nodes,ik = idx], (q[i,ik]^(1.0 + ϵ[i,ik])) / (1.0 + ϵ[i,ik]) )
	@NLexpression(m,       cu[i = 1:p.int_nodes,ik = idx], (1.0 - p.γ)*(1.0 - p.ν)*(w[i,ik] + r_pr_csbar) - p.sbar)
	@NLexpression(m,        D[i = 1:p.int_nodes,ik = idx] , H[i,ik] / h[i,ik])
	@NLexpression(m, cu_input[i = 1:p.int_nodes,ik = idx], q[i,ik] * H[i,ik] * ϵ[i,ik] / (1.0+ϵ[i,ik]) )

	# utility in center
	ii = 1
	@NLexpression(m,       cr[ik = idx], (1.0 - p.γ) * p.ν      *((w[ii,ik] + r_pr_csbar) / pr) + p.cbar)
	@NLexpression(m, utility[ik = idx], (cr[ik] - p.cbar)^(p.ν * (1-p.γ)) * (cu[ii,ik] + p.sbar)^((1-p.ν) * (1-p.γ)) * h[ii,ik]^p.γ )

	# integrals for each region ik
	@NLexpression(m, iDensity[ik = idx],  (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * D[i,ik] for i in 1:p.int_nodes))
	@NLexpression(m, iρ[ik = idx],        (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * ρ[i,ik] for i in 1:p.int_nodes))
	@NLexpression(m, icu[ik = idx],       (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * cu[i,ik] * D[i,ik] for i in 1:p.int_nodes))
	@NLexpression(m, icu_input[ik = idx], (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * cu_input[i,ik] for i in 1:p.int_nodes))
	@NLexpression(m, iτ[ik = idx],        (ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * τ[i,ik] * D[i,ik] for i in 1:p.int_nodes))

	# constraints
	# @NLconstraint(m, poscr[ik = idx], cr[ik] - p.cbar >= 0)

	@NLconstraint(m, L == sum(Lr[ik] + Lu[ik] for ik in idx))   # agg labor market clearing
	@NLconstraint(m, land_clearing[ik = idx], Sk[ik] == Sr[ik] + ϕ[ik]^2 * π + Srh[ik])   # land market clearing in each region
	@NLconstraint(m, r * L == sum(iρ[ik] + ρr[ik] * (Sr[ik] + Srh[ik]) for ik in idx))   # agg land rent definition
	# input of urban good equal urban good production
	@NLconstraint(m, sum(Lr[ik] * cur[ik] + icu[ik] + Srh[ik] * cu_inputr[ik] + icu_input[ik] + iτ[ik] for ik in idx) == sum(wu0[ik] * Lu[ik] for ik in idx))
	@NLconstraint(m, city_size[ik = idx], Lu[ik] == iDensity[ik])
	# utility is the same everywhere
	# if relax > 0
	# 	@NLconstraint(m, spatial_eq[ik = kidx], abs(utility[1] - utility[ik]) <= relax)  # kidx is 2:K by default
	# else
	@NLconstraint(m, spatial_eq[ik = kidx], utility[1] == utility[ik])  # kidx is 2:K by default
	# end
	@NLconstraint(m, uwage[ik = idx], wu0[ik] >= wr[ik])
	
	@objective(m, Min, 1.0)  # constant function

	JuMP.optimize!(m) 	

	out = (ρr = value.(ρr), 
			ℙ = value.(ρr), 
			ϕ = value.(ϕ), 
			r = value(r), 
			Lr = value.(Lr), 
			Lu = value.(Lu), 
			pr = value(pr), 
			Sr = value.(Sr), 
			θu = θu,
			θr = θr, 
			U = value.(utility)
			)

	if termination_status(m) == MOI.LOCALLY_SOLVED || termination_status(m) == MOI.ALMOST_LOCALLY_SOLVED
		return (out,value.(ϕ),m)

	else		
		println(termination_status(m))  # error
		@debug "error1" Lu=value.(Lu) 
		@debug "error2" ϕ=value.(ϕ)
		allvars = JuMP.all_variables(m)
		i = Dict(zip(JuMP.name.(allvars),raw_index.(JuMP.index.(allvars))))
		(m,)
	end
end




function x2nt(x::Vector,K::Int)
	(Sr = x[1:K],
	Lu = x[(1+K):(2K)],
	Lr = x[(1+2K):(3K)],
	r = x[3K + 1],
	pr = x[3K + 2])
end

function nt2x(x::NamedTuple)
	[x.Sr...,
	 x.Lu...,
	 x.Lr...,
	 x.r,
	 x.pr]
end

"""
	system!(x::Vector,p::Param,L, θu, θr)

unconstrained system of equations for `K=20` city model in period `t`

## Arguments

* `x` vector of choice variables
* `p` Param object holding parameter values 
* `L` Aggregate (country-wide) population in period `t`
* `θu` K-vector with urban productivities
* `θr` K-vector with rural productivities

`x` is vector of choice variables:
1. Sr[K] 
2. Lu[K]
3. Lr[K]
4. r
5. pr
"""
function system!(out::Vector,x::Vector,p::Param,L, θu, θr)

	# if any(x .< 0)

	# 	error("negative x")
	# end

		K = 20
		Sk = ones(K)  # hard wire total area of each region to 1
		
		Sr, Lu, Lr, r, pr = x2nt(x,K)

		# @debug "inputs" Lu = Lu

		@assert length(θu) == K
		@assert length(θr) == K

		# simplify some exponents
		σ1 = (p.σ - 1) / p.σ
		σ2 = 1 / (p.σ - 1)

		# price index
		P = (p.ν * real(Complex(pr) ^ (1-p.σc)) + (1 - p.ν))^(1/(1 - p.σc) )


		# constant expression in each country's rural part
		r_pr_csbar = r - pr * p.cbar + p.sbar 

		# indexed by only k
		wr = [p.α * pr * θr[ik] * real(p.α + (1-p.α)*Complex( Sr[ik] / Lr[ik] )^(σ1))^(σ2) for ik in 1:K]
		ρr = [(1-p.α)* pr * θr[ik] * real(p.α * Complex(Lr[ik]/Sr[ik])^(σ1) + (1-p.α))^σ2 for ik in 1:K]
		qr = [real(Complex((1+p.ϵr) * ρr[ik])^(1.0/(1+p.ϵr)) ) for ik in 1:K]

		xsr = [wr[ik] + r_pr_csbar for ik in 1:K]
		hr  = [p.γ * xsr[ik] / qr[ik] for ik in 1:K]
		Hr  = [(qr[ik])^p.ϵr for ik in 1:K]
		cur = [(1.0 - p.γ)*(1.0 - p.ν) * (1/P)^(1-p.σc) *(wr[ik] + r_pr_csbar) - p.sbar for ik in 1:K]
					
		cu_inputr = [((qr[ik])^(1 + p.ϵr)) * p.ϵr / (1.0+p.ϵr) for ik in 1:K]

		Srh =  [Lr[ik] * hr[ik] / Hr[ik]  for ik in 1:K]   # housing space for rural pop
		wu0 =  [θu[ik] * (Lu[ik] )^p.λ for ik in 1:K]  # urban wage in each city center

		# getting the fringe of each city

		# approach 1: using equation τ(d(ϕ)) = wu - wr => d(ϕ) = τ^-1(wu - wr)
		# @variable(m, dϕ[ik=1:K] >= 0.0	)
		if (p.d1 > 0) || (p.d2 > 0)
			dϕ = [( (wu0[ik] - wr[ik]) / ((p.a * (Lu[ik] )^p.μ) * wu0[ik]^(p.ξw)) )^(1.0/p.ξl) for ik in 1:K]
			# @NLconstraint(m,  constr_ϕ[ik = 1:K], dϕ[ik] == p.d1 * ϕ[ik] + (ϕ[ik] / (1 + p.d2 * ϕ[ik])) )  # add constraint that pins down ϕ via the transformation from residence location to commuting distance
			ϕ = [(-(1 + p.d1 - p.d2 * dϕ[ik]) + sqrt( (1 + p.d1 - p.d2 * dϕ[ik])^2 + 4 * p.d1 * p.d2 * dϕ[ik])) / (2 * p.d1 * p.d2) for ik in 1:K] #closed form solution to 2nd order polynomial equation
			nodes = [ϕ[ik] / 2 + ϕ[ik] / 2 * p.inodes[i] for i = 1:p.int_nodes, ik = 1:K ]
			dnodes = [p.d1 * ϕ[ik] + nodes[i,ik] / (1 + p.d2 * ϕ[ik]) for i = 1:p.int_nodes, ik = 1:K ]
			τ = [(p.a * (Lu[ik] )^p.μ) * wu0[ik]^(p.ξw) * dnodes[i,ik]^(p.ξl) for i = 1:p.int_nodes,ik = 1:K]
		else
			ϕ = [( abs(wu0[ik] - wr[ik]) / ((p.a * (Lu[ik] )^p.μ) * wu0[ik]^(p.ξw)) )^(1.0/p.ξl) for ik in 1:K]
			nodes = [ϕ[ik] / 2 + ϕ[ik] / 2 * p.inodes[i]  for i = 1:p.int_nodes, ik = 1:K]
			τ = [(p.a * (Lu[ik] )^p.μ) * wu0[ik]^(p.ξw) * nodes[i,ik]^(p.ξl) for i = 1:p.int_nodes, ik = 1:K]
		end

		# expressions indexed at location l in each k

		if p.ϵflat 
			ϵ = [p.ϵr for i = 1:p.int_nodes, ik = 1:K]
		else
			ϵ = [p.ϵr * nodes[i,ik] / ϕ[ik] + p.ϵs * (ϕ[ik] - nodes[i,ik])/ϕ[ik] for i = 1:p.int_nodes, ik = 1:K]
		end
		w = [wu0[ik] - τ[i,ik] for i = 1:p.int_nodes,ik = 1:K]
		q = [qr[ik] * real(Complex((w[i,ik] + r_pr_csbar) / xsr[ik])^(1.0/p.γ)) for i = 1:p.int_nodes,ik = 1:K]
		H = real.([Complex(q[i,ik])^ϵ[i,ik] for i = 1:p.int_nodes,ik = 1:K])
		h = [p.γ * (w[i,ik] + r_pr_csbar) / q[i,ik] for i = 1:p.int_nodes,ik = 1:K]
		ρ = [real(Complex(q[i,ik])^(1.0 + ϵ[i,ik])) / (1.0 + ϵ[i,ik]) for i = 1:p.int_nodes,ik = 1:K]
		cu = [(1.0 - p.γ)*(1.0 - p.ν) * (1/P)^(1-p.σc) *(w[i,ik] + r_pr_csbar) - p.sbar for i = 1:p.int_nodes,ik = 1:K]
		D = [H[i,ik] / h[i,ik] for i = 1:p.int_nodes,ik = 1:K]
		cu_input = [q[i,ik] * H[i,ik] * ϵ[i,ik] / (1.0+ϵ[i,ik]) for i = 1:p.int_nodes,ik = 1:K]

		# utility in center
		ii = 1
		cr = [(1.0 - p.γ) * p.ν   * (1/P)^(1-p.σc)   *((w[ii,ik] + r_pr_csbar) / pr) + p.cbar for ik = 1:K]
		ℂ = [(p.ν ^ (1/p.σc) * real(Complex(cr[ik] - p.cbar)^((p.σc-1)/p.σc)) + (1-p.ν) ^ (1/p.σc) * Complex(cu[ii,ik] + p.sbar)^((p.σc-1)/p.σc) )^(p.σc/(p.σc-1)) for ik = 1:K]

		#utility
		utility = real.([Complex(ℂ[ik])^(1-p.γ) * Complex(h[ii,ik])^p.γ for ik in 1:K])

		# integrals for each region ik
		iDensity= [(ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * D[i,ik] for i in 1:p.int_nodes) for ik = 1:K]
		iρ = [(ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * ρ[i,ik] for i in 1:p.int_nodes) for ik = 1:K]
		icu = [(ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * cu[i,ik] * D[i,ik] for i in 1:p.int_nodes) for ik = 1:K]
		icu_input = [(ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * cu_input[i,ik] for i in 1:p.int_nodes) for ik = 1:K]
		iτ = [(ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * τ[i,ik] * D[i,ik] for i in 1:p.int_nodes) for ik = 1:K]

		
		# define actual system of equations

		out[1] = L - sum(Lr[ik] + Lu[ik] for ik in 1:K)   # agg labor market clearing

		# urban goods market clearing
		out[2] = sum(Lr[ik] * cur[ik] + icu[ik] + Srh[ik] * cu_inputr[ik] + icu_input[ik] + iτ[ik] for ik in 1:K) -
			sum(wu0[ik] * Lu[ik] for ik in 1:K)

		# agg land rent definition
		out[3]  = r * L - sum(iρ[ik] + ρr[ik] * (Sr[ik] + Srh[ik]) for ik in 1:K)

		# equations for each region:
		for ik in 1:K 
			out[3 + ik] = Sk[ik] - (Sr[ik] + ϕ[ik]^2 * π + Srh[ik]) # land market clearing in region ik
		end
		for ik in 1:K 
			out[3 + K + ik] =  Lu[ik] - iDensity[ik]  # city size
		end
		for ik in 2:K
			out[3 + 2K + (ik-1)] = utility[1] - utility[ik]  # utility is the same everywhere
		end

		out[:] = real.(out)

		# return useful values, even though nlsolve only uses stuff in out

		(ρr = ρr, 
		ℙ = ρr .+ p.𝕊[p.it,:] , 
		ϕ = ϕ, 
		r = r, 
		Lr = Lr, 
		Lu = Lu, 
		pr = pr, 
		Sr = Sr, 
		θu = θu,
		θr = θr,
		iDensity = iDensity,
		nodes = nodes)
	
end


"""
	systemρ!(x::Vector,p::Param,L, θu, θr, ρ)

same as [`system!`](@ref) but where ρ is given and no land market clearing takes place
"""
function systemρ!(out::Vector,x::Vector,p::Param,L::Float64, θu::Vector, θr::Vector, ρr_in::Vector)

	out[:] .= 0.0
	# if any(x .< 0)

	# 	error("negative x")
	# end

		K = length(θu)
		Sk = ones(K)  # hard wire total area of each region to 1
		
		Sr, Lu, Lr, r, pr = x2nt(x,K)
		# println("trying:")
		# println(Lu)
		# println(Lr)

		# @debug "inputs" Lu = Lu

		@assert length(θu) == K
		@assert length(θr) == K

		# simplify some exponents
		σ1 = (p.σ - 1) / p.σ
		σ2 = 1 / (p.σ - 1)

		# price index
		P = (p.ν * ((pr) ^ (1-p.σc)) + (1 - p.ν))^(1/(1 - p.σc) )


		# constant expression in each country's rural part
		r_pr_csbar = r - pr * p.cbar + p.sbar 

		# indexed by only k
		# println([( Sr[ik] / Lr[ik] ) for ik in 1:K])
		# println([( Sr[ik] / Lr[ik] )^(σ1) for ik in 1:K])
		# println([(p.α + (1-p.α)*( Sr[ik] / Lr[ik] )^(σ1))^(σ2) for ik in 1:K])
		wr = [p.α * pr * θr[ik] * (p.α + (1-p.α)*( Sr[ik] / Lr[ik] )^(σ1))^(σ2) for ik in 1:K]
		ρr = [(1-p.α)* pr * θr[ik] * (p.α * (Lr[ik]/Sr[ik])^(σ1) + (1-p.α))^σ2 for ik in 1:K]
		qr = [((1+p.ϵr) * ρr[ik])^(1.0/(1+p.ϵr))  for ik in 1:K]

		xsr = [wr[ik] + r_pr_csbar for ik in 1:K]
		hr  = [p.γ * xsr[ik] / qr[ik] for ik in 1:K]
		Hr  = [(qr[ik])^p.ϵr for ik in 1:K]
		cur = [(1.0 - p.γ)*(1.0 - p.ν) * (1/P)^(1-p.σc) *(wr[ik] + r_pr_csbar) - p.sbar for ik in 1:K]
					
		cu_inputr = [((qr[ik])^(1 + p.ϵr)) * p.ϵr / (1.0+p.ϵr) for ik in 1:K]

		Srh =  [Lr[ik] * hr[ik] / Hr[ik]  for ik in 1:K]   # housing space for rural pop
		wu0 =  [θu[ik] * (Lu[ik] )^p.λ for ik in 1:K]  # urban wage in each city center

		# getting the fringe of each city

		# approach 1: using equation τ(d(ϕ)) = wu - wr => d(ϕ) = τ^-1(wu - wr)
		# @variable(m, dϕ[ik=1:K] >= 0.0	)
		if (p.d1 > 0) || (p.d2 > 0)
			dϕ = [( (wu0[ik] - wr[ik]) / ((p.a * (Lu[ik] )^p.μ) * wu0[ik]^(p.ξw)) )^(1.0/p.ξl) for ik in 1:K]
			# @NLconstraint(m,  constr_ϕ[ik = 1:K], dϕ[ik] == p.d1 * ϕ[ik] + (ϕ[ik] / (1 + p.d2 * ϕ[ik])) )  # add constraint that pins down ϕ via the transformation from residence location to commuting distance
			ϕ = [(-(1 + p.d1 - p.d2 * dϕ[ik]) + sqrt( (1 + p.d1 - p.d2 * dϕ[ik])^2 + 4 * p.d1 * p.d2 * dϕ[ik])) / (2 * p.d1 * p.d2) for ik in 1:K] #closed form solution to 2nd order polynomial equation
			nodes = [ϕ[ik] / 2 + ϕ[ik] / 2 * p.inodes[i] for i = 1:p.int_nodes, ik = 1:K ]
			dnodes = [p.d1 * ϕ[ik] + nodes[i,ik] / (1 + p.d2 * ϕ[ik]) for i = 1:p.int_nodes, ik = 1:K ]
			τ = [(p.a * (Lu[ik] )^p.μ) * wu0[ik]^(p.ξw) * dnodes[i,ik]^(p.ξl) for i = 1:p.int_nodes,ik = 1:K]
		else
			ϕ = [( abs(wu0[ik] - wr[ik]) / ((p.a * (Lu[ik] )^p.μ) * wu0[ik]^(p.ξw)) )^(1.0/p.ξl) for ik in 1:K]
			nodes = [ϕ[ik] / 2 + ϕ[ik] / 2 * p.inodes[i]  for i = 1:p.int_nodes, ik = 1:K]
			τ = [(p.a * (Lu[ik] )^p.μ) * wu0[ik]^(p.ξw) * nodes[i,ik]^(p.ξl) for i = 1:p.int_nodes, ik = 1:K]
		end

		# expressions indexed at location l in each k

		if p.ϵflat 
			ϵ = [p.ϵr for i = 1:p.int_nodes, ik = 1:K]
		else
			ϵ = [p.ϵr * nodes[i,ik] / ϕ[ik] + p.ϵs * (ϕ[ik] - nodes[i,ik])/ϕ[ik] for i = 1:p.int_nodes, ik = 1:K]
		end
		w = [wu0[ik] - τ[i,ik] for i = 1:p.int_nodes,ik = 1:K]
		q = [qr[ik] * ((w[i,ik] + r_pr_csbar) / xsr[ik])^(1.0/p.γ) for i = 1:p.int_nodes,ik = 1:K]
		H = [q[i,ik]^ϵ[i,ik] for i = 1:p.int_nodes,ik = 1:K]
		h = [p.γ * (w[i,ik] + r_pr_csbar) / q[i,ik] for i = 1:p.int_nodes,ik = 1:K]
		ρ = [(q[i,ik])^(1.0 + ϵ[i,ik]) / (1.0 + ϵ[i,ik]) for i = 1:p.int_nodes,ik = 1:K]
		cu = [(1.0 - p.γ)*(1.0 - p.ν) * (1/P)^(1-p.σc) *(w[i,ik] + r_pr_csbar) - p.sbar for i = 1:p.int_nodes,ik = 1:K]
		D = [H[i,ik] / h[i,ik] for i = 1:p.int_nodes,ik = 1:K]
		cu_input = [q[i,ik] * H[i,ik] * ϵ[i,ik] / (1.0+ϵ[i,ik]) for i = 1:p.int_nodes,ik = 1:K]

		# utility in center
		ii = 1
		cr = [(1.0 - p.γ) * p.ν   * (1/P)^(1-p.σc)   *((w[ii,ik] + r_pr_csbar) / pr) + p.cbar for ik = 1:K]
		ℂ = [(p.ν ^ (1/p.σc) * (cr[ik] - p.cbar)^((p.σc-1)/p.σc)) + (1-p.ν) ^ (1/p.σc) * (cu[ii,ik] + p.sbar)^((p.σc-1)/p.σc)^(p.σc/(p.σc-1)) for ik = 1:K]

		#utility
		utility = [(ℂ[ik])^(1-p.γ) * (h[ii,ik])^p.γ for ik in 1:K]

		# integrals for each region ik
		iDensity= [(ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * D[i,ik] for i in 1:p.int_nodes) for ik = 1:K]
		iρ = [(ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * ρ[i,ik] for i in 1:p.int_nodes) for ik = 1:K]
		icu = [(ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * cu[i,ik] * D[i,ik] for i in 1:p.int_nodes) for ik = 1:K]
		icu_input = [(ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * cu_input[i,ik] for i in 1:p.int_nodes) for ik = 1:K]
		iτ = [(ϕ[ik]/2) * sum(p.iweights[i] * 2π * nodes[i,ik] * τ[i,ik] * D[i,ik] for i in 1:p.int_nodes) for ik = 1:K]

		
		# define actual system of equations

		out[1] = L - sum(Lr[ik] + Lu[ik] for ik in 1:K)   # agg labor market clearing

		# urban goods market clearing
		out[2] = sum(Lr[ik] * cur[ik] + icu[ik] + Srh[ik] * cu_inputr[ik] + icu_input[ik] + iτ[ik] for ik in 1:K) -
			sum(wu0[ik] * Lu[ik] for ik in 1:K)

		# agg land rent definition
		out[3]  = r * L - sum(iρ[ik] + ρr[ik] * (Sr[ik] + Srh[ik]) for ik in 1:K)

		# equations for each region:
		for ik in 1:K 
			out[3 + ik] = ρr[ik] - ρr_in[ik]  # auxiliary constraint
		end
		for ik in 1:K 
			out[3 + K + ik] =  Lu[ik] - iDensity[ik]  # city size
			# println("value of city size constraint ik = $ik")
			# println(out[3 + K + ik])
		end
		for ik in 2:K
			out[3 + 2K + (ik-1)] = utility[1] - utility[ik]  # utility is the same everywhere
		end

		# out[:] = real.(out)
		# println(out)

		# return useful values, even though nlsolve only uses stuff in out

		(ρr = ρr, 
		ℙ = ρr .+ p.𝕊[p.it,:] , 
		ϕ = ϕ, 
		r = r, 
		Lr = Lr, 
		Lu = Lu, 
		pr = pr, 
		Sr = Sr, 
		θu = θu,
		θr = θr,
		iDensity = iDensity,
		nodes = nodes)
	
end


function x2ntρ(x::Vector,K::Int)
	(Sr = x[1:K],
	Lu = x[(1+K):(2K)],
	Lr = x[(1+2K):(3K)],
	r = x[3K + 1],
	pr = x[3K + 2],
	ρr = x[(3K + 3):(4K + 2)])
end

function nt2xρ(x::NamedTuple)
	[x.Sr...,
	 x.Lu...,
	 x.Lr...,
	 x.r,
	 x.pr,
	 x.ρr...]
end


fr(x) = primal_feasibility_report(x) do m
	value(m)
end

function printm(m)
	for (k,v) in filter(x -> !(x.first ∈ [:LU,:LR,:D,:iDensity,:Hr, :hr, :P,:r_pr_csbar]), m.obj_dict) 
		println("key: $k")
		println(value.(v))
	end

end
