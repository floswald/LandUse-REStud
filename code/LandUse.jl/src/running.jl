
"""
	change_reporter(x,y)

Report various notions of *change*
"""
function change_reporter(x1,x2)
	Dict(
		:log => log(x1 / x2),
		:perc => (x1 - x2) / x1,
		:ratio => x1 / x2
	)
end


"""
	text_reporter()

Function prints statistics from model to screen
together with text snippets that appear in the main text.
"""
function text_reporter()
	b = khet_run(readdisk = true)
	a = aggregator(b)

	a2020 = @subset(a, :year .== 2020)
	a1870 = @subset(a, :year .== 1870)

	claim("p. 28: density is divided by almost 6 since 1870.")

	pr = combine(a, [:mean_citydensity,:year] => ((x,t) -> xdivided_at_t(x,t,tstart = 1870, tend = 2020)) => :density_fall_factor)
	proof("density is divided by $(round(pr.density_fall_factor[1],digits = 2))")

	claim("p. 30: Our model predicts that the overall fall in the central density is about 70%
	of the fall in the average density")

	pr = combine(a, 
		[:mean_citydensity_1840,:mean_d0_1840, :year] => ((x,y,t) -> y[t .== 2020 ] ./ x[t .== 2020 ] ) => :density_fall_factor)
	bd = change_reporter(1, a2020.mean_citydensity_1840[1])
	bc = change_reporter(1, a2020.mean_d0_1840[1])

	proof("log(centraldens(1840) / centraldens(2020)) / log(meandens(1840) / meandens(2020)) is $(round(bc[:log] / bd[:log],digits = 3))")
	# proof("1840-normalized mean_citydensity / 1840-normalized d0 is $(round(pr.density_fall_factor[1],digits = 2))")


	claim("p. 30: Over time, our model generates almost a five-fold rise in the average commuting speed")
	pr = combine(a, [:mean_commuting_speed,:year] => ((x,t) -> xdivided_at_t(x,t,tstart = 2020, tend = 1840)) => :speed_increase)
	proof("average commuting speed increases by $(round(pr.speed_increase[1],digits = 2))")

	claim("p. 32: In the model, while the value of agricultural land constituted more than 80% of the total land value, it is about
	10% nowaday")
	w1840 = select(subset(a, :year => ByRow(==(1840))),:𝕎lrshare)
	w2020 = select(subset(a, :year => ByRow(==(2020))),:𝕎lrshare)

	proof("Land share 1840 = $(round(w1840.𝕎lrshare[1],digits = 3)), Land share 2020 = $(round(w2020.𝕎lrshare[1],digits = 4))")

	claim("p. 32: The model generates more than half of the increase
	in housing prices described in Knoll et al. (2017) for France (1870-2013): 5.3141837")
	pr = combine(a, [:mean_HPI,:year] => ((x,t) -> xdivided_at_t(x,t,tstart = 2010, tend = 1840)) => :hpi)
	pr70 = combine(a, [:mean_HPI,:year] => ((x,t) -> xdivided_at_t(x,t,tstart = 2010, tend = 1870)) => :hpi)

	proof("Average HPI increase (1840-2010) = $(round(pr.hpi[1],digits = 4)), i.e. $(round(pr.hpi[1] / 5.31,digits = 4)) of Knoll. 
	Average HPI increase (1870-2010) = $(round(pr70.hpi[1],digits = 4)), i.e. $(round(pr70.hpi[1] / 5.31,digits = 4)) of Knoll
	")

	claim("p. 39: This severely limits the sprawl of the city and the fall of the average urban density (Figure 17b)— the counterfactual change in (log) average urban density being 30% of the baseline and about 24% of the data since 1870")

	xi = OrderedDict( k => @chain aggregator(v) begin
				subset(:year => leq(2020))
			end 
		for (k,v) in sensitivity_ξ(save = false, readdisk = true, returnplots = false))

	xi1870 = @subset(xi[Symbol("ξw=1")], :year .== 1870)
	xi2020 = @subset(xi[Symbol("ξw=1")], :year .== 2020)

	xi_logchange   = log(xi2020.mean_citydensity[1] / xi1870.mean_citydensity[1])
	bs_logchange   = log(a2020.mean_citydensity[1] / a1870.mean_citydensity[1])
	data_logchange = log(@subset(agg_pop_density(), :year .== 2015).agg_density[1])
	
	proof("
	log Δ(2020/1870) baseline: $(round(bs_logchange,digits = 2))
	log Δ(2020/1870) ξw=1: $(round(xi_logchange,digits = 2))
	log Δ(2020/1870) data: $(round(data_logchange,digits = 2))

	The ratios of changes are thus:
	ξw=1 / baseline : $(round(xi_logchange / bs_logchange, digits = 2))
	ξw=1 / data     : $(round(xi_logchange / data_logchange, digits = 2))")
	
	claim("""
	p.41: the counterfactual increase of the farmland price is an order of magnitude larger for the interim period relative to the other ones—an increase above 300% in 1970 to keep farmland prices over income constant over 1920-1970, close to 5 times (resp. 15 times) larger than the increase in 2020 (resp. 1920).
	""")

	frho = fixed_ρr_setup(fifty_years = true)
	frho_results = fixed_ρr_results(readdisk = true)
	frho_su = fixed_ρr_summary(frho_results)

	# proof("This table shows the baseline values for average land price, GDP (Y) per capita and resulting ratio ρr_Y_pc. The task is the find a value ρr' s.t. in year t+1, the ratio ρr'/Y_pc(t+1) = mean_ρr_Y_pc(t)")
	# pretty_table(select(frho[1],:year, :mean_ρr,:GDP_agg_pc,:mean_ρr_GDP_pc))
	proof("The resulting required increase in ρr is shown here:")
	pretty_table(select(frho[2], :years,"ρr_GDP_pc_t-1" => "Counterfactual Price Change (%)"))

	claim("""
	p.41: For 2020, we find that a 10% exogenous increase in the rental price at the urban fringe of all regions increases urban density by about 3% on average—an elasticity close to the cross-sectional one (Table 2). The same elasticity is also close to 1/3 for earlier dates. 
	""")

	proof("""
	The average density increase from a 1% exogenous rise in ρr can be seen in the last column of this table:
	""")

	pretty_table(select(frho_su[:elast_10],:year,:elasticity_of_means => :elasticity))

	claim("""
	p.41: While this mechanism makes up for about 75% of the decline over 1870-1920, our counterfactual experiments suggest that it plays less of a role in the later periods—still representing a significant share, about 30% (34% over 1920-1970 and 26% over 1970-2020)
	""")

	proof("""
	All of the results for the fixed ρr counterfactual can be obtained by running
		
		```
		frho = fixed_ρr_setup(fifty_years = true)
		frho_results = fixed_ρr_results(readdisk = true)
		frho_su = fixed_ρr_summary(frho)
		```
	the result in the claim is in `frho_su[:headline]` (last column):
	
	""")
	pretty_table(select(frho_su[:headline], :years, :share_explained_by_falling_ρr_y => "Share explained"))

	@info "text reporter done."
end

claim(s) = @info "PAPER CLAIM: $s"
function proof(s) 
	@info "MODEL PROOF: $s"
	println()
end



"""
	export_baseline(; save = true)

Reads the baseline model from disk and exports plots and generated
output data as csv to disk. also writes the baseline parameter to 
a tex file.
"""
function export_baseline(; readdisk = false, save = true)


	b = khet_run(readdisk = readdisk, writedisk = save)
	p = plot_het(b, save = save, doreg = true)
	
	# export data
	if save
		prepare_data(writedisk = save)
		export_baseline_csv()
		export_baseline_interpolated_csv()
		export_params()
		plot_areapricedata()
		# save aggregation
		export_aggregation(b)
	end
	p
end

function export_aggregation(d::DataFrame; fpath = joinpath(dbtables(), "baseline-aggregated.csv"))
	da = @chain d begin
		subset(:year => x -> x .<= 2020)
		aggregator(_)
	end
	on = names(da)
	rename!(da, replace.(on, "ρ" => "rho", "θ" => "theta", "τ" => "tau", "𝕎" => "wealth"))
	CSV.write(fpath, da) 
end

function export_all_thetas()
	d1d2 = sensitivity_d1d2(returnplots = false)
	lowr = sensitivity_θr(returnplots = false)
	aggl = sensitivity_agglo(returnplots = false)

	b = @chain d1d2[:Baseline] begin
		subset(:year => leq(2020))
		select(:year, :LIBGEO, :θr => :θr_baseline, :θu => :θu_baseline)
	end

	d1d2_ = @chain d1d2[Symbol("d0=0.05, d1=2.0")] begin
		subset(:year => leq(2020))
		select(:year, :LIBGEO, :θr => :θr_d1d2, :θu => :θu_d1d2)
	end

	lowr4 = @chain lowr[1][Symbol("4.0%")] begin
		subset(:year => leq(2020))
		select(:year, :LIBGEO, :θr => :θr_lowr4, :θu => :θu_lowr4)
	end

	lowr20 = @chain lowr[1][Symbol("20.0%")] begin
		subset(:year => leq(2020))
		select(:year, :LIBGEO, :θr => :θr_lowr20, :θu => :θu_lowr20)
	end

	agglo1 = @chain aggl[:Agglomeration] begin
		subset(:year => leq(2020))
		select(:year, :LIBGEO, :θr => :θr_agglo1, :θu => :θu_agglo1)
	end

	agglo2 = @chain aggl[Symbol("Aggregate Effect")] begin
		subset(:year => leq(2020))
		select(:year, :LIBGEO, :θr => :θr_agglo2, :θu => :θu_agglo2)
	end
	cong = @chain aggl[:Congestion] begin
		subset(:year => leq(2020))
		select(:year, :LIBGEO, :θr => :θr_congest, :θu => :θu_congest)
	end

	bx = leftjoin(b,d1d2_, on = [:year, :LIBGEO])
	bx = leftjoin(bx,lowr4, on = [:year, :LIBGEO])
	bx = leftjoin(bx,lowr20, on = [:year, :LIBGEO])
	bx = leftjoin(bx,agglo1, on = [:year, :LIBGEO])
	bx = leftjoin(bx,agglo2, on = [:year, :LIBGEO])
	bx = leftjoin(bx,cong, on = [:year, :LIBGEO])

	CSV.write(joinpath(dbtables(),"all_thetas.csv"),bx)
	
	bx


end


function run_extensions(; save = false, readdisk = false)
	@info "runs all extensions $(ifelse(!readdisk,"; writes results to disk. This is going to take some time. ☕️ 🍜 🍔","; reads results from disk 💾 ⚡️⚡️"))"

	app_numillustration(overwrite = true, save = true)
	app_numillustration_sbar(overwrite = true, save = true)

	sensitivity_agglo(readdisk = readdisk, save = save)
	sensitivity_ξ(readdisk = readdisk, save = save)
	sensitivity_d1d2(readdisk = readdisk, save = save)
	sensitivity_ϵ(readdisk = readdisk, save = save)
	sensitivity_R3_θfixed(readdisk = readdisk, save = save)
	sensitivity_θr(readdisk = readdisk, save = save)
	sensitivity_nopopgrowth(readdisk = readdisk, save = save)
	sensitivity_σ(readdisk = readdisk, save = save)
	sensitivity_ω(readdisk = readdisk, save = save)
	sensitivity_fixedρ(readdisk = readdisk)

	@info "extensions done! ✅"
end


function run_all(; readdisk = false)
	@info "runs all model computations $(ifelse(!readdisk,"; writes results to disk. This is going to take some time. ☕️ 🍜 🍔","; reads results from disk 💾 ⚡️⚡️"))"

	export_baseline(readdisk = readdisk, save = true)
	run_extensions(readdisk = readdisk)

	@info "all done. ✅✅"

end


"The Only Function You'll Ever Need"
function full_pipeline( )

	@info """

		Welcome to LandUse.jl - This will create all model output

		We make the following assumptions: 📝

		1. Call `pkgroot` the root of this replication package.
		2. You successfully executed `pkgroot/code/stata/replication_aggregate_main.do`
		3. You successfully installed the `pkgroot/code/LandUseR/` package and executed function `LandUseR::run_data()`
		4. We call the values in `pkgroot/code/LandUse.jl/src/params.json` the *optimal parameter value*. 
		We obtained those values by running the estimation algorithm on the S-CAPAD cluster and takes those as given now.

		👉 All good? Then let's roll!
		
		"""

	time_taken = @elapsed begin
		# 1. create the input data for the model
		# not strictly necessary, but we demonstrate that we overwrite what is in in/tables with this step
		try
			mkpath.([nsplots(),dbplots(),dboutdata(),dbdataplots(),dbtables()])
			prepare_data(writedisk = true)
			citysample_comparison()
			plot_areapricedata()

			b = khet_run(readdisk = false, writedisk = true) # write baseline model results to out/khet-20-baseline.csv, so we can use it later in the counterfactuals.
			p = plot_het(b, save = true, doreg = true)  # make all baseline plots and save to output folder

			# print tables
			latex_param_alt()
			latex_param()
			print_latex_moments()

			run_extensions(readdisk = false, save = true)

			map_pkg_names_to_paper()
			
		catch e
			rm(joinpath(dbplots(),"revision$(revision())"), force = true, recursive = true)
			throw(e)
		end	
	end
		
	@info "all done in $(round(time_taken/60,digits = 2)) minutes ✅✅"


end





xtrace(x::NLsolve.SolverResults) = vcat([(x.trace[i].metadata["x"])' for i in 1:x.iterations]...)
ftrace(x::NLsolve.SolverResults) = vcat([(x.trace[i].metadata["f(x)"])' for i in 1:x.iterations]...)


function solve_once(p::Param,m0::Model,x0::Vector{Float64})
	nlsolve((F,x) -> solve!(F,x,p,m0),x0,iterations = p.iters,store_trace = p.trace, extended_trace = p.trace)
end



"""
run Single region model for all time periods
"""
function run(p::Param; estimateθ = false)

	setperiod!(p,1)
	# x0 = startval(p)
	x0 = nearstart(p)

	# m = Region(p)
	# x = jm(p,m,x0)
	# px = x0.pr / p.moments[1,:P_rural]
	# transform!(p.moments,:P_rural => (x -> x .* px) => :P_rural)

	sols = NamedTuple[]
	push!(sols,x0)
	M = Region[]

	for it in 1:length(p.T)
		# println(it)
		setperiod!(p,it)
		m = Region(p)
		x = jm(p,sols[it], estimateθ = estimateθ)
		push!(sols,x)
		if it == 1
			p.ϕ1 = x.ϕ * p.ϕ1x
		end
		update!(m,p,x)

		push!(M,m)
		if it == 1
			# adjust relative price in data to first period solution
			# px = x.pr / p.moments[1,:P_rural]
			# transform!(p.moments,:P_rural => (x -> x .* px) => :P_rural)
		end

	end

	(sols[2:end],M,p) # solutions, models, and parameter

end


"""
run Single region model for all time periods
"""
function run(x0::NamedTuple, p::Param)

	sols = NamedTuple[]
	push!(sols,x0)
	M = Region[]

	for it in 1:length(p.T)
		# println(it)
		setperiod!(p,it)
		m = Region(p)
		x = jm(p,sols[it])
		push!(sols,x)
		if it == 1
			p.ϕ1 = x.ϕ * p.ϕ1x
		end
		update!(m,p,x)

		push!(M,m)
		if it == 1
			# adjust relative price in data to first period solution
			# px = x.pr / p.moments[1,:P_rural]
			# transform!(p.moments,:P_rural => (x -> x .* px) => :P_rural)
		end

	end

	(sols[2:end],M,p) # solutions, models, and parameter

end


runkr2() = runk(par = Dict(:K => 2,:kshare => [0.5,0.5], 
:factors => [1.0,1.01], :gs => zeros(2), :gsr => zeros(2)), hetθr = true)

"""
run Multi-region model for all time periods starting from 
the single city starting value. 
Keyword `estimateθ` tells algorithm to find the θu for each city that fits the population distribution.
"""
function runk(;par = Dict(:K => 2,:kshare => [0.5,0.5], 
	          :factors => [1.0,1.0], :gs => zeros(2), :gsr => zeros(2)), 
			  estimateθu = true,estimateθr = true,istest = false, hetθr = false, verbose = true,θdf = nothing, lbL = 0.05,irrelevantλ = true,LUwgt_data = true, constr_viol_tol = nothing)

	# get single city solution in first period
	p = Param(par = par, use_estimatedθ = false)
	@assert p.K > 1

	if haskey(par, :σ)
		@info "running with σ = $(p.σ)"
	end
	if haskey(par, :ϵflat)
		@info "running with flat ϵr"
	end
	if haskey(par, :ξw)
		@info "running with ξw = $(p.ξw)"
	end

	setperiod!(p,1)
	x0 = nearstart(p)
	m = Region(p)
	x0 = jm(p,x0, estimateθ = false)
	update!(m,p,x0)

	# Build starting values from single city for each Region depending on hetr or not
	if hetθr
		x = (Lr = [m.Lr for i in 1:p.K],
		r = m.r,
		pr = m.pr,
		Sr = [m.Sr for i in 1:p.K],
		ϕ = [m.ϕ for i in 1:p.K],
		Lu = [m.Lu for i in 1:p.K], 
		ρr = [m.ρr for i in 1:p.K]
		)
	else
		x = (LS = m.Lr / m.Sr,
		     r = m.r,
			 pr = m.pr,
			 Sr = [m.Sr for i in 1:p.K],
			 ϕ = [m.ϕ for i in 1:p.K],
			 Lu = [m.Lu for i in 1:p.K]
			 )
	end
	
	runk_impl(x,p, estimateθu = estimateθu, estimateθr = estimateθr,istest = istest, verbose = verbose, θdf = θdf, lbL = lbL,irrelevantλ = irrelevantλ,LUwgt_data = LUwgt_data,constr_viol_tol = constr_viol_tol)
end


function growth() 
	g = [1.02, 1.03, 1.04]
	prod(g, 1.0)
end

function lastn_avg(x::Vector,n)
	m = length(x)
	if m < n
		mean(x)
	else
		mean(x[(end-(n-1)):end])
	end
end


function runk_impl(x0::NamedTuple,p::Param; estimateθr = true,estimateθu = true,istest = false, verbose = true,θdf = nothing,lbL = 0.05,irrelevantλ = true,LUwgt_data = true, constr_viol_tol = nothing)
	sols = NamedTuple[]
	objs = Float64[]
	edatas = DataFrame[]
	# ϕvs =Vector{Float64}[]
	# dϕvs =Vector{Float64}[]
	push!(sols,x0)

	ishetθr = haskey(x0, :Lr)

	if ishetθr
		this = HetCountry # an emtpy array of countries
	else
		this = Country  # an emtpy array of countries
	end
	C = this[]

	oθr = zeros(p.K)
	oθu = zeros(p.K)

	# growth vectors
	gθr = Float64[]
	gθu = Float64[]


	if verbose prog = Progress(length(p.T), desc = "Country Model", barglyphs=BarGlyphs("[=> ]"), barlen = 50, color = :green) end
	# @showprogress "Country Model" for it in 1:length(p.T)
	for it in 1:length(p.T)
		# println(it)
		# println(it)
		setperiod!(p,it)
		c = this(p,istest = istest)  # performs scaling of productivity upon creation

		# if new growth values are supplied, insert here
		if !isnothing(θdf) && p.T[it] <= 2020
			for ik in 1:p.K
				idx = (θdf.it .== it) .&  (θdf.region .== ik)
				# println("k = $ik, t = $it, idx = $idx")
				c.pp[ik].θr = θdf.θrkt_new[idx][1]
				c.pp[ik].θu = θdf.θukt_new[idx][1]
			end
		end

		@debug "θrs in" [c.pp[ik].θr for ik in 1:p.K]
		@debug "θus in" [c.pp[ik].θu for ik in 1:p.K]

		if p.T[it] == 2030 # first period of future
			# manually fix the theta distribution
			mθr = mean(sols[end].θr) # get growth over last 2 periods
			mθu = mean(sols[end].θu)

			push!(gθr, (mθr - mean(sols[end-1].θr)) / mean(sols[end-1].θr) )
			push!(gθu, (mθu - mean(sols[end-1].θu)) / mean(sols[end-1].θu) )

			# offsets are fixed at last period
			oθr = sols[end].θr .- mθr
			oθu = sols[end].θu .- mθu
		end

		if p.T[it] > 2020
			# manually fix the thetas
			mθr = mean(sols[end].θr)
			mθu = mean(sols[end].θu)
			# add to growth vector
			push!(gθr, p.magr)
			push!(gθu, p.magu)

			for ik in 1:c.K
				c.pp[ik].θu = (1 + lastn_avg(gθu,p.magt)) * mθu + oθu[ik]
				c.pp[ik].θr = (1 + lastn_avg(gθr,p.magt)) * mθr + oθr[ik]
			end
			xmod = jc(c,sols[it],estimateθr = false, estimateθu = false,irrelevantλ = irrelevantλ) # returns a JuMP model as last element

		else
			xmod = jc(c,sols[it],estimateθr = estimateθr, estimateθu = estimateθu,lbL = lbL,irrelevantλ = irrelevantλ,LUwgt_data = LUwgt_data, constr_viol_tol = constr_viol_tol) # returns a JuMP model as last element
		end
		# returns a JuMP model as last element
		if isa(xmod, JuMP.Model)
			return xmod
		else
			# clean up results and save
			if ishetθr
				x,ϕs, mod , edata = xmod[1], xmod[2], xmod[3], xmod[4]
				push!(objs,objective_value(mod))
				push!(edatas,edata)
			else
				x,ϕs, mod  = xmod[1], xmod[2], xmod[3]
				push!(objs,objective_value(mod))
			end
		end

		# x,ϕs = jc(c,sols[it],estimateθ = estimateθ)
		# x,ϕs,dϕs = jc(c,sols[it],estimateθ = estimateθ)
		push!(sols,x)
		
		if it == 1
			for ik in 1:p.K
				c.pp[ik].ϕ1 = ϕs[ik] * c.pp[ik].ϕ1x
			end
		else
			for ik in 1:p.K
				c.pp[ik].ϕ1 = C[1].R[ik].ϕ * c.pp[ik].ϕ1x
			end
		end

		@debug "θrs in == out" all([c.pp[ik].θr for ik in 1:p.K] .== [x.θr[ik] for ik in 1:p.K])
		@debug "θus in == out" all([c.pp[ik].θu for ik in 1:p.K] .== [x.θu[ik] for ik in 1:p.K])


		# overwrite θs in any case
		for ik in 1:p.K
			c.pp[ik].θu = x.θu[ik]
			c.pp[ik].θut[it] = x.θu[ik]
			c.pp[ik].θr = x.θr[ik]
			c.pp[ik].θrt[it] = x.θr[ik]
		end

		update!(c,x,estimateθ = true)
		push!(C,c)
		if verbose next!(prog) end
		# push!(ϕvs,ϕs)
		# push!(dϕvs,dϕs)
	end
	# return (gθr, gθu)
	# (sols,C,p, ϕvs, dϕvs) # solutions, models, and parameter
	if ishetθr
		(sols,C,p,objs,edatas) # solutions, models, and parameter
	else
		(sols,C,p,objs) # solutions, models, and parameter
	end
end



function k(K;pars = Dict(),estimateθ = true, fit_allyears = true)
	runk(par = merge(Dict(:K => K,:kshare => [1/K for i in 1:K], :factors => ones(K), :gs => zeros(K)), pars),estimateθ = estimateθ)
end

function khet(K;pars = Dict(),estimateθu = true, estimateθr = true, verbose = true, θdf = nothing, lbL = 0.05,irrelevantλ = true,LUwgt_data = true, constr_viol_tol = nothing)
	d0 = Dict(:K => K,:kshare => [1/K for i in 1:K], :factors => ones(K), :gs => zeros(K), :gsr => zeros(K), :𝕊 => zeros(60,K))
	runk(par = merge(d0, pars),estimateθu = estimateθu,estimateθr = estimateθr, hetθr = true, verbose = verbose,θdf = θdf, lbL = lbL,irrelevantλ = irrelevantλ,LUwgt_data = LUwgt_data, constr_viol_tol = constr_viol_tol)
end

function plot_edata()
	p = Param()
	x = DataFrame()
	for it in 1:19
		setperiod!(p,it)
		append!(x,areaprice_estim(p))
	end
	
	@df x plot(:year, :s_price_data, group = :LIBGEO, leg = :outerright, title = "Price Data")
	
end

function read_baseline_csv()
	fp = joinpath(dbtables(),"baseline.csv")
	CSV.read(fp, DataFrame)
end

function read_baseline_aggregated_csv()
	fp = joinpath(dbtables(),"baseline-aggregated.csv")
	CSV.read(fp, DataFrame)
end


function export_baseline_csv()
	fp = joinpath(dbtables(),"baseline.csv")
	da = @chain khet_run(readdisk = true) begin
		select(_, Not(names(_, Vector)))
	end
	on = names(da)
	rename!(da, replace.(on, "ρ" => "rho", "θ" => "theta", "τ" => "tau", "𝕎" => "wealth"))
	CSV.write(fp, da)
	@info "export to $fp done"
end

function export_any_csv(d::DataFrame,fp::String)
	da = @chain d begin
		select(_, Not(names(_, Vector)))
	end
	on = names(da)
	rename!(da, replace.(on, "ρ" => "rho", "θ" => "theta", "τ" => "tau", "𝕎" => "wealth"))
	CSV.write(fp, da)
	@info "export to $fp done"
end

function export_baseline_interpolated_csv()
	fp = joinpath(dbtables(),"baseline-interpolated.csv")
	@chain khet_run(readdisk = true) begin
		select(_, Not(names(_, Vector)))
		interp_model_data(_)
		CSV.write(fp, _)
	end
	@info "export to $fp done"
end

"""
	khet_run 

Main heterogeneous region (k) runner. Builds (or loads from disk) the baseline version of the model. Building the baseline means calling [`khet_estimate_impl`](@ref) which implements the estimation of the multi region model and is used to estimate the model parameters.

Use this function to load the results from the baseline model as a dataframe. You need to build the baseline on your first run by running the function with default keyword arguments, i.e. `khet_run()` will save the baseline to your disk.

"""
function khet_run(;K = 20, writedisk = true, readdisk = false,  tol = 1e-5,par = Dict(),fname = "", verbose = true,iterateP = 2)
	fn = fname == "" ? "baseline" : fname
	if !readdisk
		# run the function
		d = khet_estimate_impl(K,tol = tol,par = par,verbose = verbose,iterateP = iterateP)  # returns a csv
		if writedisk
			CSV.write(joinpath(outdir,"khet-$(K)-$(fn).csv"), d)
		end
	else
		d = CSV.read(joinpath(outdir,"khet-$(K)-$(fn).csv"), DataFrame, types = Dict(:CODGEO => String))
		# read a vector encoded in a long string!
		d[!,:iDensities_n] = [strparse(x) for x in d.iDensities_n]
		d[!,:iDensities]   = [strparse(x) for x in d.iDensities]
		d[!,:ϕmids] = [strparse(x) for x in d.ϕmids]
	end
	d
end


function check_df_numerics(d1::DataFrame,d2::DataFrame; exact = false)
	# check only numeric data
	bm = @chain d1 begin
		select(_,eltype.(eachcol(_)) .<: Number)
		select(_,all.(isfinite,eachcol(_)))  # drop NaN in HPI for final period
	end

	km = select(d2, names(bm))

	return isapprox.(bm,km)

	e = abs.( Matrix(bm) .- Matrix(km) )  # matrix of abs deviations


	if exact
		r = maximum( abs.( Matrix(bm) .- Matrix(km) ) )
		if r > 0 
			println("error: $r")
		end
		r == 0.0
	else
		r = maximum( abs.( Matrix(bm) .- Matrix(km) ) )
		println("error: $r")
		maximum( abs.( Matrix(bm) .- Matrix(km) ) ) < 1e-7
	end
end

"Choose how to update the sequence of 𝕊 matrices. Different scenarios call for different speeds and methods of updating this in order to achieve convergence in the land price loop"
function update_𝕊(𝕊::Vector{Matrix{Float64}}, iterateP, p::Param; scenario = :default)
	ls = length(𝕊)  # how many iterations have been done already?
	@info "ls = $ls"

	if scenario == :default
		if ls > 3
			# mapslices(y -> smooth(y, 19), mean(𝕊), dims = 1)
			# (ls + 1) * 0.1 * mean(𝕊)
			# (ls + 1) * 0.1 * mean(𝕊)
			# mean(𝕊[(end-2):end])
			# mapslices(y -> smooth(y, 5), 𝕊[end], dims = 1)
			if iterateP == 1
				mean(𝕊)
			else
				p.psmooth == 1 ? 0.9 * mean(𝕊) : 0.3 * 𝕊[end]
			end
		elseif ls == 3
			if iterateP == 1
				mean(𝕊)
			elseif iterateP == 2
				# 𝕊[end]
				# mapslices(y -> smooth(y, 7), 𝕊[end], dims = 1)
				p.psmooth == 1 ? 0.7 * mean(𝕊) : 0.4 * 𝕊[end]

			end
		elseif ls == 2
			if iterateP == 1
				mean(𝕊)
			elseif iterateP == 2
				p.psmooth == 1 ? 0.6 * mean(𝕊) : 0.4 * 𝕊[end]
				# 0.6 * mapslices(y -> smooth(y, 7), 𝕊[end], dims = 1)

				# 0.8 * S_adjuster(𝕊[end], 1.0)
			end
		elseif ls == 1
			if iterateP == 1
				𝕊[end]
			elseif iterateP == 2
				# mapslices(y -> smooth(y, 5), 𝕊[end], dims = 1)
				0.1 * S_adjuster(𝕊[end], 0.2)
				# if p.psmooth == 1 
				# 	0.7 * 𝕊[end]
				# elseif p.psmooth == 2
				# 	0.4 * 𝕊[end]
				# end
			end
		end
	elseif scenario == :aggloμ
		if ls == 1
			0.6 * 𝕊[end]
		else
			0.5 * mean(𝕊)
		end
	elseif scenario == :aggloλ
		if ls == 1
			𝕊[end]
		else
			0.4 * mean(𝕊)
		end
	else 
		error("invalid scenario selected")
	end
end

"beta should be 0.9888"
function getbeta(d::DataFrame)
	@chain d begin
		subset(:year => x-> x.==1840)
		transform([:Yu, :Yr, :pr] => ((x,y,z) -> x .+ y .* z) => :Y)
		combine([:Y,:𝕎lr,:pop] => ((x,y,z) ->  (mean(y ./ x,Weights(z)),sum(y) ./ sum(x))))
	end
end

"""
	Heterogeneous Country Estimation Loop

This function implements the estimation strategy outlined in section XX (Dynamic Model) of the paper. We repeatedly run function [`khet`](@ref), each time computing a new set of future discounted rents 𝕊 for each region, which is supplied to the next iteration of the loop. The loop stops once 2 successive price vectors are sufficiently close.
"""
function khet_estimate_impl(K;tol = 1e-4,par = Dict(),verbose = true, iterateP = 2,estimateθu = true,estimateθr = true,θdf = nothing, lbL = 0.05, irrelevantλ = true,LUwgt_data = true, constr_viol_tol = nothing)
	
	if (!estimateθr && isnothing(θdf)) | (!estimateθu && isnothing(θdf))
		error("if you choose not to choose θ, you need to supply in θdf")
	end

	if (haskey(par, :λ))
		@info "running model with Agglomeration forces λ = $(par[:λ])"
	elseif haskey(par, :μ)
		@info "running model with congestion forces μ = $(par[:μ])"
	end

	# 0. containers
	ℙ = Matrix{Float64}[]	# land prices from model
	𝕊 = Matrix{Float64}[]   # future rents offsets model
	c = Vector{HetCountry}[]  # vector of heterogeneous countries
	sols = Vector{NamedTuple}[]

	# create a default dict
	pars = merge(Dict(
		:T => 1840:10:2350,
		:K => K,
		:kshare => [1/K for i in 1:K], 
		:factors => ones(K), 
		:gs => zeros(K), 
		:gsr => zeros(K), :𝕊 => zeros(60,K)), par)

	# 1. first iteration : starts at single city model, as usual
	so,C,p,objs,edatas = khet(K, pars = pars,verbose = verbose,estimateθu = estimateθu,estimateθr = estimateθr,θdf = θdf, lbL = lbL, irrelevantλ = irrelevantλ, LUwgt_data = LUwgt_data, constr_viol_tol = constr_viol_tol)
	so = so[2:end] # throw away first period (from single city)
	push!(sols,so)
	push!(c, C)
	# return post_proc(c[end],edatas)

	sfr = sum_future_rents(c[end], doW = true)  # those are W[t,k]
	push!(𝕊, sfr[:S])  # rents from period 1 forward
	push!(ℙ, hcat([so[it].ρr for it in 1:(length(so)-1)]...)' .+ 𝕊[end][1:(end-1),:] )

	# plot([C[it].R[1].θr for it in 1:20])	
	# savefig(joinpath(@__DIR__,"..","out","S-theta00.pdf"))
			

	# plot(plot(𝕊[end], leg = :left, color = reshape(distinguishable_colors(K),1,K), ylims = (0,2)),
	# plot(ℙ[end], leg = :left, color = reshape(distinguishable_colors(K),1,K), ylims = (0,2)))
	# savefig(joinpath(@__DIR__,"..","out","S-current00.pdf"))
	
	# 2. Price search loop
	if iterateP > 0
		price_loop!(𝕊,ℙ,sols,pars,c,tol, doplot = true,verbose = verbose)
	else
		if verbose
			@info "not iterating for ℙ"
		end
	end
	# return c
	
	d = post_proc(c[end],edatas)
	# p1 = @df subset(d, :year => leq(2020)) plot(:year, :θr, group = :region, color = reshape(distinguishable_colors(K),1,K), yscale = :log10)
	# p2 = @df subset(d, :year => leq(2020)) plot(:year, :𝕊 .+ :ρr, group = :region, color = reshape(distinguishable_colors(K),1,K), ylims = (0,2))

	# plot(p2, leg = :left)
	# savefig(joinpath(@__DIR__,"..","out","S-current00.pdf"))
	
	d

end

# 2. Price search loop

"""
rural price loop

by default always chooses thetau and thetar
"""
function price_loop!(𝕊,ℙ,sols,par::Dict,c::Vector{Vector{HetCountry}},tol::Float64; doplot = false,verbose = false)
	if verbose
		@info "Iterating on Prices"
	end
	K = size(𝕊[end],2)
	Δ = 100.0
	i = 1
	while Δ > tol 
		if verbose @info "start iteration $i" end
		newS = 𝕊[end]

		if doplot
			plot(plot(𝕊[end][:,1:20], leg = false, title = L"\mathbb{S}", color = reshape(distinguishable_colors(K),1,K), ylims = (0,2)),
			plot(ℙ[end][:,1:20], leg = false,title = L"\mathbb{P}" , color = reshape(distinguishable_colors(K),1,K), ylims = (0,2)))
			savefig(joinpath(@__DIR__,"..","out","S-current$i.pdf"))

			plot(plot(newS[:,1:20], leg = false, title = L"\mathbb{S}",color = reshape(distinguishable_colors(K),1,K), ylims = (0,2)),
			plot(ℙ[end][:,1:20], leg = false,title = L"\mathbb{P}" ,color = reshape(distinguishable_colors(K),1,K), ylims = (0,2)))
			savefig(joinpath(@__DIR__,"..","out","S-start$i.pdf"))
		end

		# in the price loop we reduce the weight in the objective function of fitting the prices in some periods 
		# this does *not* mean the fit is significantly worse, but it helps the solver over large jumps in S
		pars = Dict(
			:T => 1840:10:2350,
			:K => K,
			:kshare => [1/K for i in 1:K], 
			:factors => ones(K), 
			:gs => zeros(K), 
			:gsr => zeros(K), 
			:𝕊 => newS)
		pars[:pweight] = ones(length(pars[:T]))
		pars[:pweight][8:14] .= 0.9
		pars[:pweight][13] = 0.4
		pars[:pweight][17] = 0.8

		p = Param(par = merge(pars,par)) 

		# start iteration at first period solution of previous iteration.
		so,C,p,objs,edatas = runk_impl(sols[end][1], p, verbose = verbose)
		so = so[2:end] # throw away first period (from previous solution)

		plot([C[it].R[1].θr for it in 1:20])	
		savefig(joinpath(@__DIR__,"..","out","S-theta1.pdf"))

		push!(sols,so)

		push!(c, C)
		push!(𝕊, sum_future_rents(C)[:S])
		push!(ℙ, hcat([so[it].ρr for it in 1:(length(so)-1)]...)' .+ 𝕊[end][1:(end-1),:] )
				

		Δ = maximum(abs.(ℙ[end][isfinite.(ℙ[end])] .- ℙ[end-1][isfinite.(ℙ[end-1])]))
		if verbose @info "end iteration $i: difference ℙ = $Δ" end
		# @info "10-th period rents: $(so[10].ρr)"
		i += 1
	end
end





function strparse(x::AbstractString)
	rr = r"[+-]?([0-9]*[.])?[0-9]+"
	[parse(Float64, t.match) for t in eachmatch(rr, x)]
end

function k20(;overwrite = false)
	if overwrite
		x,C,p = runk(par = Dict(:K => 20,:kshare => [1/20 for i in 1:20], :factors => ones(20), :gs => zeros(20)),estimateθ = true)
		d = dataframe(C)

		FileIO.save(joinpath(intables, "k20.jld2"), Dict("df" => d))
		
		x,C,p,d
	else
		df = FileIO.load(joinpath(intables, "k20.jld2"))
		df["df"]
	end

	# K = 20
	# # par = Dict(:K => K, :kshare => [1/K for i in 1:K], :factors => [1.09, 1.02, 1.02, 1.01,1.01, [1.005 for i in 1:7]...,ones(8)...], :gs => [0.003,zeros(K-1)...])
	# par = Dict(:K => K, :kshare => [1/K for i in 1:K], :factors => ones(K), :gs => zeros(K))
	# # par = Dict(:K => K, :kshare => [1/K for i in 1:K], :factors => [1.1, 1.01, 1.01, 1.005,1.005, [1.0025 for i in 1:7]...,ones(8)...], :gs => [0.0025,zeros(K-1)...])
	# # par = Dict(:K => K, :kshare => [1/K for i in 1:K], :factors => [1.05, 1.01, 1.01, 1.005,1.005, [1.0025 for i in 1:7]...,ones(8)...], :gs => [0.002,zeros(K-1)...])
	# # par = Dict(:K => K, :kshare => [1/K for i in 1:K], :factors => [1.02, 1.001, 1.001, 1.01,1.01, [1.005 for i in 1:7]...,ones(8)...], :gs => [0.001,zeros(K-1)...])
	# # x,C,p = runk(par = par)
	# runk(par = par)
	# # relpop(C)
	# # dd = relpop(C)
	# @df subset(dd, :region => x-> x.> 1, :year => x-> x.> 1870) plot(:year, :rel_Lu_mean, group = :grouplabel, title = "model relative to paris")
	
end


function feas_check(it; d1 = 0.04, d2= 1.0)
	par = Dict(:d1 => d1, :d2 => d2,:K => 2,:kshare => [0.5,0.5], :factors => [1.0,1.0], :gs => zeros(2))

	x0,M,p = run(Param(par = par))
	m = M[it]  # period 
	setperiod!(p,it)
	sols =Vector{Float64}[]
	C = Country[]  # an emtpy array of countries

	x = Float64[]
	push!(x, m.Lr / m.Sr)
	push!(x, m.r)
	push!(x, m.pr)
	for ik in 1:p.K
		push!(x,m.Sr)
	end
	for ik in 1:p.K
		push!(x,m.Lu)
	end
	for ik in 1:p.K
		push!(x,m.ϕ)
	end
	for ik in 1:p.K
		push!(x,p.θu)
	end
	push!(sols, x)
	c = Country(p)  # performs scaling of productivity upon creation
	mo,i = jc(c,sols[1],estimateθ = false,solve = false) # returns a JuMP model as last element
	xmod = jc(c,sols[1],estimateθ = false,solve = true) #


	# ct = JuMP.list_of_constraint_types(mo)
	# ct = JuMP.list_of_constraint_types(mo)
	# JuMP.all_constraints(m, ct[1]...)
	# JuMP.all_constraints(m, ct[2]...)

	# n = JuMP.all_variables(mo)
	

	# https://jump.dev/JuMP.jl/stable/manual/nlp/#Querying-derivatives-from-a-JuMP-model
	# raw_index(v::MOI.VariableIndex) = v.value
	# model = Model()
	# @variable(model, x)
	# @variable(model, y)
	# @NLobjective(model, Min, sin(x) + sin(y))
	values = zeros(length(i))
	values[i["LS"]] = x[1]
	values[i["r"]] = x[2]
	values[i["pr"]] = x[3]
	values[i["Sr[1]"]] = x[4]
	values[i["Sr[2]"]] = x[5]
	values[i["Lu[1]"]] = x[6]
	values[i["Lu[2]"]] = x[7]
	values[i["ϕ[1]"]] = m.ϕ
	values[i["ϕ[2]"]] = m.ϕ

	g0 = zeros(JuMP.num_nl_constraints(mo))

	d = NLPEvaluator(mo)
	MOI.initialize(d, [:Grad])
	MOI.eval_constraint(d, g0, values) 

	# now the solved model
	values[i["LS"]]    = xmod[1][1]
	values[i["r"]]     = xmod[1][2]
	values[i["pr"]]    = xmod[1][3]
	values[i["Sr[1]"]] = xmod[1][4]
	values[i["Sr[2]"]] = xmod[1][5]
	values[i["Lu[1]"]] = xmod[1][6]
	values[i["Lu[2]"]] = xmod[1][7]
	values[i["ϕ[1]"]]  = xmod[2][1]
	values[i["ϕ[2]"]]  = xmod[2][2]

	g1 = zeros(JuMP.num_nl_constraints(mo))

	MOI.eval_constraint(d, g1, values) 


	return g0,g1
		
end

function check(it; d1 = 0.0, d2= 0.0)
	par = Dict(:d1 => d1, :d2 => d2,:K => 2,:kshare => [0.5,0.5], :factors => [1.0,1.0], :gs => zeros(2))

	x0,M,p = run(Param(par = par))
	m = M[it]  # period 
	setperiod!(p,it)
	sols =Vector{Float64}[]
	C = Country[]  # an emtpy array of countries
	
	x = Float64[]
	push!(x, m.Lr / m.Sr)
	push!(x, m.r)
	push!(x, m.pr)
	for ik in 1:p.K
		push!(x,m.Sr)
	end
	for ik in 1:p.K
		push!(x,m.Lu)
	end
	for ik in 1:p.K
		push!(x,m.ϕ)
	end
	for ik in 1:p.K
		push!(x,p.θu)
	end
	push!(sols, x)
	# for it in 1:length(p.T)
		# println(it)
		# setperiod!(p,it)
		c = Country(p)  # performs scaling of productivity upon creation
		xmod = jc(c,sols[1],estimateθ = false) # returns a JuMP model as last element
		if termination_status(xmod[end]) == MOI.LOCALLY_SOLVED
			# clean up results and save
			x,ϕs = xmod[1], xmod[2]
		else		
			println("period = $it")
			println(termination_status(xmod[end]))  # error
			println(JuMP.all_variables(xmod[end]))
			return JuMP.primal_feasibility_report(xmod[end])
		end
		update!(c,x,estimateθ = false)

		push!(C,c)
		# push!(ϕvs,ϕs)
		# push!(dϕvs,dϕs)
	
	# (sols,C,p) # solutions, models, and parameter

	println("percent difference in radius: $(round(100 * (m.ϕ - c.R[1].ϕ) / m.ϕ,digits = 6)) ")
	println("radius single:    $(m.ϕ)")
	println("radius multi (1): $(c.R[1].ϕ) ")
	println("radius multi (2): $(c.R[2].ϕ) ")
	println()
	println("Lr single:    $(m.Lr)")
	println("Lr multi (1): $(c.R[1].Lr) ")
	println("Lr multi (2): $(c.R[2].Lr) ")
	println()
	println("Lu single:    $(m.Lu)")
	println("Lu multi (1): $(c.R[1].Lu) ")
	println("Lu multi (2): $(c.R[2].Lu) ")

	println()
	println("pr single:    $(m.pr)")
	println("pr multi (1): $(c.R[1].pr) ")
	println("pr multi (2): $(c.R[2].pr) ")

	println()
	println("Sr single:    $(m.Sr)")
	println("Sr multi (1): $(c.R[1].Sr) ")
	println("Sr multi (2): $(c.R[2].Sr) ")

	println()
	println("Srh single:    $(m.Srh)")
	println("Srh multi (1): $(c.R[1].Srh) ")
	println("Srh multi (2): $(c.R[2].Srh) ")

	println()
	println("Srh - cityarea single:    $(m.Srh      - π* (m.ϕ)^2)")
	println("Srh - cityarea multi (1): $(c.R[1].Srh - π* (c.R[1].ϕ)^2) ")
	println("Srh - cityarea multi (2): $(c.R[2].Srh - π* (c.R[2].ϕ)^2) ")

	println()
	println("Sr + cityarea + Srh single:    $(m.Srh      + m.Sr      + π* (m.ϕ)^2)")
	println("Sr + cityarea + Srh multi (1): $(c.R[1].Srh + c.R[1].Sr + π* (c.R[1].ϕ)^2) ")
	println("Sr + cityarea + Srh multi (2): $(c.R[2].Srh + c.R[2].Sr + π* (c.R[2].ϕ)^2) ")

end


function check2(;d1 = 0.0, d2= 0.0)
	par = Dict(:d1 => d2, :d2 => d2,:K => 2,:kshare => [0.5,0.5], :factors => [1.0,1.0], :gs => zeros(2))

	x0,M,p = run(Param(par = par))
	m = M[1]  # period 2
	setperiod!(p,1)
	sols =Vector{Float64}[]
	C = Country[]  # an emtpy array of countries


	x = Float64[]
	push!(x, m.Lr / m.Sr)
	push!(x, m.r)
	push!(x, m.pr)
	for ik in 1:p.K
		push!(x,m.Sr)
	end
	for ik in 1:p.K
		push!(x,m.Lu)
	end
	for ik in 1:p.K
		push!(x,m.ϕ)
	end
	for ik in 1:p.K
		push!(x,p.θu)
	end
	push!(sols, x)
	for it in 1:length(p.T)
		# println(it)
		setperiod!(p,it)
		c = Country(p)  # performs scaling of productivity upon creation
		xmod = jc(c,sols[it],estimateθ = false) # returns a JuMP model as last element
		if termination_status(xmod[end]) == MOI.LOCALLY_SOLVED
			# clean up results and save
			x,ϕs = xmod[1], xmod[2]
		else		
			println("period = $it")
			println(termination_status(xmod[end]))  # error
			println(JuMP.all_variables(xmod[end]))
			return JuMP.primal_feasibility_report(xmod[end])
		end

		# x,ϕs = jc(c,sols[it],estimateθ = estimateθ)
		# x,ϕs,dϕs = jc(c,sols[it],estimateθ = estimateθ)
		push!(sols,x)
		# println(sols)
		if it == 1
			for ik in 1:p.K
				c.pp[ik].ϕ1 = ϕs[ik] * c.pp[ik].ϕ1x
			end
		else
			for ik in 1:p.K
				c.pp[ik].ϕ1 = C[1].R[ik].ϕ * c.pp[ik].ϕ1x
			end
		end
		# overwrite θu if estimated
		# if estimateθ
		# 	for ik in 1:p.K
		# 		c.pp[ik].θu = x[3 + 2p.K + ik]
		# 	end
		# end

		update!(c,x,estimateθ = false)

		push!(C,c)
		# push!(ϕvs,ϕs)
		# push!(dϕvs,dϕs)
	end
	
	(sols,C,p) # solutions, models, and parameter
end





"helper function to prepare country param"
function startvals_par(K::Int; θus = 1.2:0.01:1.35)
	if K == 2
		θus = 1.2:0.01:1.32
		facs = [1.0, θus[1]]
	elseif K == 3	
		facs = [1.0, 1.07, θus[1]]
	elseif K == 4	
		facs = [1.0, 1.07, 1.1, θus[1]]
	elseif K == 5	
		facs = [1.0, 1.07, 1.08, 1.09, θus[1]]
	end
		
	(par = Dict(:K => K,:kshare => [1/K for i in 1:K], :factors => facs, :gs => zeros(K)), θus = θus)
end

"""
collect valid starting values for the k country case by reusing the first period solution
	of each preceding step in θu along the sequence
"""
function startvals_impl(K::Int; θu = 1.2:0.01:1.35)
	
	par, θus = startvals_par(K,θus = θu)

	x,C,p = runk(par = par)

	x0 = Vector{Float64}[] 
	push!(x0, x[2])
	for (i,θu) in enumerate(θus)
		# println("θu = $θu")
		par[:factors][K] = θu
		p = Param(par = par)
		x1,C1,p1 = runk_impl(x0[i],p)
		push!(x0,x1[2])
	end
	(par = par, x0 = x0[end])
end


"""
	startvals_k()

Find feasible starting values for multi city case (2 to 5 cities) and write to disk. By default return the saved dict with start param and initial guess vector `x0`.
"""
function startvals_k(K::Int; overwrite = false)
	if overwrite
		# recompute all starting values and write to disk
		d = Dict()
		for k in 2:K
			@info "doing case k=$k"
			d[k] = startvals_impl(k)
		end
		bson(joinpath(@__DIR__,"..","out","multistarts.bson"), d)

	else
		# read from disk
		d = BSON.load(joinpath(@__DIR__,"..","out","multistarts.bson"))
	end
	d[K]
end

"5 country case"
function k5()
	(par, x0) = startvals_k(5)
	par[:gs] = zeros(5)
	p = Param(par = par)
	runk_impl(x0,p)
end


function k3()

	(par, x0) = startvals_impl(3, θu = 1.2:0.01:1.24)
	par[:gs] = [0.0,0.001,0.01]
	
	p = Param(par = par)
	x,C,p = runk_impl(x0,p)
	d = dataframe(C)
	gg = groupby(select(filter(x -> x.region .∈ Ref([1,3]), d), :region, :year, :Lu), :year)
    combine(gg, :Lu => (x -> maximum(x) / minimum(x)) => :rel_Lu)
end

"""
return model relative population in each year to largest city
largest city is city 1
"""
function relpop(C::Vector{Country})
	d = dataframe(C)
	cla = select(C[1].pp[1].citylist, :rank, :group, :ngroup)
	transform!(cla, [:group, :ngroup] => ((a,b) -> string.(a,"(n=",b,")")) => :grouplabel)

	d2 = leftjoin(select(d, :year, :region, :Lu), C[1].pp[1].citylist, on = [:region => :rank, :year])
	gg = groupby(d2, :year)
    g2 = combine(gg, [:Lu, :region] => ((a,b) -> a ./ a[b .== 1]) => :rel_Lu, :region, :Lu, :grouplabel)
	combine(groupby(g2, [:grouplabel, :year]),  :rel_Lu => mean, :region) # mean amongst groups
end



function runm()
	run(Param())
end
function plot1()
	x,M,p = run(Param())
	ts_plots(M,p)
end
function plot1cs(it)
	x,M,p = run(Param())
	cs_plots(M[it],p,it)
end

"""
	dash(it;par = Dict())

Helper function for quick Region dashboard.
"""
function dash(it;par = Dict())
	x,M,p = run(Param(par = par))
	dashboard(M,p,it)
end


function cdash(ik,it)
	x,M,p = k(ik)
	dashboard(M,it)
end
function export_params()
	x,M,p = runm()
	latex_param()
	d = DataFrame(year = collect(p.T), thetau = p.θut, thetar = p.θrt, pr = [M[it].pr for it in 1:length(M)], Lt = p.Lt)
	CSV.write(joinpath(dbtables(),"export_theta_pr.csv"),d)
	CSV.write(joinpath(intables,"export_theta_pr.csv"),d)

	x0 = nearstart(Param())
	CSV.write(joinpath(dbtables(),"export_x0.csv"),DataFrame([x0]))
	CSV.write(joinpath(intables,"export_x0.csv"),DataFrame([x0]))
end


"""
post process simulation output by computing discounted value of 
future rents and merging with estimation data
"""
function post_proc(C::Vector{LandUse.HetCountry},edatas)
	sfr = sum_future_rents(C, doW = true) 
	d = dataframe(C)
	sort!(d, [:region,:year])

	d.𝕎lu = sfr[:Wlu][:]
	d.𝕎lc = sfr[:Wlc][:]
	d.𝕎lr = sfr[:Wlr][:]
	d.𝕎hu = sfr[:Whu][:]
	d.𝕎hc = sfr[:Whc][:]
	d.𝕎hr = sfr[:Whr][:]
	d.ℍu = sfr[:Hu][:]
	d.ℍc = sfr[:Hc][:]
	d.𝕊 = sfr[:S][:]
	d.ℙ = d.𝕊 .+ d.ρr  # update P!
	d.ℍr = sfr[:Hr][:]
	d.Rt = repeat(sfr[:R], outer = C[1].K)

	transform!(d, [["$(x)u", "$(x)r" ] => ByRow((x,y) -> x + y) => "$x" for x in ["𝕎l", "𝕎h", "ℍ"]])

	# make a df out of last iteration
	# df = dataframe(c[end])
	ed = vcat(edatas...)
	d = @chain d begin
		groupby(:year)
		transform([:ρr, :region] => ((x,y) -> normx_by_y(x,y .== 1)) => :ρr_n, 
		           [:ℙ, :region] => ((x,y) -> normx_by_y(x,y .== 1)) => :ℙ_n )
		innerjoin(ed, on = [:year, :region => :rank])
		select(Not([:nodes_bins, :nodes_speeds]))
		sort([:year, :region])
		groupby([:region])
		transform(:cityarea => firstnorm => :cityarea_n,
			:rel_cityarea => firstnorm => :rel_cityarea_n,
			:Lu => firstnorm => :Lu_n)
		leftjoin(_,GDPdeflator(_), on = :year)
		transform([:𝕎h , :ℍ, :p_indexall] => ((x,y,z) -> (x ./ y) ./ z) => :HPI,
			[:𝕎hc , :ℍc, :p_indexall] => ((x,y,z) -> (x ./ y) ./ z) => :HPI_center,
			[:wu0, :wr] => ((x,y) -> C[1].pp[1].α .* x ./ y) => :APG
		)
	end
	transform!(d, :iDensities => ByRow(x -> firstnorm(x)) => :iDensities_n)
	d
end