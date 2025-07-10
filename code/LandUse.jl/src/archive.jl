
# archive



# new strategy to find starting values:
# use a constrained optimizer to avoid x < 0
# this works for the fixed model so far.
# julia> x = LandUse.nlopt_solve(par = Dict(:L => 100.0, :S => 20.0))
# (1.0, [0.04796005781257872, 0.4480817574692655, 0.3219286166291397, 14.974867748022646], :ROUNDOFF_LIMITED)


function NLopt_wrap(result::Vector, x::Vector, grad::Matrix,m::Model,p::Param)
	if length(grad) > 0
		# not implemented
	end
	solve!(result,x,p,m)
end


function nlopt_solve(m::Model,p::Param,x0::Vector{Float64})
	opt = Opt(:LN_COBYLA,length(x0))
	opt.lower_bounds = fill(0.001,length(x0))
	f0(x::Vector, grad::Vector) = 1.0
	opt.min_objective = f0  # fix at a constant function
	equality_constraint!(opt,(r,x,g) -> NLopt_wrap(r,x,g,m,p), fill(1e-9,length(x0)))
	# m.r    = x[1]   # land rent
	# m.Lr   = x[2]   # employment in rural sector
	# m.pr   = x[3]   # relative price rural good
	# m.Sr   = x[4]   # amount of land used in rural production
	# (optf,optx,ret) = optimize(opt, [0.1 * p.S, 0.5 * p.L, 0.3775, 0.545 * p.S])
	opt.upper_bounds = [Inf,1.0,x0[3]*1.5,1.0]

	(optf,optx,ret) = optimize(opt, x0)
end
#
# function nlopt_solve(;p = Param(),x0=nothing)
# 	fm = Region(p)
# 	opt = Opt(:LN_COBYLA,4)
# 	# opt = Opt(:LN_NELDERMEAD,4)
# 	opt.lower_bounds = fill(0.001,4)
# 	# opt.upper_bounds = [1.0,1.0,1.0,1.0]
# 	f0(x::Vector, grad::Vector) = 1.0
# 	opt.min_objective = f0  # fix at a constant function
# 	equality_constraint!(opt,(r,x,g) -> NLopt_wrap(r,x,g,fm,p), fill(1e-9,4))
# 	# m.r    = x[1]   # land rent
# 	# m.Lr   = x[2]   # employment in rural sector
# 	# m.pr   = x[3]   # relative price rural good
# 	# m.Sr   = x[4]   # amount of land used in rural production
# 	# (optf,optx,ret) = optimize(opt, [0.1 * p.S, 0.5 * p.L, 0.3775, 0.545 * p.S])
# 	if isnothing(x0)
# 		# x0 = [0.1 * p.S, 0.5 * p.L, 0.3775, 0.545 * p.S]   # an almost arbitrary point in the interior domain.
# 		x0 = [0.24, 0.68, 1.1, 0.86]   # an almost arbitrary point in the interior domain.
# 	else
# 		@assert length(x0) == ndims(opt)
# 		opt.upper_bounds = [Inf,1.0,x0[3]*1.5,1.0]
#
# 	end
# 	(optf,optx,ret) = optimize(opt, x0)
# end

function nlsolve_starts(x0;p = Param())
	m = Region(p)
	r1 = LandUse.nlsolve((F,x) -> LandUse.solve!(F,x,p,m),
							 x0,iterations = 10000,store_trace = p.trace, extended_trace = p.trace)
	if !converged(r1)
		error("no starting values found")
	end
	r1
end








"""
	get_starts(p::Param)

Generate starting values for all years from the fixed ϵ model `FModel`.

In year 1:

1. Construct a `FModel` from a starting value `[0.1 * p.S, 0.5 * p.L, 0.3775, 0.545 * p.S]`
2. In subsequent years, take solution from 1. of previous year as starting value

"""
# function get_starts(;par = Dict())
function get_starts(p::Param)
	# fm = LandUse.FModel(p)  # create a fixed elasticity model
	# p = Param(par=par)
	starts = Vector{Float64}[]  # an empty array of vectors

	# 2. For each time period
	for it in 1:length(p.T)
		setperiod!(p, it)   # set period on param to it

		# if first year
		if it == 1
			x0 = nlopt_solve(startval(),p=p)
			if (x0[3] == :ROUNDOFF_LIMITED) | (x0[3] == :SUCCESS)
				# update2!(fm,p,x0[2])
				push!(starts, x0[2])
				# println("rural market clears with $(Rmk(fm,p))")
			else
				# p1 = plot(fm.Ftrace',ylim = (-5,5),title = "Ftrace")
				# p2 = plot(fm.xtrace',title = "xtrace",label = ["rho" "phi" "r" "Lr" "pr" "Sr"])
				# pl = plot(p1,p2,layout = (1,2))
				# savefig(pl,joinpath(@__DIR__,"..","images","Fmodel_trace.png"))
				error("first FModel not converged")
			end

		else  # in other years just start at previous solution
			x0 = nlopt_solve(p=p,x0 = startvals[it-1])
			if (x0[3] == :ROUNDOFF_LIMITED) | (x0[3] == :SUCCESS)
				# update2!(fm,p,x0[2])
				push!(starts, x0[2])
				# println("rural market clears with $(Rmk(fm,p))")
			else
				print(x0)
				error("FModel not converged in period $it")
			end
		end
	end
	return starts
end



"""
	adapt_ϵ(p::Param,x0::Vector{Float64})

Adaptively increase slope coefficient ``s`` in elasticity of housing supply function [`ϵ`](@ref).
Starts from the first period solution of [`FModel`](@ref).
"""
function adapt_ϵ(m::Model,p::Param,x0::Vector{Float64})
	# setperiod!(p,1)  # start in year one again
	# m = Region(p)

	startvals = Vector{Float64}[]  # an empty array of vectors
	push!(startvals, x0)  # put 1860 solution for flat epsilon function

	# range of elasticity slope values
	ϵs = range(0,stop = p.ϵsmax, length = p.ϵnsteps)[2:end]

	for (i,ϵ) in enumerate(ϵs)
		setfield!(p, :ϵs, ϵ)  # set current value for elaticity function slope

		r1 = LandUse.nlsolve((F,x) -> LandUse.solve!(F,x,p,m),startvals[i],iterations = 1000)
		if converged(r1)
			push!(startvals, r1.zero)
		else
			error("adaptive search not converged for ϵ = $ϵ")
		end
	end
	return (startvals,p)
end



"""
in a given period and given a target param, this step-wise increases thetau
"""
function θstepper(p::Param,it::Int,m::Model,x0::Vector)

	sols = Vector[]

	push!(sols,x0)

	# target thetas
	if abs(p.θu - p.θr) > 0.78
		println("period $it")
		println("diff=$(p.θu - p.θr)")
		# go back to previous period, take p.θu, and set p.θu = p.θu_old + half the distance to the new obtained

		θus = range(p.θut[it-1],stop = p.θu, length = p.nsteps)
		θrs = range(p.θrt[it-1],stop = p.θr, length = p.nsteps)

		for i in 1:length(θus)
			p.θu = θus[i]
			p.θr = θrs[i]
			println("thetau = $(p.θu),thetar = $(p.θr)")
			r1 = solve_once(p,m,sols[i])
			if p.trace
				traceplot(r1,it)
			end
			if converged(r1)
				push!(sols, r1.zero)
			else
				println("last solution: $(sols[end])")

				error("adaptive searchrrrrrr not converged for thetau = $(p.θu),thetar = $(p.θr)")
			end
		end

		# if that converges, go to final values

		# else make step size smaller
	else
		r1 = solve_once(p,m,sols[1])
		if p.trace
			traceplot(r1,it)
		end
		if converged(r1)
			push!(sols, r1.zero)
		else
			error("not converged")
		end
	end


	return sols[end]
end





function matlab_bm()
	println("starting values at same parameter vector:")
	display(vcat(LandUse.get_starts()'...))

	println("timing full model run")
	@time (x,M,p) = run();

end

function make_space_gif()
	x,C,cpar,par = LandUse.runk()
	anim_space(C,par)
end

function make_ts_space_gif()
	x,C,cpar,par = LandUse.runk()
	plot_ts_xsect(C,par,1)
end


function solve1(p::Param)
	x = stmodel(p)
	# p.taum = 1
	# p.taul = 1
	m = Region(p)
	solve_once(p,m,[x...])
end

function reduce_θ_step!(p::Param)
	# reduce step size
	p.θstep -= 0.05
	if p.θstep > 0 || error("reached neg step") end
end

function x0grid(m,p,x0,it)
	# make a grid over potential starting vectors: +- 5 % of current value
	# m.r    = x[1]   # land rent
	# m.Lr   = x[2]   # employment in rural sector
	# m.pr   = x[3]   # relative price rural good
	# m.Sr   = x[4]   # amount of land used in rural production

	r1 = solve_once(p,m,x0)
	if converged(r1)
		return r1.zero
	else
		println(it)
		n = 5
		ra = range(0.96,1.04,length = n)
		rp = range(1.00,1.05,length = n)
		rn = range(0.95,1.0,length = n)
		grid = vec(collect(Base.product(x0[1] .* rp, x0[2] .* rp,x0[3] .* rn,x0[4] .* rn)))

		for i in grid
			println([i...])
			r = solve_once(p,m,[i...])
			if converged(r)
				return r.zero
			end
			if p.trace
				traceplot(r,it)
			end
		end
		error("no solution found in entire grid")
	end
end

function trysolve(m,p,x0,it)

	# solution vector
	solutions = Vector[]
	push!(solutions,x0)
	ps   = Tuple{Float64,Float64}[]


	# targets
	θr1 = p.θr
	θu1 = p.θu
	r1 = solve_once(p,m,solutions[end])

	if converged(r1)
		return r1.zero
	else
		# get previous values
		setperiod!(p,it-1)
		θr0 = p.θr
		θu0 = p.θu
		println("enter while in period $it")

		while θu0!=θu1 || θr0!=θr1
			for current in (:urban, :rural)
				println(current)
				# current tuple (thetau, thetar)
				done = false
				step = 0.1
				while (!done)
					if current == :urban

						# p.θu = θu0 + step*(θu1-θu0)
						p.θu = θu0 + step
						# println("thetau0 = $θu0")
						# println("thetar0 = $θr0")
						println("thetau = $(p.θu)")
						r = solve_once(p,m,solutions[end])
						if converged(r)
							push!(solutions,r.zero)
							push!(ps,(p.θu,p.θr))
							done = true
							println(r)
							θu0 = p.θu
							println("done!")
							if p.trace
								traceplot(r,it)
							end
						else
							step = step * 0.9
						end


						# println("urban step = $step")
					else
						# p.θr = θr0 + step*(θr1-θr0)
						p.θr = θr0 + step

						# println("thetau0 = $θu0")
						# println("thetar0 = $θr0")
						println("thetar = $(p.θr)")
						r = solve_once(p,m,solutions[end])
						if converged(r)
							push!(solutions,r.zero)
							push!(ps,(p.θu,p.θr))
							done = true
							θr0 = p.θr
							println(r)


							println("done!")
							if p.trace
								traceplot(r,it)
							end
						else
							step = step * 0.9
						end
						# println("rural step = $step")
					end
				end
			end
		end
		return solutions[end]
	end

end

# plotting archive









"make separate time series plot for each region"
function plot_ts(C::Vector{Country})
	df = dataframe(C)
	K = length(C[1].R)
	# vars = (:ρr, :qr, :Lr, :Lu, :wu0, :wr, :Sr, :Srh, :r, :pr, :ϕ, :icu_input, :iDensity, :icu, :icr, :iτ, :iq, :iy)
	# @df df plot(:year, cols(2:size(df,2)))
	s = stack(df, Not([:year, :region]))
	# sims = [(:Lr,:Lu); (:Sr, :ϕ, :Srh); (:qr, :r); (:wr , :wu0)]
	sims = [[:Lr,:Lu], [:Sr, :ϕ, :Srh], [:qr, :r], [:wr , :wu0]]
	titles = ["Labor"; "Land"; "Rents"; "Wages"]
	nms = [[L"L_r" L"L_u"], [L"S_r" L"\phi" L"S_{rh}"] , [L"q" L"r"], [L"w_r" L"w_u"]]

	plts = Any[]
	for ir in 1:K
		plt = Any[]
		for i in 1:length(sims)
			x = @linq s |>
				where((:variable .∈ Ref(sims[i])) .& (:region .== ir))
			px = @df x plot(:year, :value, group = :variable,
							title = titles[i],
							titlefontsize=10,
							label=nms[i],
							legend = :topleft,
							linewidth=2,marker = (:circle,3))
			push!(plt, px)
		end
		push!(plts, plot(plt...,layout = (2,2)))
	end
	plts
end


function impl_plot_slopes(C::Vector{Country})
	d = dataframe(C)
	d.larea = log.(d.cityarea)
	d.lu = log.(d.Lu)
	gd = groupby(d,:year)
	gd = combine(gd, AsTable([:larea, :lu]) => (x -> round.(diff(vcat(extrema(x.larea)...)) ./ diff(vcat(extrema(x.lu)...)),digits = 1)) => :slope)
	d  = innerjoin(d,gd,on = :year)
	transform!(d, AsTable([:year, :slope]) => (x -> string.(x.year) .* ": slope=" .* string.(x.slope) ) => :year_s)

	cols = range(colorant"red",colorant"blue",length = length(unique(d.year)))
	dd = select(d, :year_s, :region, :larea, :lu)
	pl2 = @df dd plot(:lu,:larea,group = :year_s,
					ylab = L"\log area",
					xlab = L"\log L_u",
					marker = (:circle, 4),
					colour = cols',
					legend = :outertopright,
					# ylims = (-3.8,-2.7),
					# xlims = K == 3 ? (-1.5,-0.8) : (-1.0,-0.5),
					)
	totarea = combine(filter(row -> row.year .== 2010, d), :cityarea => sum)[1,1]
	pl3 = @df d plot(:Lu, :cityarea, group = :region, xlab = "Lu", ylab = "area",m = :circle, series_annotation = Plots.text.(:year, 8, :right),title = "Total Urban area: $(round(totarea,digits=2))")
	pl4 = @df d plot(:Lu, :citydensity, group = :region, xlab = "Lu", ylab = "density",m = :circle, series_annotation = Plots.text.(:year, 8, :right))
	(pl2,pl3,pl4)
end

"time series showing all regions together"
function impl_plot_ts_all(C::Vector{Country})
	df = dataframe(C)
	K = length(C[1].R)
	# vars = (:ρr, :qr, :Lr, :Lu, :wu0, :wr, :Sr, :Srh, :r, :pr, :ϕ, :icu_input, :iDensity, :icu, :icr, :iτ, :iq, :iy)
	# @df df plot(:year, cols(2:size(df,2)))
	s = stack(df, Not([:year, :region]))
	# sims = [(:Lr,:Lu); (:Sr, :ϕ, :Srh); (:qr, :r); (:wr , :wu0)]
	# sims = [[:Lu], [:ϕ], [:pop], [:ρ0], [:q0], [:Srh]]
	# sims = ["Lu", "ϕ", "pop", "ρ0", "q0","Srh", "Sr", "Hr"]
	sims = ["Lu", "ϕ", "pop","dbar"]
	# titles = ["Urban Labor"; "Urban Size"; "Total pop"; "Central Land values"; "Central House prices"; "Rural Housing (Srh)"; "Rural Land (Sr)"; "r Housing supply"]
	titles = ["Urban Labor"; "Urban Size"; "Total pop"; "Avg Density"]

	plts = Any[]
	for i in 1:length(sims)
			x = @linq s |>
				where((:variable .== sims[i]))
			px = @df x plot(:year, :value, group = :region,
							title = titles[i],
							titlefontsize=10,
							legend = i == 1 ? true : false,
							linewidth=2,marker = (:circle,3),
							legendfontsize = 5)
			push!(plts, px)

	end
	# push!(plts,plot())  # fill up with empty
	# push!(plts, scatter((1:3)', xlim = (4,5), legend = true, framestyle = :none, labels = ["1" "2" "3"]))
	# plot(plts...,layout = (1,4))
	plts


end


"single spatial cross sections region"
function plot_space(m::Region,p::Param,it::Int; fixy = false)

	lvec = collect(range(0.,area(m),length=100))  # space for region ik
	setperiod!(p,it)  # set time on parameter!
	plts = Any[]
	if fixy
		push!(plts, plot(lvec, [ϵ(i,m.ϕ,p) for i in lvec],title = L"\epsilon(l)", ylims = (2,4.1), linewidth = 2, leg = false))
		push!(plts, plot(lvec, [D(i,p,m) for i in lvec],title = L"D(l)", ylims = (-3,60), linewidth = 2, leg = false))
		push!(plts, plot(lvec, [H(i,p,m) for i in lvec],title = L"H(l)", ylims = (-0.1,15), linewidth = 2, leg = false))
		push!(plts, plot(lvec, [ρ(i,p,m) for i in lvec],title = L"\rho(l)", ylims = (-0.1,10), linewidth = 2, leg = false))
		push!(plts, plot(lvec, [w(m.Lu,i,m.ϕ,p) for i in lvec],title = L"w(l)", ylims = (-0.1,5.5), linewidth = 2, leg = false))
		# push!(plts, plot(lvec, [h(i,p,m) for i in lvec],title = L"h(l)", ylims = (-0.1,1.5), linewidth = 2, leg = false))
		push!(plts, plot(lvec, [q(i,p,m) for i in lvec],title = L"q(l)", ylims = (0.5,5.5), linewidth = 2, leg = false))
	else
		push!(plts, plot(lvec, [ϵ(i,m.ϕ,p) for i in lvec],title = L"\epsilon(l)",linewidth = 2, leg = false))
		push!(plts, plot(lvec, [D(i,p,m) for i in lvec],title = L"D(l)",linewidth = 2, leg = false))
		push!(plts, plot(lvec, [H(i,p,m) for i in lvec],title = L"H(l)",linewidth = 2, leg = false))
		push!(plts, plot(lvec, [ρ(i,p,m) for i in lvec],title = L"\rho(l)",linewidth = 2, leg = false))
		push!(plts, plot(lvec, [w(m.Lu,i,m.ϕ,p) for i in lvec],title = L"w(l)",linewidth = 2, leg = false))
		# push!(plts, plot(lvec, [h(i,p,m) for i in lvec],title = L"h(l)",linewidth = 2, leg = false))
		push!(plts, plot(lvec, [q(i,p,m) for i in lvec],title = L"q(l)", linewidth = 2, leg = false))

	end

	ti = plot(title = "Spatial Cross-Section in $(p.T[it])", grid = false, showaxis = false, bottom_margin = -30Plots.px)
	pl = plot(ti,plot(plts...,layout = (2,3), link = :x), layout = @layout([A{0.05h}; B]))
	pl
end

"plot repeated spatial cross sections for each region"
function anim_space(C::Vector{Country},pp::Vector{Param})
	K = length(C[1].R)

	anims = Plots.Animation[]
	for ik in 1:K
		plt = Any[]
		p = pp[ik]
		# pl0 = plot(leg = false)
		anim = Animation()

		for (jt,it) in enumerate(C[1].T)
			reg = C[jt].R[ik]
			pl0 = plot_space(reg,p,jt,fixy = true)
			frame(anim,pl0)
		end
		gif(anim, joinpath(dbpath(),"x-section-$ik.gif"),fps=0.5)
		push!(anims,anim)
	end
	anims
end


"single spatial cross sections region"
function plot_rho_space(m::Region,p::Param,it::Int; fixy = false)

	lvec = collect(range(0.,area(m),length=100))  # space for region ik
	setperiod!(p,it)  # set time on parameter!
	plts = Any[]
	if fixy
		push!(plts, plot(lvec, [ϵ(i,m.ϕ,p) for i in lvec],title = L"\epsilon(l)", ylims = (2,4.1), linewidth = 2, leg = false))
		# push!(plts, plot(lvec, [D(i,p,m) for i in lvec],title = L"D(l)", ylims = (-3,60), linewidth = 2, leg = false))
		# push!(plts, plot(lvec, [H(i,p,m) for i in lvec],title = L"H(l)", ylims = (-0.1,15), linewidth = 2, leg = false))
		push!(plts, plot(lvec, [ρ(i,p,m) for i in lvec],title = L"\rho(l)", ylims = (-0.1,10), linewidth = 2, leg = false))
		# push!(plts, plot(lvec, [w(m.Lu,i,m.ϕ,p) for i in lvec],title = L"w(l)", ylims = (-0.1,5.5), linewidth = 2, leg = false))
		# push!(plts, plot(lvec, [h(i,p,m) for i in lvec],title = L"h(l)", ylims = (-0.1,1.5), linewidth = 2, leg = false))
		# push!(plts, plot(lvec, [q(i,p,m) for i in lvec],title = L"q(l)", ylims = (0.5,5.5), linewidth = 2, leg = false))
	else
		push!(plts, plot(lvec, [ϵ(i,m.ϕ,p) for i in lvec],title = L"\epsilon(l)",linewidth = 2, leg = false))
		# push!(plts, plot(lvec, [D(i,p,m) for i in lvec],title = L"D(l)",linewidth = 2, leg = false))
		# push!(plts, plot(lvec, [H(i,p,m) for i in lvec],title = L"H(l)",linewidth = 2, leg = false))
		push!(plts, plot(lvec, [ρ(i,p,m) for i in lvec],title = L"\rho(l)",linewidth = 2, leg = false))
		# push!(plts, plot(lvec, [w(m.Lu,i,m.ϕ,p) for i in lvec],title = L"w(l)",linewidth = 2, leg = false))
		# push!(plts, plot(lvec, [h(i,p,m) for i in lvec],title = L"h(l)",linewidth = 2, leg = false))
		# push!(plts, plot(lvec, [q(i,p,m) for i in lvec],title = L"q(l)", linewidth = 2, leg = false))

	end

	ti = plot(title = "X-Section", grid = false, showaxis = false, bottom_margin = -30Plots.px)
	pl = plot(ti,plot(plts...,layout = (2,1), link = :x), layout = @layout([A{0.05h}; B]))
	pl
end

function TS_impl(s::DataFrame; year = nothing, xlim = nothing,ylim = nothing,tstring = nothing)

	# sims = [[:Lr,:Lu], [:Sr, :ϕ, :Srh], [:qr, :r], [:wr , :wu0]]
	sims = [["Lr","Lu"], ["ϕ"], ["r"], ["wr" , "wu0"]]
	# sims = [[:Lr,:Lu], [:ϕ], [:r], [:q0, :ρ0] , [:qr, :ρr], [:wr , :wu0],[:Hr , :hr]]
	# titles = ["Labor"; "Land"; "Rents"; "Central prices"; "Fringe prices"; "wages"; "Housing at phi"]
	nms = [[L"L_r" L"L_u"], [L"S_r" L"\phi" L"S_{rh}"] , [L"q" L"r"], [L"w_r" L"w_u"]]
	# nms = [[L"L_r" L"L_u"], L"\phi" , L"r", [L"q_0" L"\rho_0"], [L"q_r" L"\rho_r"], [L"w_r" L"w_u = \theta_u"], ["Supply" "Demand"]]
	nms = [[L"L_r" L"L_u"], L"\phi" , L"r", [L"q_0" L"q_r"]]

	plt = Any[]
	if isnothing(year)
		for i in 1:length(sims)
			x = @linq s |>
				where((:variable .∈ Ref(sims[i])))
			px = @df x plot(:year, :value, group = :variable,
							# title = titles[i],
							titlefontsize=10,
							label=nms[i],
							legend = i < 3 ? :bottomright : :topleft,
							linewidth=2,marker = (:circle,3), legendfontsize = 8)
			push!(plt, px)
		end
		# push!(plt,plot())
		# push!(plt,plot())
		# ti = plot(title = "Time Series", grid = false, showaxis = false, bottom_margin = -30Plots.px)
		# pl = plot(ti,plot(plt...,layout = (2,3), link = :x), layout = @layout([A{0.05h}; B]))
		pl = plot(plt...,layout = (2,2), link = :x)
		# return extrema of each subplot
		xlims = [plt[i].subplots[1][:xaxis][:extrema] for i in 1:length(plt)]
		ylims = [plt[i].subplots[1][:yaxis][:extrema] for i in 1:length(plt)]
		xlims = [[xlims[i].emin,xlims[i].emax] for i in 1:length(plt)]
		ylims = [[ylims[i].emin,ylims[i].emax] for i in 1:length(plt)]
		xlims = [(xlims[i][1] .- 0.05*diff(xlims[i])[1],xlims[i][2] .+ 0.05*diff(xlims[i])[1]) for i in 1:length(plt)]
		ylims = [(ylims[i][1] .- 0.05*diff(ylims[i])[1],ylims[i][2] .+ 0.05*diff(ylims[i])[1]) for i in 1:length(plt)]
		return pl,xlims,ylims
	else
		@assert isa(xlim, AbstractArray) && isa(ylim, AbstractArray)
		for i in 1:length(sims)
			x = @linq s |>
				where((:variable .∈ Ref(sims[i])) .& (:year .<= year))
			legpos = (sims[i][1] == :Lr) ? :bottomright : :topleft
			px = @df x plot(:year, :value, group = :variable,
							title = titles[i],
							titlefontsize=10,
							label=nms[i],
							xlims = xlim[i],
							ylims = ylim[i],
							legend = legpos,
							linewidth=2,marker = (:circle,3))
			push!(plt, px)
		end
		if isnothing(tstring)
			ti = "Time Series until $year"
		else
			ti = tstring
		end
		# ti = plot(title = ti, grid = false, showaxis = false, bottom_margin = -30Plots.px)
		# pl = plot(ti,plot(plt...,layout = (2,3), link = :x), layout = @layout([A{0.05h}; B]))
		pl = plot(plt...,layout = (2,2), link = :x)

		return pl
	end

end



function plot_ts(M::Vector{Region},p::Param,it::Int)
	df = dataframe(M,p)
	s = stack(df, Not([:year]))
	# prepare a TS

	ts0,xlims,ylims = TS_impl(s)

	yr = p.T[it]

	LandUse.setperiod!(p,1)
	θ1 = p.θu
	LandUse.setperiod!(p,14)
	θ14 = p.θu

	# make a TS up to period jt: keeping extrema fixed, however, so the plot "grows" nicely
	pl = TS_impl(s, year = yr, xlim = xlims, ylim = ylims , tstring = "first and last period prod: [$θ1,$(round(θ14,digits = 2))]")
	plot(pl)
end

function plot_ts(M::Vector{Region},p::Param)
	df = dataframe(M,p)
	s = stack(df, Not([:year]))
	# prepare a TS
	LandUse.setperiod!(p,1)
	θ1 = p.θu
	LandUse.setperiod!(p,14)
	θ14 = p.θu

	# make a TS up to period jt: keeping extrema fixed, however, so the plot "grows" nicely
	ts0,xlims,ylims = TS_impl(s,tstring = "first and last period prod: [$θ1,$(round(θ14,digits = 2))]")
	ts0
end

function plot_ts_xsect(M::Vector{Region},p::Param,it::Int)
	df = dataframe(M,p)
	s = stack(df, Not([:year]))
	# prepare a TS

	ts0,xlims,ylims = TS_impl(s)

	yr = p.T[it]

	LandUse.setperiod!(p,1)
	θ1 = p.θu
	LandUse.setperiod!(p,14)
	θ14 = p.θu

	# make a TS up to period jt: keeping extrema fixed, however, so the plot "grows" nicely
	ts = TS_impl(s, year = yr, xlim = xlims, ylim = ylims , tstring = "first and last period prod: [$θ1,$(round(θ14,digits = 2))]")

	# make a x-section
	xs = plot_rho_space(M[it],p,it, fixy = true)

	# combine both
	# o = plot(ts, xs, layout = (1,2), size = (1300,500))
	o = plot(ts, xs, layout = @layout([A{0.75w} B]))
	o

end






function single_TS(s::DataFrame, ik::Int ; year = nothing, xlim = nothing,ylim = nothing)
	# sims = [[:Lr,:Lu], [:Sr, :ϕ, :Srh], [:qr, :r], [:wr , :wu0]]
	sims = [[:Lr,:Lu], [:Sr, :ϕ, :Srh], [:r], [:wr , :wu0]]
	titles = ["Labor"; "Land"; "Rents"; "Wages"]
	# nms = [[L"L_r" L"L_u"], [L"S_r" L"\phi" L"S_{rh}"] , [L"q" L"r"], [L"w_r" L"w_u"]]
	nms = [[L"L_r" L"L_u"], [L"S_r" L"\phi" L"S_{rh}"] , [L"r"], [L"w_r" L"w_u"]]

	plt = Any[]
	if isnothing(year)
		for i in 1:length(sims)
			x = @linq s |>
				where((:variable .∈ Ref(sims[i])) .& (:region .== ik))
			px = @df x plot(:year, :value, group = :variable,
							title = titles[i],
							titlefontsize=10,
							label=nms[i],
							legend = :topleft,
							linewidth=2,marker = (:circle,3))
			push!(plt, px)
		end
		ti = plot(title = "Time Series Region $ik", grid = false, showaxis = false, bottom_margin = -30Plots.px)
		pl = plot(ti,plot(plt...,layout = (2,2), link = :x), layout = @layout([A{0.05h}; B]))
		# return extrema of each subplot
		xlims = [plt[i].subplots[1][:xaxis][:extrema] for i in 1:length(plt)]
		ylims = [plt[i].subplots[1][:yaxis][:extrema] for i in 1:length(plt)]
		xlims = [[xlims[i].emin,xlims[i].emax] for i in 1:length(plt)]
		ylims = [[ylims[i].emin,ylims[i].emax] for i in 1:length(plt)]
		xlims = [(xlims[i][1] .- 0.05*diff(xlims[i])[1],xlims[i][2] .+ 0.05*diff(xlims[i])[1]) for i in 1:length(plt)]
		ylims = [(ylims[i][1] .- 0.05*diff(ylims[i])[1],ylims[i][2] .+ 0.05*diff(ylims[i])[1]) for i in 1:length(plt)]
		return pl,xlims,ylims
	else
		@assert isa(xlim, AbstractArray) && isa(ylim, AbstractArray)
		for i in 1:length(sims)
			x = @linq s |>
				where((:variable .∈ Ref(sims[i])) .& (:region .== ik) .& (:year .<= year))
			px = @df x plot(:year, :value, group = :variable,
							title = titles[i],
							titlefontsize=10,
							label=nms[i],
							xlims = xlim[i],
							ylims = ylim[i],
							legend = :topleft,
							linewidth=2,marker = (:circle,3))
			push!(plt, px)
		end
		ti = plot(title = "Time Series Region $ik in $year", grid = false, showaxis = false, bottom_margin = -30Plots.px)
		pl = plot(ti,plot(plt...,layout = (2,2), link = :x), layout = @layout([A{0.05h}; B]))

		return pl
	end

end


function plot_ts_xsect(C::Vector{Country},pp::Vector{Param},ik::Int)

	# prepare a TS
	df = dataframe(C)
	s = stack(df, Not([:year, :region]))
	ts0,xlims,ylims = single_TS(s,ik)  # to get extrema on each subplot right



	# loop over time
	anim = Animation()
	for (jt,it) in enumerate(C[1].T)

		# make a TS up to period jt: keeping extrema fixed, however, so the plot "grows" nicely
		ts = single_TS(s,ik, year = it, xlim = xlims, ylim = ylims )

		# make a x-section
		xs = plot_space(C[jt].R[ik],pp[ik],jt, fixy = true)

		# combine both
		o = plot(ts, xs, layout = (1,2), size = (1300,500))

		# push onto an anim
		frame(anim)

	end
	g = gif(anim, joinpath(dbpath(),"TS-xsection-$ik.gif"),fps=0.5)
	# gif(anim, joinpath(dbpath(),"TS-xsection-$ik.gif"),fps=0.5, variable_palette = true)
	return anim
end


function plot_ts_impl(mm::Vector{Country},pp::Vector{Param},ik::Int,it::Int)

	df = dataframe(mm)
	s = stack(df, Not([:year, :region]))
	ts0,xlims,ylims = LandUse.single_TS(s,ik)  # to get extrema on each subplot right

	single_TS(s,ik, year = it, xlim = xlims, ylim = ylims )

end

function plot_dyn(mm::Vector{Model},p::Param,it::Int)
	plot(plot_dyn_impl(mm,p,it)..., layout = (2,3), link = :x)
end

function plot_dynxs(mm::Vector{Model},p::Param,it::Int)
	dyns = plot_dyn_impl(mm,p,it)
	xs = plot_xs_impl(mm[it],p)
	plot([dyns;xs]..., size = (800,400), layout = @layout [grid(2,3){0.8w} grid(2,1){0.2w}])
end


# plotter for optimization tracking

function traceplot(x::NLsolve.SolverResults,it)

	ft = ftrace(x)
	xt = xtrace(x)
	p1 = plot(ft,title = "Ftrace $it",
	         label = ["wr" "rhor" "Citysize" "Rent" "Land clear" "Urban good"],
			 xlabel = "iteration",
			 legend = :bottomright)
	p2 = plot(xt,title = "xtrace $it",label = ["rho" "phi" "r" "Lr" "pr" "Sr"], xlabel = "iteration",leg = :left)
	# p2 = plot(xt[1:nrows,:],title = "xtrace",label = hcat(["LS" "r" "pr"],reshape(["SR_$i" for i in 1:K],1,K)),xlabel = "iteration")
	pl = plot(p1,p2,layout = (1,2))
	savefig(pl,joinpath(@__DIR__,"..","images","solver_trace$it.pdf"))
end
function countrytraceplot(x::NLsolve.SolverResults,it)

	ft = ftrace(x)
	xt = xtrace(x)
	p1 = plot(ft,title = "Ftrace $it",
	         label = ["Lr/Sr" "r" "pr" "Sr1" "Sr2" "Lu1" "Lu2"],
			 xlabel = "iteration",
			 legend = :bottomright)
	p2 = plot(xt,title = "xtrace $it",label = ["Lr/Sr" "r" "pr" "Sr1" "Sr2" "Lu1" "Lu2"], xlabel = "iteration",leg = :left)
	# p2 = plot(xt[1:nrows,:],title = "xtrace",label = hcat(["LS" "r" "pr"],reshape(["SR_$i" for i in 1:K],1,K)),xlabel = "iteration")
	pl = plot(p1,p2,layout = (1,2))
	savefig(pl,joinpath(@__DIR__,"..","images","country_trace$it.pdf"))
end

function plotsol(x)
	K = Int((length(x[1])-3)/2)
	plot(hcat(x...)',lab = hcat(["LS" "r" "pr"],reshape(["SR_$i" for i in 1:K],1,K),reshape(["Lu_$i" for i in 1:K],1,K)),title="solution paths")
end


# nicolas question about rho vs y
function plot_ts0(M::Vector{Region},p::Param)
	df = dataframe(M,p)
	# vars = (:ρr, :qr, :Lr, :Lu, :wu0, :wr, :Sr, :Srh, :r, :pr, :ϕ, :icu_input, :iDensity, :icu, :icr, :iτ, :iq, :iy)
	# @df df plot(:year, cols(2:size(df,2)))
	s = stack(df, Not(:year))
	sims = [(:Lr,:Lu,:Sr,:pr,:qr,:r); (:wr, :wu0,:U, :icu , :iy); (:ϕ, :iτ , :ρr ,:Srh)]
	nms = [(L"L_r",L"L_u",L"S_r",L"p_r",L"q_r",L"r"); (L"w_r", L"w_u",:U, L"\int c_u(l) D(l) dl" , L"\int w(l) D(l) dl"); (L"\phi",L"i\tau",L"\rho_r",L"S_{rh}")]
	plts = Any[]
	for i in 1:length(sims)
		x = @linq s |>
			where(:variable .∈ Ref(sims[i]))
		px = @df x plot(:year, :value, group = :variable, layout = length(sims[i]),title = reshape(collect(nms[i]),1,length(nms[i])),titlefontsize=10,leg=false,linewidth=2)
		savefig(px,joinpath(dbpath(),"ts_$i.pdf"))
		push!(plts, px)
	end
	return plts

end



function plot_ρ_ts(M::Vector{Region},p::Param)
	plts = Any[]
	push!(plts,plot(p.T,[ M[i].ρr for i in 1:length(p.T)], label="rho_r"))
	push!(plts,plot(p.T,[ pcy(M[i],p) for i in 1:length(p.T)], label="y"))
	push!(plts,plot(p.T,[M[i].ρr / pcy(M[i],p) for i in 1:length(p.T)], label="rho_r/y"))
	pl = plot(plts...,size=(900,450))
	savefig(pl,joinpath(dbpath(),"rho_r_y.pdf"))
	pl
end

function plot_y_ts(M::Vector{Region},p::Param)
	x = hcat([ LandUse.pcy(M[i],p) for i in 1:length(p.T)],
	[ M[i].r for i in 1:length(p.T)],
	[ M[i].iy / p.L for i in 1:length(p.T)],
	[ M[i].wr * M[i].Lr / p.L for i in 1:length(p.T)])

	pl = plot(p.T,x,label = ["agg y" "r" "urban y" "rural y"],legend=:topleft)
	savefig(pl,joinpath(dbpath(),"y-components.pdf"))
	pl
end



# archive

function plot_static2(m::Region,p::Param)
	lvec = collect(range(0.,area(m),length=100))
	xt = round.(collect(range(0.,area(m),length=3)),digits=2)
	plts = Any[]
	push!(plts,plot(lvec,[ϵ(i,m.ϕ,p) for i in lvec], title = "epsilon(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	push!(plts,plot(lvec,[D(i,p,m) for i in lvec], title = "D(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	push!(plts,plot(lvec,[H(i,p,m) for i in lvec], title = "H(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	push!(plts,plot(lvec,[ρ(i,p,m) for i in lvec], title = "rho(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	push!(plts,plot(lvec,[w(m.Lu,i,m.ϕ,p) for i in lvec], title = "w(l)",leg = false, xticks = xt,titlefontsize=10,tickfont=font(6)))
	push!(plts,plot(lvec,[h(i,p,m) for i in lvec], title = "h(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	plot(plts...)
end

function plot_static(m::Model,p::Param)
	lvec = collect(range(0.,1.0,length=100))
	gr()
	xt = [0.0; 0.5;1.0]
	plts = Any[]
	push!(plts,plot(lvec,[τ(i,m.ϕ,p) for i in lvec], title = "tau",leg = false, xticks = xt,titlefontsize=10,tickfont=font(6)))
	push!(plts,plot(lvec,[w(m.Lu,i,m.ϕ,p) for i in lvec], title = "w(l)",leg = false, xticks = xt,titlefontsize=10,tickfont=font(6)))
	push!(plts,plot(lvec,[cu(i,p,m) for i in lvec], title = "cu(l)",leg = false, xticks = xt,titlefontsize=10,xguidefontsize=6))
	# push!(plts,plot(lvec,[cr(i,p,m) for i in lvec], title = "cr(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	push!(plts,plot(lvec,[cost(i,m.ϕ,p) for i in lvec], title = "cost(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	push!(plts,plot(lvec,[q(i,p,m) for i in lvec], title = "q(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	push!(plts,plot(lvec,[ρ(i,p,m) for i in lvec], title = "rho(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	push!(plts,plot(lvec,[h(i,p,m) for i in lvec], title = "h(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	push!(plts,plot(lvec,[H(i,p,m) for i in lvec], title = "H(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	push!(plts,plot(lvec,[D(i,p,m) for i in lvec], title = "D(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	push!(plts,plot(lvec,[utility(i,p,m) for i in lvec], title = "U(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	push!(plts,plot(lvec,[ϵ(i,m.ϕ,p) for i in lvec], title = "epsilon(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	push!(plts,plot(lvec,[χ(i,m.ϕ,p) for i in lvec], title = "chi(l)",leg = false, xticks = xt,titlefontsize=10,guidefontsize=6))
	plot(plts...)
end

function tplot()
	x,C,cpar,par = LandUse.runk()
	a  = LandUse.anim_cross(C,par)
	a
end
