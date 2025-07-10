
"""
# Housing Supply Elasticity

In this experiment, we impose a constant House
price elasticity ϵ equal to 3 in each location. This
lies in the middle of estimated ranges for this parameter.

In the baseline model, ϵ ranges from 2.5 in the center to 5 at the fringe.
"""
function sensitivity_ϵ(;save = false,readdisk = true, returnplots = true)

    pth = joinpath(dbplots(), "revision$(revision())","sensitivity-epsilonr")
    mkpath(pth)


    if !readdisk
        d = Dict(:Baseline => khet_run(readdisk = true),  # never recompute the baseline...
             Symbol("ϵ=3") => ϵ_stepper())
            JLD2.jldsave(joinpath(pth,"data.jld2"); d)
    else
        d = JLD2.load_object(joinpath(pth,"data.jld2"))
    end

	if !returnplots
		return d
	else

		p = sensitivity_ϵ_plot(d)

		if save
			
			for (k,v) in filter(x -> x.first != :Baseline,d)
                export_any_csv(v, joinpath(pth, "$k.csv"))
            end

			for (k,v) in p
				savefig(v, joinpath(pth,"$k.pdf"))
			end
		end
		p
	end
end

function sensitivity_ϵ_plot(d::Dict)
    a = Dict( k => @chain aggregator(v) begin
            subset(:year => leq(2020))
            end 
        for (k,v) in d)

    sys = setdiff(collect(keys(a)), [:Baseline])

	def_theme()

    o = Dict()

    # add exponential decay model
    # compute mean of all distance bins by lu weight
    dbase = @chain d[:Baseline] begin
        subset(:year => ieq(2020))
        combine([:iDensities, :Lu] => ((x,y) -> mean(x,Weights(y))) => :avg_density,
                 :iDensities => (x -> 1:length(x)) => :bin)
    end
    dd1d2 = @chain d[sys[1]] begin
        subset(:year => ieq(2020))
        combine([:iDensities, :Lu] => ((x,y) -> mean(x,Weights(y))) => :avg_density,
        :iDensities => (x -> 1:length(x)) => :bin)
    end
	def_theme(xticks = :auto)

    o[:dens_decay] = @df dbase plot(:bin, :avg_density, linestyle = :dot, color = reds()[1], label = "Baseline", size = panelsizef())
    @df dd1d2 plot!(o[:dens_decay],:bin, :avg_density, linestyle = :solid, color = reds()[2], label = "$(sys[1])",markershape = :circle)


    # o[:density_decay] = @df exp_gradients(d[:Baseline])[1] plot(:bin, :mean_exp, label = "Baseline",linecolor = reds()[1], linestyle = :dot, leg = :topright, size = panelsizef())
    # @df exp_gradients(d[sys[1]])[1] plot!(o[:density_decay],:bin, :mean_exp, label = String(sys[1]),linecolor = reds()[2], linestyle = :solid)

	def_theme()
    # avg density
    o[:citydensity_1840] = @df a[:Baseline] plot(:year, :mean_citydensity_1840, label = "Baseline", linecolor = reds()[1], linestyle = :dot, leg = :topright,yformatter = x -> string.(round(x,digits = 2)),yscale = :log10,yticks = [0.1, 0.2, 0.4, 1], size = panelsizef(),ylims = (0,1.05))
    @df a[sys[1]] plot!(o[:citydensity_1840],:year, :mean_citydensity_1840, label = String(sys[1]), linecolor = reds()[2])

    # urban area and population 
    o[:area_pop] = @df a[:Baseline] plot(:year, :cityarea_agg_1840,label = "Baseline: Urban Area", color = golds()[1], linestyle = :dot, leg = :topleft, size = panelsizef(),yformatter = x -> string.(round(Int,x)),yscale = :log10,yticks = [1,4,10,25,50])
    @df a[:Baseline] plot!(o[:area_pop], :year, :Lu_agg_1840,label = "Baseline: Urban Population",color = reds()[1], linestyle = :dot)
    
    @df a[sys[1]] plot!(o[:area_pop], :year, :cityarea_agg_1840,label = "$(sys[1]): Urban Area", color = golds()[2], linestyle = :solid)
    @df a[sys[1]] plot!(o[:area_pop], :year, :Lu_agg_1840,label = "$(sys[1]): Urban Population", color = reds()[2], linestyle = :solid)
    o
end


"""
sensitivity ϵ implementation
"""
function ϵ_stepper(; K = 20, newϵ = 3.0)
	@info "ϵ stepper"

	d = khet_run(readdisk = true)  # get baseline results 
	# p = Param(par = Dict(σ => 0.5))
	p = Param(par = Dict(:gsr => zeros(K),:gs => zeros(K),:kshare => [1/K for i in 1:K],:factors => ones(K),:K => K,:𝕊 => zeros(60,K)))

	C = HetCountry[]
	edatas = DataFrame[]  # collect estimation input data along the way even if we dont' estiamte the thetas

	# try to get first period solution from baseline

	it = 1 # first period 
	setperiod!(p,it)
	# first period solution from baseline is starting point 
	push!(edatas, areaprice_estim(p))
	
	d0 = @chain d begin
		subset(:year => x -> x .== minimum(x))  # period == 1
		sort(:region)  # just to make sure
		select(:region,:r,:pr,:Sr,:Lu,:Lr,:θu,:θr, :ϕ, :Srh)
	end
 
	sols = NamedTuple[]
	sysouts = NamedTuple[]
	
	x0 = (Sr = d0.Sr,
		  Lu = d0.Lu,
		  Lr = d0.Lr,
		  r = unique(d0.r)[1],
		  pr = unique(d0.pr)[1])
	push!(sols, x0)

	# return nlsolve((x,y) -> system!(x,y,p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr),nt2x(sols[end]), autodiff = :forward)
	
    ups = range(5.0,3.01, length = 3)
    downs = range(p.ϵs,2.99, length = 3)
	
	# evaluate system in period 1
	for i in 1:length(ups)
		p.ϵr = ups[i]
		p.ϵs = downs[i]
		@debug "ϵr=$(p.ϵr), ϵs=$(p.ϵs)"

		xx = nlsolve((x,y) -> system!(x,y,p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr),nt2x(sols[end]), autodiff = :forward)
		push!(sols, x2nt(xx.zero,K))
		converged(xx) || error("not converged ϵr=$(p.ϵr), ϵs=$(p.ϵs)")
	end
    p.ϵr = 3.0
	p.ϵs = 3.0
	p.ϵflat = true
    xx = nlsolve((x,y) -> system!(x,y,p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr),nt2x(sols[end]), autodiff = :forward)
	converged(xx) || error("not converged ϵr=$(p.ϵr), ϵs=$(p.ϵs)")

    push!(sols, x2nt(xx.zero,K))
	
	# get full solutions output
	sysout = system!(zeros(length(nt2x(sols[end]))),nt2x(sols[end]),p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr)
	println(sysout.Lu - sysout.iDensity)
	push!(sysouts, sysout)

	# create first country
	c = HetCountry(p) # with the ϵflat flag on!!
	for ik in 1:K
		c.pp[ik].θr = d0.θr[ik]
		c.pp[ik].θu = d0.θu[ik]
		# only in first period
		
		c.pp[ik].ϕ1 = sysout.ϕ[ik] * c.pp[ik].ϕ1x
	end
	update!(c,sysout,estimateθ = false)
	push!(C,c)

	# do remaining periods
	years = sort(unique(d.year))
	prog = Progress(length(p.T), desc = "ϵ Model", barglyphs=BarGlyphs("[=> ]"), barlen = 50, color = :green) 

	for iy in 2:length(years)

		if iy < 20
			setperiod!(p,iy)
			c = HetCountry(p) 
			push!(edatas, areaprice_estim(p))
			@debug iy
			if iy >= 11
				
				# take convex combinations of thetau and thetar between both time periods 
				θu0 = d0.θu  # last period
				θr0 = d0.θr  # last period

				d0 = @chain d begin
					subset(:year => ieq(years[iy]))  # period == it
					sort(:region)  # just to make sure
					select(:region,:r,:pr,:Sr,:Lu,:Lr,:θu,:θr, :ϕ, :Srh)
				end
				for convex in 0.1:0.1:1
					@debug "convex combo of thetas $convex"
					θu = (1 - convex) .* θu0 .+ convex .* d0.θu
					θr = (1 - convex) .* θr0 .+ convex .* d0.θr
					xx = nlsolve((x,y) -> system!(x,y,p,sum(d0.Lu .+ d0.Lr),θu, θr),nt2x(sols[end]), autodiff = :forward)
					push!(sols, x2nt(xx.zero,K))
					converged(xx) || error("not converged $convex")

				end
			else
				d0 = @chain d begin
					subset(:year => ieq(years[iy]))  # period == it
					sort(:region)  # just to make sure
					select(:region,:r,:pr,:Sr,:Lu,:Lr,:θu,:θr, :ϕ, :Srh)
				end
				xx = nlsolve((x,y) -> system!(x,y,p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr),nt2x(sols[end]), autodiff = :forward)
				push!(sols, x2nt(xx.zero,K))
				converged(xx) || error("not converged $(years[iy])")
			end
			sysout = system!(zeros(length(nt2x(sols[end]))),nt2x(sols[end]),p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr)
			for ik in 1:K
				c.pp[ik].θr = d0.θr[ik]
				c.pp[ik].θu = d0.θu[ik]
				# in all other periods
				c.pp[ik].ϕ1 = C[1].R[ik].ϕ * c.pp[ik].ϕ1x		
			end
			push!(sysouts, sysout)
			update!(c,sysout,estimateθ = false)
			push!(C,c)
			next!(prog)
		end
	end
	post_proc(C,edatas)
end
