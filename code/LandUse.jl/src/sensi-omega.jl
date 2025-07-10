
"""
# Elasticity of Substitution Land and Labor 

In the baseline model with have an elasticity of sub ω of 1.
Here we assess the impact of changing that to 0.25 and 4, respectively.

"""
function sensitivity_ω(; save = false,readdisk = true)

    pth = joinpath(dbplots(), "revision$(revision())","sensitivity-omega")
    mkpath(pth)

    if !readdisk
        d = sensitivity_ω_()
		JLD2.jldsave(joinpath(pth,"data.jld2"); d)
    else
        d = JLD2.load_object(joinpath(pth,"data.jld2"))
    end

    p = sensitivity_ω_plot(d)
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

function sensitivity_ω_(; ωs = [0.33,3])
    d = khet_run(readdisk = true)
    # get counteractuals for all values
    ks = Symbol.(string.("ω=",ωs))
    ωp = OrderedDict(zip(ks, ωs))

    a = Dict(k => ω_stepper(newω = v) for (k,v) in ωp)

    a[:Baseline] = d
    a
end

function sensitivity_ω_plot(d::Dict)
    a = Dict( k => @chain aggregator(v) begin
            subset(:year => leq(2020))
            # transform(:mean_ρr_1840 => (x -> log.(x)) => :mean_ρr_1840)
            end 
        for (k,v) in d)

	def_theme()


    sys = setdiff(collect(keys(a)), [:Baseline])

    pvars = [:cityarea_agg_1840,:ρr_1840,:citydensity_1840,:dr_1840]
    ticks = Dict(:cityarea_agg_1840 => [1,10,25,50,90],
			     :citydensity_1840 => [0.1, 0.2, 0.4, 1],
                 :dr_1840 => [0.04,0.1, 0.2, 0.4, 1],
                 :ρr_1840 => [0.3,0.5,1,2],
                 )
    lims = Dict(:cityarea_agg_1840 => :auto,
	:citydensity_1840 => (0,1.05),
                 :dr_1840 => (0,1.05),
                 :ρr_1840 => (0.1,4),
				 )
    legs = Dict(:cityarea_agg_1840 => :topleft,
	:citydensity_1840 => :topright,
                 :dr_1840 => :topright,
                 :ρr_1840 => :topleft,
				 )


    scales = Dict(:cityarea_agg_1840 => :log10,
	:citydensity_1840 => :log10,
    :dr_1840 => :log10,
    :ρr_1840 => :log10,
	)
                 
    cold = Dict(
		:cityarea_agg_1840 => golds(),
		:citydensity_1840 => reds(),
                 :dr_1840 => blues(),
                 :ρr_1840 => greens(),
                 )


	namepref = Dict(:cityarea_agg_1840 => "",
	:citydensity_1840 => "mean_",
	:dr_1840 => "mean_",
	:ρr_1840 => "mean_",
	)
    p = Dict()
    for ip in pvars
        

		ss = Symbol("$(namepref[ip])$(ip)")


        blab = ip == :cityarea_agg_1840 ? string("Urban Area Baseline:") : "Baseline"
        plab1 = ip == :cityarea_agg_1840 ? string("Urban Area ",sys[1]) : String(sys[1])
        plab2 = ip == :cityarea_agg_1840 ? string("Urban Area ",sys[2]) : String(sys[2])


        p[ip] = @df a[:Baseline] plot(:year, cols(ss), label = blab, color = cold[ip][1],yaxis = scales[ip], leg = legs[ip], linestyle = :dot, size = panelsizef(),lw = 2,yformatter = x -> string.(round(x,digits = 2)),yticks = ticks[ip])

        @df a[sys[1]] plot!(p[ip],:year, cols(ss), label = plab1, color = cold[ip][2],ylims = lims[ip], linestyle = :solid,lw = 2, markershape = :circle)

        @df a[sys[2]] plot!(p[ip],:year, cols(ss), label = plab2, color = cold[ip][3],ylims = lims[ip], linestyle = :solid,lw = 2)
    end


	# retro-fit :cityarea_agg_1840 with population
	ddd = a[:Baseline]
	bbb1 = a[sys[1]]
	bbb2 = a[sys[2]]
	blab = "Urban Pop Baseline"
	plab1 = string("Urban Pop ",sys[1])
	plab2 = string("Urban Pop ",sys[2])

	plot!(p[:cityarea_agg_1840], ddd.year, ddd.Lu_agg_1840, color = reds()[1], ls = :dot, label = blab)
	plot!(p[:cityarea_agg_1840], bbb1.year, bbb1.Lu_agg_1840, color = reds()[2], ls = :solid,label = plab1)
	plot!(p[:cityarea_agg_1840], bbb2.year, bbb2.Lu_agg_1840, color = reds()[3], ls = :solid,label = plab2, markershape = :diamond)
 

    p
end


"""
	sensitivity ω implementation

runs baseline model but with different values for ω. all other values, in particular θs 
are as in the baseline, loaded from disk. uses a stepper to move ω closer to the target
value, starting from value in the baseline.

## keywords

* `flatL` will run exercise will keeping L flat at initial period.
"""
function ω_stepper(; K = 20, newω = 0.25, maxT = 2020, flatL = false)
	@info "ω stepper"

	d = khet_run(readdisk = true)  # get baseline results 
	# d = khet_run(readdisk = false, par = Dict(:σc => 1.000001))  # get baseline results 
	# p = Param(par = Dict(ω => 0.5))
	p = Param(par = Dict(:T => 1840:10:maxT,:gsr => zeros(K),:gs => zeros(K),:kshare => [1/K for i in 1:K],:factors => ones(K),:K => K,:𝕊 => zeros(60,K), :Lflat => flatL))

	C = HetCountry[]
	edatas = DataFrame[]  # collect estimation input data along the way even if we dont' estiamte the thetas

	up = newω > 1

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
	
	if up
		ωs = range(2.0,newω, length = 4)
	else
		ωs = range(0.75,newω, length = 4)
	end
	# evaluate system in period 1
	for i in 1:length(ωs)
		p.σ = ωs[i]   # code calls this still sigma! Caution!
		@debug "ω=$(p.σ)"

		xx = nlsolve((x,y) -> system!(x,y,p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr),nt2x(sols[i]), autodiff = :forward)
		push!(sols, x2nt(xx.zero,K))
		converged(xx) || error("not converged ω=$(p.σ)")
	end

	c = HetCountry(p) 
	
	# get full solutions output
	sysout = system!(zeros(length(nt2x(sols[end]))),nt2x(sols[end]),p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr)
	push!(sysouts, sysout)
	for ik in 1:K
		c.pp[ik].θr = d0.θr[ik]
		c.pp[ik].θu = d0.θu[ik]
		# only in first period
		
		c.pp[ik].ϕ1 = sysout.ϕ[ik] * c.pp[ik].ϕ1x
	end
	update!(c,sysout,estimateθ = false)
	push!(C,c)

	years = unique(d.year)

	if flatL
		@info "running with flat population growth"
	end

	# do remaining periods
	prog = Progress(length(p.T), desc = "sensitivity ω Model", barglyphs=BarGlyphs("[=> ]"), barlen = 50, color = :green) 

	for iy in 2:length(p.T)

		setperiod!(p,iy)
		c = HetCountry(p) 
		push!(edatas, areaprice_estim(p))
		@debug iy
		if iy >= 11 && iy < 20
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
				xx = nlsolve((x,y) -> system!(x,y,p,flatL ? sum(x0.Lu .+ x0.Lr) : sum(d0.Lu .+ d0.Lr),θu, θr),nt2x(sols[end]), autodiff = :forward)
				push!(sols, x2nt(xx.zero,K))
				converged(xx) || error("not converged $convex")
			end
		else
			d0 = @chain d begin
				subset(:year => ieq(years[iy]))  # period == it
				sort(:region)  # just to make sure
				select(:region,:r,:pr,:Sr,:Lu,:Lr,:θu,:θr, :ϕ, :Srh)
			end
			xx = nlsolve((x,y) -> system!(x,y,p,flatL ? sum(x0.Lu .+ x0.Lr) : sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr),nt2x(sols[end]), autodiff = :forward)
			push!(sols, x2nt(xx.zero,K))
			converged(xx) || error("not converged $(years[iy])")
		end
		sysout = system!(zeros(length(nt2x(sols[end]))),nt2x(sols[end]),p,flatL ? sum(x0.Lu .+ x0.Lr) : sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr)
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

	post_proc(C,edatas)
end
