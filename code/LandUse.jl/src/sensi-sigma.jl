
"""
# Elasticity of Substitution Urban and Rural Good

In the baseline model with have an elasticity of sub σ of 1.008.
Here we assess the impact of changing that this value.

"""
function sensitivity_σ(; save = false,readdisk = true, returnplots = true)

    pth = joinpath(dbplots(), "revision$(revision())","sensitivity-sigma")
    mkpath(pth)

    if !readdisk
        d = sensitivity_σ_()
		JLD2.jldsave(joinpath(pth,"data.jld2"); d)
    else
        d = JLD2.load_object(joinpath(pth,"data.jld2"))
    end

	if !returnplots
		return d 
	else
		p = sensitivity_σ_plot(d)
		if save
			for (k,v) in filter(x -> x.first != :Baseline,d)
                export_any_csv(v, joinpath(pth, "$k.csv"))
            end

			for (k,v) in p
				savefig(v, joinpath(pth,"$k.pdf"))
			end
		end
	end
    p
end

function compare_σc()
    d = Dict()
    d["baseline"] =  objective(d2x(malbec_ces()), plot = true, showmoments = true)
    
    p = LandUse.malbec_ces()
    p[:σc] = 1.8
    d["σc1.8"] =  objective(d2x(p), plot = true, showmoments = true)

    p[:σc] = 0.75
    d["σc0.75"] =  objective(d2x(p), plot = true, showmoments = true)

    return d

end

function sensitivity_σ_(; σs = [0.5,2.0])
    d = khet_run(readdisk = true)
    # get counteractuals for all values
    ks = Symbol.(string.("σ=",σs))
    ωp = OrderedDict(zip(ks, σs))

    a = Dict(k => σc_stepper(news = v) for (k,v) in ωp)

    a[:Baseline] = d
    a
end

function sensitivity_σ_plot(d::Dict)
    a = Dict( k => @chain aggregator(v) begin
            subset(:year => leq(2020))
            # transform(:mean_ρr_1840 => (x -> log.(x)) => :mean_ρr_1840)
            end 
        for (k,v) in d)

	def_theme()


    sys = setdiff(collect(keys(a)), [:Baseline])

    pvars = [:ρr_1840,:citydensity_1840,:dr_1840,:Cr_share,:Lrshare_agg, :pr]
    ticks = Dict(:citydensity_1840 => [0.1, 0.2, 0.4, 1],
                 :dr_1840 => [0.04,0.1, 0.2, 0.4, 1],
                 :pr => :auto,
                 :ρr_1840 => [0.3,0.5,1,2],
				 :Lrshare_agg => [0.02, 0.25, 0.5, 0.7],
                 :Cr_share => :auto
                 )
    lims = Dict(:citydensity_1840 => (0,1.05),
                 :dr_1840 => (0,1.05),
                 :Cr_share => :auto,
                 :pr => :auto,
				 :Lrshare_agg => :auto,
                 :ρr_1840 => (0.1,4))
    legs = Dict(:citydensity_1840 => :topright,
                 :dr_1840 => :topright,
                 :pr => :topright,
                 :Cr_share => :topright,
				 :Lrshare_agg => :bottomleft,
                 :ρr_1840 => :topleft)


    scales = Dict(:citydensity_1840 => :log10,
    :dr_1840 => :log10,
    :Cr_share => :identity,
    :pr => :identity,
    :Lrshare_agg => :identity,
    :ρr_1840 => :log10)

	namepref = Dict(:citydensity_1840 => "mean_",
    :pr => "mean_",
    :dr_1840 => "mean_",
    :Cr_share => "",
    :Lrshare_agg => "",
    :ρr_1840 => "mean_")
	
                 
    cold = Dict(:citydensity_1840 => reds(),
                 :dr_1840 => blues(),
                 :ρr_1840 => greens(),
                 :pr => greens(),
                 :Cr_share => greens(),
                 :Lrshare_agg => greens()
                 )
    p = Dict()
    for ip in pvars
        
		ss = Symbol("$(namepref[ip])$(ip)")

        p[ip] = @df a[:Baseline] plot(:year, cols(ss), label = "Baseline", color = cold[ip][1],yaxis = scales[ip], leg = legs[ip], linestyle = :dot, size = panelsizef(),lw = 2,yformatter = x -> string.(round(x,digits = 2)),yticks = ticks[ip])

        @df a[sys[1]] plot!(p[ip],:year, cols(ss), label = "$(sys[1])", color = cold[ip][2],ylims = lims[ip], linestyle = :solid,lw = 2, markershape = :circle)

        @df a[sys[2]] plot!(p[ip],:year, cols(ss), label = "$(sys[2])", color = cold[ip][3],ylims = lims[ip], linestyle = :solid,lw = 2)

		if length(sys) > 2
			@df a[sys[3]] plot!(p[ip],:year, cols(ss), label = "$(sys[3])", color = cold[ip][4],ylims = lims[ip], linestyle = :solid,lw = 2, markershape = :diamond)

		end
    end
    p
end


"""
	sensitivity σ implementation

runs baseline model but with different values for σ. all other values, in particular θs 
are as in the baseline, loaded from disk. 
"""
function σc_stepper(; K = 20, news = 0.75, maxT = 2020)
	@info "σc stepper"

	d = khet_run(readdisk = true)  # get baseline results 
	# p = Param(par = Dict(ω => 0.5))
	p = Param(par = Dict(:T => 1840:10:maxT,:gsr => zeros(K),:gs => zeros(K),:kshare => [1/K for i in 1:K],:factors => ones(K),:K => K,:𝕊 => zeros(60,K)))

	C = HetCountry[]
	edatas = DataFrame[]  # collect estimation input data along the way even if we dont' estiamte the thetas

	σs = range(p.σc,news, length = 5)

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
	
	# evaluate system in period 1
	for i in 1:length(σs)
		p.σc = σs[i]   # code calls this sigmac! Caution!
		@debug "σc=$(p.σc)"

		xx = nlsolve((x,y) -> system!(x,y,p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr),nt2x(sols[i]), autodiff = :forward)
		push!(sols, x2nt(xx.zero,K))
		converged(xx) || error("not converged σ=$(p.σc)")
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


	# do remaining periods
	prog = Progress(length(p.T), desc = "sensitivity σ Model", barglyphs=BarGlyphs("[=> ]"), barlen = 50, color = :green) 

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
	post_proc(C,edatas)
end
