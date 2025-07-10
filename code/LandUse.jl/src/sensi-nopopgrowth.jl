"""
Run model without population growth

https://github.com/floswald/LandUse.jl/issues/129

same as [issue 90](https://github.com/floswald/LandUse.jl/issues/90) but for K cities.

also investigates role of low ω in this setting. Therefore use functionality in 
[`sensitivity_ω`](@Ref)

does re-estimation: no.
"""
function sensitivity_nopopgrowth(;newω = 0.5, save = false, readdisk = true)

    pth = joinpath(dbplots(), "revision$(revision())","sensitivity-nopopgrowth")
    mkpath(pth)

    if !readdisk
        d = sensitivity_nopopgrowth_impl(20,newω = newω)
        JLD2.jldsave(joinpath(pth,"data.jld2"); d)
    else
        d = JLD2.load_object(joinpath(pth,"data.jld2"))
    end

    pω = sensitivity_nopopgrowth_plot(filter(x -> occursin("ω",x.first),d))
    p  = sensitivity_nopopgrowth_plot(filter(x -> !occursin("ω",x.first),d))
    if save
        for (k,v) in p
			savefig(v, joinpath(pth,"$k.pdf"))
        end
        for (k,v) in pω
			savefig(v, joinpath(pth,"$k-ω=$(newω).pdf"))
        end
    end
    p,pω
end


function sensitivity_nopopgrowth_impl(K; newω = 0.5)

    # baseline
    # baseline with ω = 0.5
    # baseline without L growth
    # baseline without L growth and with ω = 0.5

    b = khet_run(readdisk = true)

    # get counteractuals for all values
    p0 = Dict(:gsr => zeros(K),:gs => zeros(K),:kshare => [1/K for i in 1:K],:factors => ones(K),:K => K,:𝕊 => zeros(60,K), :Lflat => true, :T => 1840:10:2200)

    # sigma at abaseline
    p = Param(par = p0)
  
    @info "no population growth in baseline"
    bnol = runk_impl_nopopgrowth(p,b,overdrive = 2)

    @info "baseline with ω=$(newω)"
    bsig = ω_stepper(K = 20, newω = newω , maxT = 2020)

    @info "no population growth in baseline with ω=$(newω)"
    # p.ω = newω
    # p.T = 1840:10:2020
    bsignol = ω_stepper(K = 20, newω = newω,maxT = 2020, flatL = true)

    d = Dict("Baseline" => b,
             "no L growth" => bnol,
             "ω=$(newω)" => bsig,
             "ω=$(newω), no L growth" => bsignol
             )


    return d
end

function sensitivity_nopopgrowth_plot(di::Dict)


    # aggregate all datasets 
    aa = Dict( k => aggregator(v) for (k,v) in di)
    a = Dict( k => @chain v begin
                subset(:year => leq(2020))
                end 
        for (k,v) in aa)

    def_theme()

    ks = collect(keys(a))
    nol = findall(occursin.(r"growth", ks) .== 1)
    base = findall(occursin.(r"growth", ks) .== 0)
    
    # make 3 plots 
    # return a
    # rural emp share, city size and density for baseline vs both counterfactuals
    pvars = [:Lrshare_agg,:cityarea_agg_1840,:citydensity_1840,:HPI_1840]
    ticks = Dict(:cityarea_agg_1840 => :auto,
                 :citydensity_1840 => [0.1, 0.2, 0.4, 1],
                 :HPI_1840 => [100,150,200,250],
                 :Lrshare_agg => :auto
                 )
    lims = Dict(:cityarea_agg_1840 => :auto,
                 :citydensity_1840 => (0,1.3),
                 :HPI_1840 => (90,Inf),
                 :Lrshare_agg => (0,Inf)
                 )
    cold = Dict(:cityarea_agg_1840 => golds(),
                 :citydensity_1840 => reds(),
                 :HPI_1840 => blues(),
                 :Lrshare_agg => greens()
                 )
    namepref = Dict(:cityarea_agg_1840 => "",
                 :citydensity_1840 => "mean_",
                 :HPI_1840 => "mean_",
                 :Lrshare_agg => ""
                 )
    scales = Dict(:cityarea_agg_1840 => :log10,
                 :citydensity_1840 => :log10,
                 :HPI_1840 => :identity,
                 :Lrshare_agg => :identity
                 )
    legs = Dict(:cityarea_agg_1840 => :topleft,
                 :citydensity_1840 => :topleft,
                 :HPI_1840 => :topleft,
                 :Lrshare_agg => :topright
                 )
    p = Dict()
    for ip in pvars
        
        ss = Symbol("$(namepref[ip])$(ip)")
        blab = ip == :cityarea_agg_1840 ? string("Urban Area ",ks[base][1]) : ks[base][1]
        plab = ip == :cityarea_agg_1840 ? string("Urban Area ",ks[nol][1]) : ks[nol][1]
        p[ip] = @df a[ks[base][1]] plot(:year, cols(ss), label = blab, color = cold[ip][1],yscale = scales[ip], yticks = ticks[ip],
        yformatter = ((ip == :HPI_1840) | (ip == :cityarea_agg_1840)) ? x -> string.(floor(Int,x)) : x -> string.(round(x,digits = 2)),
        ylims = lims[ip],leg = legs[ip], linestyle = :dot, size = panelsizef(),  xticks = 1840:20:2020)

        @df a[ks[nol][1]] plot!(p[ip],:year, cols(ss), label = plab, color = cold[ip][2],ylims = lims[ip], linestyle = :solid)

        # @df a[:no_L_growth_ω] plot!(p[ip],:year, cols(ss), label = "No L Growth, ω=0.5", color = cold[ip][3],ylims = lims[ip], linestyle = :dash, lw = 2)
    end

    # retro-fit :cityarea_agg_1840 with population
    ddd = a[ks[base][1]]
    bbb= a[ks[nol][1]]
    blab = string("Urban Pop ",ks[base][1])
    plab = string("Urban Pop ",ks[nol][1])

    plot!(p[:cityarea_agg_1840], ddd.year, ddd.Lu_agg_1840, color = reds()[1], ls = :dot, label = blab)
    plot!(p[:cityarea_agg_1840], bbb.year, bbb.Lu_agg_1840, color = reds()[2], ls = :solid,label = plab)


    # add population growth 
    p[:population] = @df subset(aa[ks[base][1]], :year => leq(2100)) plot(:year, :pop_agg ./ 20, label = ks[base][1], color = blues()[1],ls = :solid,leg = false, yscale = :log10, yticks = [1,1.5,2,2.5],yformatter = x -> string.(round(x,digits = 2)), xticks = [1840,1900,2000,2100])

    # p[:population] = @df aa[ks[nol][1]] plot!(p[:population],:year, :pop_agg, label = ks[nol][1], color = blues()[2],linestyle =:solid,lw = 2)


    p

end


# goes until 2020
function runk_impl_nopopgrowth(p::Param,baselinedf::DataFrame; overdrive = 2)
	sols = NamedTuple[]
	objs = Float64[]
	edatas = DataFrame[]
	# ϕvs =Vector{Float64}[]
	# dϕvs =Vector{Float64}[]

	# get starting point from baseline
	d0 = @chain baselinedf begin
		subset(:year => x -> x .== minimum(x))  # period == it
		sort(:region)  # just to make sure
		select(:region,:r,:pr,:Sr,:Lu,:Lr,:θu,:θr, :ϕ, :Srh)
	end
	x0 = (Sr = d0.Sr,
		  Lu = d0.Lu,
		  Lr = d0.Lr,
		  r = unique(d0.r)[1],
		  pr = unique(d0.pr)[1])
	push!(sols, x0)

	# push!(sols,x0)
	C = HetCountry[]

	years = sort(unique(baselinedf.year))
    ϕs = zeros(p.K)


	prog = Progress(length(p.T), desc = "no pop growth Model", barglyphs=BarGlyphs("[=> ]"), barlen = 50, color = :green) 
	# @showprogress "Country Model" for it in 1:length(p.T)
	for it in 1:length(p.T)
		@debug it
		setperiod!(p,it)
		c = HetCountry(p) 
		push!(edatas, areaprice_estim(p))
		
		if it < overdrive || it > 20
			d0 = @chain baselinedf begin
				subset(:year => ieq(years[it]))  # period == it
				sort(:region)  # just to make sure
				select(:region,:r,:pr,:Sr,:Lu,:Lr,:θu,:θr, :ϕ, :Srh)
			end
            for ik in 1:p.K
                # println("k = $ik, t = $it, idx = $idx")
                c.pp[ik].θr = d0.θr[ik]
                c.pp[ik].θu = d0.θu[ik]
            end
			# xx = nlsolve((x,y) -> system!(x,y,p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr),nt2x(sols[end]), autodiff = :forward)
			# push!(sols, x2nt(xx.zero,K))
            # xx = jumpsystem(sols[end],p,sum(x0.Lu .+ x0.Lr),d0.θu, d0.θr)  # notice population fixed at first period!
			# push!(sols, xx[1])
            xx = jc(c,sols[end], estimateθr = false, estimateθu = false)
            # xx = jumpsystem(sols[end],p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr)
			push!(sols, xx[1])
            ϕs[:] = xx[2]  # store phis

            # println(xx[1].U)
			# converged(xx) || error("not converged $(years[it])")
		else
			@warn "turning on 4x4 drive 🚚" maxlog=1
			# take convex combinations of thetau and thetar between both time periods 
			θu0 = d0.θu  # last period
			θr0 = d0.θr  # last period
			d0 = @chain baselinedf begin
				subset(:year => ieq(years[it]))  # period == it
				sort(:region)  # just to make sure
				select(:region,:r,:pr,:Sr,:Lu,:Lr,:θu,:θr, :ϕ, :Srh)
			end

            tvals = if it > 10
                # [range(0.01, 0.95, 20)..., range(0.95, 1, 100)...]
                # range(0.01, 0.95, 40)
                range(0.01, 1, 10)
            else
                range(0.01, 1, 10)
            end
			for convex in tvals
			# for convex in range(0.1,1, length = 10)
				θu = (1 - convex) .* θu0 .+ convex .* d0.θu
				θr = (1 - convex) .* θr0 .+ convex .* d0.θr
                @debug "convex combo of thetas $convex" θu=θu θr=θr

                for ik in 1:p.K
                    # println("k = $ik, t = $it, idx = $idx")
                    c.pp[ik].θr = θr[ik]
                    c.pp[ik].θu = θu[ik]
                end

				# xx = nlsolve((x,y) -> system!(x,y,p,sum(d0.Lu .+ d0.Lr),θu, θr),nt2x(sols[end]), autodiff = :forward)
				# push!(sols, x2nt(xx.zero,K))
                xx = jc(c,sols[end], estimateθr = false, estimateθu = false )

			    push!(sols, xx[1])
                ϕs[:] = xx[2]  # store phis

				# converged(xx) || error("not converged $convex")
			end
		end
		# sysout = system!(zeros(length(nt2x(sols[end]))),nt2x(sols[end]),p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr)
		# update!(c,sysout,estimateθ = false)
        if it == 1
			for ik in 1:p.K
				c.pp[ik].ϕ1 = ϕs[ik] * c.pp[ik].ϕ1x
			end
		else
			for ik in 1:p.K
				c.pp[ik].ϕ1 = C[1].R[ik].ϕ * c.pp[ik].ϕ1x
			end
		end
		update!(c,sols[end],estimateθ = false)
		push!(C,c)
        next!(prog)
	end
	post_proc(C,edatas)
end

