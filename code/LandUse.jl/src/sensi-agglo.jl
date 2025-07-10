
# TODO needs full re-estimation of the model fitting aggregate data
# expect commuting cost base parameter a to change significantly.


"""
# Agglomeration and Congestion forces

We add agglomeration forces such that the urban producitivity becomes 
a function of urban employment.

Similarly, we add congestion costs such that the level of commuting costs
becoems a function of the urban employment.

In both instances, take the baseline estimates but we adjust parameter `a` until final city size is as in the data (17.3% of rural area).
"""
function sensitivity_agglo(;save = false, readdisk = true, λ = 0.05, μ = 0.05, returnplots = true)

    pth = joinpath(dbplots(), "revision$(revision())","sensitivity-agglomeration")
    mkpath(pth)
    
    if !readdisk
        d = agglo_congest(λ = λ, μ = μ)
        JLD2.jldsave(joinpath(pth,"data.jld2"); d)
    else
        d = JLD2.load_object(joinpath(pth,"data.jld2"))
    end

    if !returnplots
        return d
    else
        dc = filter(x -> x.first ∈ [:Baseline, :Congestion], d)
        pa = agglo_plots(d, λ)
        pc = agglo_plots(dc, λ)

        if save

            for (k,v) in filter(x -> x.first != :Baseline,d)
                export_any_csv(v, joinpath(pth, "$k.csv"))
            end

            for (k,v) in pa
                savefig(v, joinpath(pth,replace("$k.pdf"," " => "_")))
            end
            for (k,v) in pc
                savefig(v, joinpath(pth,"congest-$k.pdf"))
            end

        end
        Dict(:agglomeration => pa, :congestion => pc)
    end
end

function agglo_congest( ; λ = 0.05, μ = 0.05, estimateθ = true)
    # get the baseline 
    d = khet_run(readdisk = true)

    if estimateθ
        # experiment number 1 (equation (2))
        @info "λ irrelevant"

        irrelevant = khet_estimate_impl(20, par = merge(Dict(:λ => λ,:T => 1840:10:2150)),irrelevantλ = true,iterateP = 0)
        
        # with correct lambda in constraints
        @info "λ relevant - aggregate effect"
        relevant = khet_estimate_impl(20, par = merge(Dict(:λ => λ,:T => 1840:10:2150, :a => 1.75)),irrelevantλ = false,iterateP = 0)

        congest = khet_estimate_impl(20, par = Dict(:μ => μ, :a => 1.6,:T => 1840:10:2150),iterateP = 0)
    else 

        θdf = select(d, :region, :year, :θu => :θukt_new, :θr => :θrkt_new, :it)

        @info "λ irrelevant"

        irrelevant = khet_estimate_impl(20, par = merge(Dict(:λ => λ,:T => 1840:10:2150)),irrelevantλ = true,iterateP = 0, estimateθu = false, estimateθr = true, θdf = θdf,verbose = true)
        
        # with correct lambda in constraints
        @info "λ relevant - aggregate effect"
        relevant = khet_estimate_impl(20, par = merge(Dict(:λ => λ,:T => 1840:10:2150, :a => 1.75)),irrelevantλ = false,iterateP = 0, estimateθu = false, estimateθr = false, θdf = θdf)

        congest = khet_estimate_impl(20, par = Dict(:μ => μ, :a => 1.6,:T => 1840:10:2150),iterateP = 0, estimateθu = false, estimateθr = false, θdf = θdf)

    end

    di = OrderedDict(:Baseline => d, :Agglomeration => irrelevant, Symbol("Aggregate Effect") => relevant, :Congestion => congest)
    return di

end


function agglo_plots(d::OrderedDict, λ::Float64)

    # aggregate all simulations
    # aggregate all datasets 
    a = OrderedDict( k => @chain aggregator(v) begin
            subset(:year => leq(2020))
            end 
        for (k,v) in d)
            # return a
    
    # report mean city size
    for (k,v) in a
        ia = @chain v begin
            subset(:year => ieq(2010))
        end
        @info "avg city size $(k) in 2010 = $(ia.rel_cityarea)"
    end

    # * city area

    def_theme()

    # * avg urban density
    # * central density
    # * rural goods price
    # * real HPI normalized to 1870.
    # * rental price of farmland

    sys = setdiff(collect(keys(a)), [:Baseline])
    ns = length(sys)

    
    # make 3 plots 
    # return a
    # avd_n, dr_1840, ℙ_n for baseline vs both counterfactuals
    pvars = [:cityarea_agg_1840,:citydensity_1840,:d0_1840,:dr_1840,:pr_1840,:pr,:ρr_1840,:ρr,:HPI_1840]
    
    # pvars = [:cityarea_agg_1840,:avgd_n,:d0_1840,:pr_1840]
    ticks = Dict(:cityarea_agg_1840 => [1,4,10,25,50],
                 :citydensity_1840 => ns > 1 ? [1, 0.4, 0.2,0.1] : [1, 0.4, 0.2,0.11],
                 :d0_1840 => ns > 1 ? [0.2, 0.3, 0.5, 1] : [0.21,0.3, 0.5, 1],
                 :pr_1840 => 0.1:0.1:1,
                 :dr_1840 => [0.04,0.1, 0.2, 0.4, 1],
                 :pr => 0.1:0.1:1,
                 :ρr_1840 =>  ns > 1 ? [ 0.8, 1,1.5] : [ 0.8, 1,1.3],
                 :ρr => :auto,
                 :HPI_1840 => 100:50:300
                 )
    scales = Dict(:cityarea_agg_1840 => :log10,
        :citydensity_1840 => :log10,
        :d0_1840 => :log10,
        :dr_1840 => :log10,
        :pr_1840 => :identity,
        :pr => :identity,
        :ρr_1840 => :log10,
        :ρr => :log10,
        :HPI_1840 => :identity
        )
    lims = Dict(:cityarea_agg_1840 => :auto,
        :citydensity_1840 => :auto,
        :d0_1840 => :auto,
        :dr_1840 => :auto,

        :pr_1840 => (0,1.05),
        :pr => (0,Inf),
        :ρr_1840 => :auto,
        :ρr => :auto,
        :HPI_1840 => (90,Inf)
        )
    cold = Dict(:cityarea_agg_1840 => golds(),
                :citydensity_1840 => reds(),
                :d0_1840 => golds(),
                :dr_1840 => blues(),
                :ρr_1840 => greens(),
                :ρr => greens(),
                :pr_1840 => greens(),
                :pr => greens(),
                :HPI_1840 => reds()
                 )
    legs = Dict(:cityarea_agg_1840 => :topleft,
                 :citydensity_1840 => :bottomleft,
                 :d0_1840 => :bottomleft,
                 :pr_1840 => :bottomleft,
                 :pr => :bottomleft,
                 :dr_1840 => :topright,
                 :ρr_1840 => :bottomright,
                 :ρr => :bottomleft,
                 :HPI_1840 => :topleft
                  )
    namepref = Dict(:cityarea_agg_1840 => "",
    :citydensity_1840 => "mean_",
    :d0_1840 => "mean_",
    :dr_1840 => "mean_",
    :pr_1840 => "mean_",
    :pr => "mean_",
    :ρr_1840 => "mean_",
    :ρr => "mean_",
    :HPI_1840 => "mean_"
    )


    p = Dict()
    for ip in pvars
        
        ss = Symbol("$(namepref[ip])$(ip)")
        # println(ss)

        p[ip] = @df a[:Baseline] plot(:year, cols(ss), label = "Baseline", color = cold[ip][1],yscale = scales[ip], yticks = ticks[ip],
        yformatter = HPI_fmt(ip),
        ylims = lims[ip],linestyle = :dot, size = panelsizef(), leg = legs[ip], xticks = 1840:20:2020)

        @df a[sys[1]] plot!(p[ip],:year, cols(ss), label = "$(sys[1])", color = cold[ip][2],ylims = lims[ip], linestyle = :solid, markershape = :circle)

        if ns > 1
            @df a[sys[2]] plot!(p[ip],:year, cols(ss), label = "$(sys[2])", color = cold[ip][3],ylims = lims[ip], linestyle = :solid)
        end
    end
    # retro-fit :cityarea_agg_1840 with population
    ddd = a[:Baseline]
    bbb1 = a[sys[1]]
    blab = "Urban Pop Baseline"
    plab1 = string("Urban Population")
    if ns > 1
        bbb2 = a[sys[2]]
        plab2 = string("Urban Pop ",sys[2])
    end

    plot!(p[:cityarea_agg_1840], ddd.year, ddd.Lu_agg_1840, color = reds()[1], ls = :dot, label = "")
    plot!(p[:cityarea_agg_1840], bbb1.year, bbb1.Lu_agg_1840, color = reds()[1], ls = :solid,label = plab1)
    if ns > 1
        plot!(p[:cityarea_agg_1840], bbb2.year, bbb2.Lu_agg_1840, color = reds()[1], ls = :solid,label = "")
    end

    if length(sys) == 1  # mu case

        # plot thetau and thetar counterfactual vs baseline
        pθu = plot_diagnostic_thetas(d[:Baseline], d[sys[1]], sys[1])

        pθuagg = @df a[:Baseline] plot(:year, :mean_θu, yticks = 1:2:15, color = :red, label = "baseline",lw = 1)
        plot!(pθuagg, a[sys[1]].year, a[sys[1]].mean_θu , color = :blue, label = string(sys[1]),lw = 1)

        pθragg = @df a[:Baseline] plot(:year, :mean_θr, color = :red, label = "baseline",lw = 1)
        plot!(pθragg, a[sys[1]].year, a[sys[1]].mean_θr , color = :blue, label = string(sys[1]),lw = 1)



        for (k,v) in pθu
            p["diag_$k"] = v
        end

    else # then we have 2 lambda cases and need to carefully aggregate thetas

        pθ1 = plot_diagnostic_thetas(d[:Baseline], d[sys[1]],sys[1])
        pθ2 = plot_diagnostic_thetas(d[:Baseline], d[sys[2]],sys[2])
        for (k,v) in pθ1
            p["diag_$k"] = v
        end
        for (k,v) in pθ2
            p["diag_$k"] = v
        end

        # 2 lambda cases

        # \lambda irrelevant "aggleration" :edata.popwgt[ik] * θu[ik] * (Lu[ik])^p.λ
        # \lambda relevant (agg effect):  (edata.popwgt[ik])^(1 + p.λ) * θu[ik]
        
        # recompute aggregations. 
        λagglo = @chain d[:Agglomeration] begin
            subset(:year => leq(2020))
            groupby(:year)
            combine([:θu, :Lu] => ((x,y) -> sum( x .* (y ./ sum(y)) .* y .^ λ )) => :θu)    
        end

        λaggre = @chain d[Symbol("Aggregate Effect")] begin
            subset(:year => leq(2020))
            groupby(:year)
            combine([:θu, :Lu] => ((x,y) -> sum( x .* ((y ./ sum(y)) .^ (1 + λ) )))  => :θu)    
        end
        

        pθuagg = @df a[:Baseline] plot(:year, :mean_θu, yticks = 1:2:15, color = :red, label = "baseline",lw = 1)
        plot!(pθuagg, λagglo.year, λagglo.θu , color = :blue, label = "Agglomeration",lw = 1)
        plot!(pθuagg, λaggre.year, λaggre.θu , color = :green, label = "Aggregate Effect",lw = 1)

        pθragg = @df a[:Baseline] plot(:year, :mean_θr, color = :red, label = "baseline",lw = 1)
        plot!(pθragg, a[sys[1]].year, a[sys[1]].mean_θr , color = :blue, label = string(sys[1]),lw = 1)
        plot!(pθragg, a[sys[2]].year, a[sys[2]].mean_θr , color = :green, label = string(sys[2]),lw = 1)



    end

    p["diag_θuagg"] = pθuagg
    p["diag_θragg"] = pθragg
    p

end


# # goes until 2020
# function runk_impl_agglo(p::Param,θrdf::DataFrame; overdrive = 4)
# 	sols = NamedTuple[]
# 	objs = Float64[]
# 	edatas = DataFrame[]
# 	# ϕvs =Vector{Float64}[]
# 	# dϕvs =Vector{Float64}[]

# 	# get starting point from baseline
# 	d0 = @chain θrdf begin
# 		subset(:year => x -> x .== minimum(x))  # period == it
# 		sort(:region)  # just to make sure
# 		select(:region,:r,:pr,:Sr,:Lu,:Lr,:θukt_new => :θu,:θrkt_new => :θr, :ϕ, :Srh)
# 	end
# 	x0 = (Sr = d0.Sr,
# 		  Lu = d0.Lu,
# 		  Lr = d0.Lr,
# 		  r = unique(d0.r)[1],
# 		  pr = unique(d0.pr)[1])
# 	push!(sols, x0)

# 	# push!(sols,x0)
# 	C = HetCountry[]

# 	years = sort(unique(θrdf.year))

#     ϕs = zeros(p.K)

# 	prog = Progress(length(p.T), desc = "agglo/congest Country Model", barglyphs=BarGlyphs("[=> ]"), barlen = 50, color = :green) 
# 	# @showprogress "Country Model" for it in 1:length(p.T)
# 	for it in 1:length(p.T)
# 		println(it)
# 		setperiod!(p,it)
# 		c = HetCountry(p) 
# 		push!(edatas, areaprice_estim(p))

#         for ik in 1:p.K
#             idx = (θrdf.it .== it) .&  (θrdf.region .== ik)
#             # println("k = $ik, t = $it, idx = $idx")
#             c.pp[ik].θr = θrdf.θrkt_new[idx][1]
#             c.pp[ik].θu = θrdf.θukt_new[idx][1]
#         end

		
# 		if (it < overdrive) || (it > 19)
# 			d0 = @chain θrdf begin
# 				subset(:year => ieq(years[it]))  # period == it
# 				sort(:region)  # just to make sure
# 				select(:region,:r,:pr,:Sr,:Lu,:Lr,:θukt_new => :θu,:θrkt_new => :θr, :ϕ, :Srh)
# 			end
# 			# xx = nlsolve((x,y) -> system!(x,y,p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr),nt2x(sols[end]), autodiff = :forward)
# 			# push!(sols, x2nt(xx.zero,K))
#             xx = jc(c,sols[end], estimateθr = true, estimateθu = true ,constr_viol_tol = nothing)
#             # xx = jumpsystem(sols[end],p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr,optimizer = Ipopt.Optimizer)
# 			push!(sols, xx[1])
#             # println(xx[1].U)
#             ϕs[:] = xx[2]  # store phis
# 			# converged(xx) || error("not converged $(years[it])")
# 		else
# 			@info "period $it turning on 4x4 drive 🚚"
# 			# take convex combinations of thetau and thetar between both time periods 
#             # d0.θu[1] = d0.θu[1] * 0.95
# 			θu0 = d0.θu  # last period
# 			θr0 = d0.θr  # last period
# 			d0 = @chain θrdf begin
# 				subset(:year => ieq(years[it]))  # period == it
# 				sort(:region)  # just to make sure
# 				select(:region,:r,:pr,:Sr,:Lu,:Lr,:θukt_new => :θu,:θrkt_new => :θr, :ϕ, :Srh)
# 			end
#             # d0.θu[1] = d0.θu[1] * 0.95

#             tvals = if it > 4
#                 range(0.0, 1, 10)
#             else
#                 range(0.0, 1, 5)
#             end
# 			for convex in tvals
# 			# for convex in range(0.1,1, length = 10)
# 				println("convex combo of thetas $convex")
# 				θu = (1 - convex) .* θu0 .+ convex .* d0.θu
# 				θr = (1 - convex) .* θr0 .+ convex .* d0.θr
#                 # fill into country object
#                 for ik in 1:p.K
#                     # idx = (θrdf.it .== it) .&  (θrdf.region .== ik)
#                     # println("k = $ik, t = $it, idx = $idx")
#                     c.pp[ik].θr = θr[ik]
#                     c.pp[ik].θu = θu[ik]
#                 end
#                 # period 6 is hard, so relax constraint violation constraint to 0.001
#                 # xx = jc(c,sols[end], estimateθr = false, estimateθu = false ,lbL = 0.05, constr_viol_tol = it == 6 ? 1e-3 : nothing)
#                 # xx = jc(c,sols[end], estimateθr = false, estimateθu = false ,constr_viol_tol = it == 4 ? 1e-3 : 1e-4 )
#                 # lb = if it == 5
#                 #     0.05
#                 # elseif it == 6
#                 #     0.0
#                 # else
#                 #     0.05
#                 # end
#                 xx = jc(c,sols[end], estimateθr = true, estimateθu = true )
#                 # xx = jc(c,sols[end], estimateθr = false, lbL = it == 18 ? 0.001 : 0.05 )
#                 # xx = jumpsystem(sols[end],p,sum(d0.Lu .+ d0.Lr),θu, θr)

#                 if length(xx) == 1
#                     return (xx[1],JuMP.primal_feasibility_report(xx[1]))
#                     # return xx[1]
#                 else
# 			        push!(sols, xx[1])
#                 end
#                 ϕs[:] = xx[2]  # store phis
# 			end
# 		end

#         if it == 1
# 			for ik in 1:p.K
# 				c.pp[ik].ϕ1 = ϕs[ik] * c.pp[ik].ϕ1x
# 			end
# 		else
# 			for ik in 1:p.K
# 				c.pp[ik].ϕ1 = C[1].R[ik].ϕ * c.pp[ik].ϕ1x
# 			end
# 		end
# 		update!(c,sols[end],estimateθ = false)
# 		push!(C,c)
# 	end
# 	post_proc(C,edatas)
# end




