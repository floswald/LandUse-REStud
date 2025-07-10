"""
# Sensitivity to θr: role of rural productivity growth

* takes output of 20 city model and infers common growth component as well as regional shifter 
* reduces the growth of the common component to very small growth: 2 percent.
* reapplies regional shifter and runs model again, this time _without_ treating θr as choice variables.

##

1. runs counterfactuals
2. makes plots

https://github.com/floswald/LandUse.jl/issues/113
"""
function sensitivity_θr(;K = 20, newgs = [0.2,0.04], save = false, readdisk = true, returnplots = true)

    pth = joinpath(dbplots(), "revision$(revision())","sensitivity-thetar")
    mkpath(pth)

    if !readdisk
        d = sensitivity_θr_impl(K,newgs = newgs)
        JLD2.jldsave(joinpath(pth,"data.jld2"); d)
    else
        d = JLD2.load_object(joinpath(pth,"data.jld2"))
    end
    
    if !returnplots
        return d
    else
        # filter out here the notheta for the baseline version
        # then make another version with notheta next to the 20% scenario
        p = sensitivity_θr_plot(filter(x -> !contains(string(x.first), "noθu"),d[1]),d[2])
        # pθu = sensitivity_θr_plot(filter(x -> contains(string(x.first), r"noθu|Baseline"),d[1]),d[2])
        pθu = sensitivity_θr_plot(filter(x -> !contains(string(x.first), "4.0%"),d[1]),d[2])
        if save
            for (k,v) in filter(x -> x.first != :Baseline,d[1])
                export_any_csv(v, joinpath(pth, "$k.csv"))
            end
            for (k,v) in p
                fname = replace(k, "%" => "perc")
                println("saving $fname")
                savefig(v, joinpath(pth,"$(fname).pdf"))
            end
            for (k,v) in pθu

                fname = replace(k, "%" => "perc")
                println("saving $fname")

                savefig(v, joinpath(pth,"noθu-$(fname).pdf"))
            end
        end
        p,pθu

    end

end

function plot_labor_θr(d)
    plu = Dict(k => @chain v begin
        subset(:year => leq(2020))
        @df plot(:year, :Lu, group = :LIBGEO, color = dcol(20) , leg =  false, title = "Lu: $k")
    end
    for (k,v) in d)
    
    plr = Dict(k => @chain v begin
        subset(:year => leq(2020))
        @df plot(:year, :Lr, group = :LIBGEO, color = dcol(20) , leg =  false, title = "Lr: $k")
    end
    for (k,v) in d)
    # plot(collect(values(pl))..., size = (1000,300), layout = @layout [a{0.4w} b{0.3w} c{0.3w}])
    lu = plot(collect(values(plu))..., size = (1000,300), layout = (1,3), link = :y)
    lr = plot(collect(values(plr))..., size = (1000,300), layout = (1,3), link = :y)
    Dict(:Lu => lu,:Lr => lr)
end

function plot_θdens(d)
    pl0 = Dict(k => @chain v begin
        subset(:year => leq(2020))
        @df plot(:year, :d0_n, group = :LIBGEO, color = dcol(20) , leg =  false, title = "d0: $k")
        end
    for (k,v) in d)

    pla = Dict(k => @chain v begin
        subset(:year => leq(2020))
        @df plot(:year, :avgd_n, group = :LIBGEO, color = dcol(20) , leg =  false, title = "avgd: $k")
        end
    for (k,v) in d)

    plr = Dict(k => @chain v begin
        subset(:year => leq(2020))
        @df plot(:year, :dr_n, group = :LIBGEO, color = dcol(20) , leg =  false, title = "dr: $k")
        end
    for (k,v) in d)

    plro = Dict(k => @chain v begin
            subset(:year => leq(2020))
            @df plot(:year, :pr, group = :LIBGEO, color = dcol(20) , leg =  false, title = "rhor: $k")
            end
        for (k,v) in d)
    
    pl0 = plot(collect(values(pl0))..., size = (1000,300), layout = (1,3))
    plr = plot(collect(values(plr))..., size = (1000,300), layout = (1,3))
    pla = plot(collect(values(pla))..., size = (1000,300), layout = (1,3))
    plro = plot(collect(values(plro))..., size = (1000,300), layout = (1,3))

    Dict(:d0 => pl0,:avgd => pla, :dr => plr,:ρ => plro)
end


function sensitivity_θr_impl(K; newgs = [0.2,0.04])

    # get counteractuals for all values
    v = Dict(i => sensitivity_θr_impl2(K,newrg = i) for i in newgs)

    sys = [Symbol("$(round(newgs[i]*100,digits = 0))%") for i in 1:length(newgs)]

    d = OrderedDict(:Baseline => khet_run(readdisk = true))
    for i in 1:length(newgs)
        d[sys[i]] = v[newgs[i]][:result]
        if isa(v[newgs[i]][:nothetas], DataFrame)
            d[Symbol("$(round(newgs[i]*100,digits = 0))%-noθu")] = v[newgs[i]][:nothetas]
        end
    end
    dg = Dict(Symbol("$(round(k*100,digits = 0))%") => v[:growth_data] for (k,v) in v)
    return (d,dg)
end

function sensitivity_plot_thetas(d0::Union{Dict,OrderedDict}, dg::Union{Dict,OrderedDict}, which)
    d0 = @chain d0[ which ] begin
        select(:year, :region, :θu => :θu_counterfactual,:θr => :θr_counterfactual)
        leftjoin(select(d0[:Baseline], :year, :region, :θu, :θr), on = [:year, :region])
        leftjoin(select(dg[ which ], :year, :region, :θut, :θrt,:θut_new, :θrt_new), on = [:year, :region])
        transform(
                  [:θu, :θut] => ((x,y) -> x ./ y) => :θu_netofagg,
                  [:θr, :θrt] => ((x,y) -> x ./ y) => :θr_netofagg,
                  [:θu_counterfactual, :θut_new] => ((x,y) -> x ./ y) => :θu_counterfactual_netofagg,
                  [:θr_counterfactual, :θrt_new] => ((x,y) -> x ./ y) => :θr_counterfactual_netofagg
        )
        sort([:year, :region])
    end

    # dg[ which ]  has got old and new aggregate components

    p1 = @df d0 scatter(log.(:θu), log.(:θu_counterfactual), xlab = "log θu", ylab =  
    "log θu_counterfactual",xticks = :auto,leg = false)
    p2 = @df d0 scatter(log.(:θr), log.(:θr_counterfactual), xlab = "log θr", ylab =  
    "log θr_counterfactual",xticks = :auto,leg= false)

    # normalized by aggregate component
    p3 = @df d0 scatter(log.(:θu_netofagg), log.(:θu_counterfactual_netofagg), xlab = "log θuk", ylab =  
    "log θuk_counterfactual",xticks = :auto,leg = false)

    p4 = @df d0 scatter(log.(:θr_netofagg), log.(:θr_counterfactual_netofagg), xlab = "log θrk", ylab =  
    "log θrk_counterfactual",xticks = :auto,leg = false)

    # normalized by aggregate component
    p5 = @df d0 scatter(log.(:θu_netofagg), log.(:θu_counterfactual_netofagg), xlab = "log θuk", ylab =  
    "log θuk_counterfactual",xticks = :auto,leg = false, color = :region)
    plot!(p5, log.(d0.θu_netofagg),log.(d0.θu_counterfactual_netofagg), color = d0.region , group = d0.region, lw = 1.5)
    plot!(p5, x -> x, lab = "", linewidth = 2, color = "black")

    p6 = @df d0 scatter(log.(:θr_netofagg), log.(:θr_counterfactual_netofagg), xlab = "log θrk", ylab =  
    "log θrk_counterfactual",xticks = :auto,leg = false, color = :region)


    Dict(Symbol(string(which)* "θu") => p1,
         Symbol(string(which)* "θr") => p2,
         Symbol(string(which)* "θu_net") => p3,
         Symbol(string(which)* "θr_net") => p4,
         Symbol(string(which)* "θu_color_net") => p5,
         Symbol(string(which)* "θr_color_net") => p6
         )
end

function sensitivity_θr_plot(d::OrderedDict,dg::Dict)

    # d = dr[1]
    # dg = dr[2]  # growth data dict

    # aggregate all datasets 
    a = OrderedDict( k => @chain aggregator(v) begin
    subset(:year => leq(2020))
    end 
        for (k,v) in d)
            
    sys = setdiff(collect(keys(a)), [:Baseline])


    noθu = any(contains.(string(sys),"noθu"))
    if noθu 
        @info "plotting sensitivity to θr noθu."
        println()
    end


    # theme
    def_theme(legfontsize = 10)
    
    # make 3 plots 
    # return a
    # avd_n, dr_1840, ℙ_n for baseline vs both counterfactuals
    # pvars = [:citydensity_1840,:dr_agg_1840,:ρr_1840]
    pvars = [:citydensity_1840,:dr_1840,:ρr_1840, :d0_1840, :commuting_speed_1840, :Lrshare_agg, :ρr_GDP,:ρr_y_disposable]
    # pvars = [:citydensity_1840,:dr_1840,:ρr_1840,   :Lrshare_agg, :ρr_y]
    
    ticks = Dict(:commuting_speed_1840 => 1:0.5:5,
                :citydensity_1840 => [0.1, 0.25, 0.5, 1,2],
                 :dr_1840 => [0.05, 0.25, 0.5, 1,2,5],
                 :ρr_y => :auto,
                 :d0_1840 => [0.25, 0.5, 1,1.2],
                 :Lrshare_agg => [0.02, 0.25, 0.5, 0.7],
                 :ρr_GDP => :auto,
                 :ρr_y_disposable => :auto,
                 :ρr_1840 => [1,2,5,10,20,50]
                 )
    lims = Dict(:commuting_speed_1840 => (0.8,5),
                 :citydensity_1840 => (0.1,3),
                 :ρr_y => :auto,
                 :dr_1840 => (0.04,7),
                 :d0_1840 => (0.2,1.3),
                 :Lrshare_agg => :auto,
                 :ρr_GDP => :auto,
                 :ρr_y_disposable => :auto,
                 :ρr_1840 => :auto
                 )
    legs = Dict(:citydensity_1840 => :topleft,
                :dr_1840 => :topleft,
                :commuting_speed_1840 => :topleft,
                :ρr_y => :topleft,
                :d0_1840 => :bottomleft,
                :Lrshare_agg => :bottomleft,
                :ρr_GDP => :bottomleft,
                 :ρr_y_disposable => :bottomleft,
                :ρr_1840 => :topleft
                )
    cold = Dict(:commuting_speed_1840 => reds(),
                 :citydensity_1840 => reds(),
                 :dr_1840 => blues(),
                 :ρr_y => greens(),
                 :d0_1840 => golds(),
                 :Lrshare_agg => greens(),
                 :ρr_GDP => greens(),
                 :ρr_y_disposable => greens(),
                 :ρr_1840 => greens()
                 )

    namepref = Dict(:commuting_speed_1840 => "mean_",
                 :citydensity_1840 => "mean_",
                 :dr_1840 => "mean_",
                 :ρr_y => "mean_",
                 :d0_1840 => "mean_",
                 :Lrshare_agg => "",
                 :ρr_GDP => "mean_",
                 :ρr_y_disposable => "mean_",
                 :ρr_1840 => "mean_",
                 )
    scales = Dict(:commuting_speed_1840 => :identity,
                :dr_1840          => :log10,
                :d0_1840          => :log10,
                :ρr_y => :identity,
                :citydensity_1840          => :log10,
                :Lrshare_agg          => :identity,
                :ρr_GDP => :log10,
                :ρr_y_disposable => :log10,
                :ρr_1840          => :log10)

    # labels depend on whether we have the noθu or not
    labels = if noθu
        ["Baseline", "$(sys[1]) of agg. Rural Growth: re-estimation of θu","$(sys[1]) of agg. Rural Growth: Baseline θu"]
    else
        if length(sys) < 3
            ["Baseline", "$(sys[1]) of agg. Rural Growth","$(sys[2]) of agg. Rural Growth"]
        else
            ["Baseline", "$(sys[1]) of agg. Rural Growth","$(sys[2]) of agg. Rural Growth", "$(sys[3]) of agg. Rural Growth"]
        end
    end

    yform(v) = if v == :ρ_1840
        x -> string.(round(Int,x))
    elseif v ∈ [:ρr_GDP,:ρr_y_disposable]
        x -> string.(round(x,digits = 3))
    else
        x -> string.(round(x,digits = 2))
    end

    p = Dict()

    for ip in pvars
        
        ss = Symbol("$(namepref[ip])$(ip)")
        pkey = string(ip)  # plot key on dict
        # println(ss)

        p[pkey] = @df a[:Baseline] plot(:year, cols(ss), label = labels[1], color = cold[ip][1],yscale = scales[ip], yticks = ticks[ip],
        yformatter = yform(ip),
        ylims = lims[ip],leg = legs[ip], linestyle = :dot, size = panelsizef(), xticks = 1840:20:2020)

        @df a[sys[1]] plot!(p[pkey],:year, cols(ss), label = labels[2], color = cold[ip][2],ylims = lims[ip], linestyle = :solid, markershape = :circle)

        if length(sys) > 1

            @df a[sys[2]] plot!(p[pkey],:year, cols(ss), label = labels[3], color = cold[ip][3],ylims = lims[ip], linestyle = :solid)
            
            if length(sys)==3
                @df a[sys[3]] plot!(p[pkey],:year, cols(ss), label = labels[4], color = cold[ip][3],ylims = lims[ip], linestyle = :dot)
            end
        end
    end

    # TODO only if not running the noθu thing
    # plot diagnostics
    # theme(:default)
    if !noθu
        pdensitites = plot_θdens(d)
        plabor = plot_labor_θr(d)
        pθu = sensitivity_plot_thetas(d, dg, sys[1])
        pθr = sensitivity_plot_thetas(d, dg, sys[2])
        pθuagg = @df a[:Baseline] plot(:year, :mean_θu, yticks = 1:2:15, color = :red, label = "baseline",lw = 1)
        plot!(pθuagg, a[sys[1]].year, a[sys[1]].mean_θu , color = :blue, label = string(sys[1]),lw = 1)
        plot!(pθuagg, a[sys[2]].year, a[sys[2]].mean_θu , color = :green, label = string(sys[2]),lw = 1)


        for (k,v) in pdensitites
            p["diag_$k"] = v
        end
        for (k,v) in plabor
            p["diag_$k"] = v
        end
        for (k,v) in pθu
            p["diag_$k"] = v
        end
        for (k,v) in pθr
            p["diag_$k"] = v
        end
        p["diag_θuagg"] = pθuagg
    else
        # scatter pop, area, avg density baseline vs non theta re-estimation
        # compare d[sys[1]] vs d[sys[2]]
        xdata = innerjoin(
            select(d[sys[1]], :year, :LIBGEO, :Lu => :Lu_reestimated, :cityarea => :cityarea_reestimated, :citydensity => :citydensity_reestimated),
            select(d[sys[2]], :year, :LIBGEO, :Lu => :Lu_baseline, :cityarea => :cityarea_baseline, :citydensity => :citydensity_baseline),
            on = [:year, :LIBGEO]
        )
        def_theme(xticks = :auto)

        p["noθu-diag-Lu"] = modvsdata(xdata, :Lu_reestimated, :Lu_baseline, xlab = "log Lu re-estimated θu", ylab = "log Lu baseline θu", title = "", demean = false)
        p["noθu-diag-Area"] = modvsdata(xdata, :cityarea_reestimated, :cityarea_baseline, xlab = "log Area re-estimated θu", ylab = "log Area baseline θu", title = "",xan = 0.2, yan = 0.1, demean = false)
        p["noθu-diag-Density"] = modvsdata(xdata, :citydensity_reestimated, :citydensity_baseline, xlab = "log Density re-estimated θu", ylab = "log Density baseline θu", title = "",xan = 80, yan = 200, demean = false)

    end
        
    
    

    # print avg_density
    bs2020 = subset(a[:Baseline], :year => ieq(2020))
    p2020 = subset(a[sys[1]], :year => ieq(2020))
    if length(sys) > 1

        p2020_2 = subset(a[sys[2]], :year => ieq(2020))
        if length(sys) == 3
            p2020_3 = subset(a[sys[3]], :year => ieq(2020))
        end
    end


    if length(sys) == 1
        @info "Avg density 2020. Baseline: $(round(bs2020.mean_citydensity_1840[1],digits =2)), $(sys[1]): $(round(p2020.mean_citydensity_1840[1],digits=2))"
        @info "Speed increase.   Baseline: $(round(bs2020.mean_commuting_speed_1840[1],digits = 2)), $(sys[1]): $(round(p2020.mean_commuting_speed_1840[1],digits=2))"
    elseif length(sys) == 2
        @info "Avg density 2020. Baseline: $(round(bs2020.mean_citydensity_1840[1],digits =2)), $(sys[1]): $(round(p2020.mean_citydensity_1840[1],digits=2)), $(sys[2]): $(round(p2020_2.mean_citydensity_1840[1],digits=2))"
        @info "Speed increase.   Baseline: $(round(bs2020.mean_commuting_speed_1840[1],digits = 2)), $(sys[1]): $(round(p2020.mean_commuting_speed_1840[1],digits=2)), $(sys[2]): $(round(p2020_2.mean_commuting_speed_1840[1],digits=2))"

    else
        @info "Avg density 2020. Baseline: $(round(bs2020.mean_citydensity_1840[1],digits =2)), $(sys[1]): $(round(p2020.mean_citydensity_1840[1],digits=2)), $(sys[2]): $(round(p2020_2.mean_citydensity_1840[1],digits=2)), $(sys[3]): $(round(p2020_3.mean_citydensity_1840[1],digits=2))"
        @info "Speed increase.   Baseline: $(round(bs2020.mean_commuting_speed_1840[1],digits = 2)), $(sys[1]): $(round(p2020.mean_commuting_speed_1840[1],digits=2)), $(sys[2]): $(round(p2020_2.mean_commuting_speed_1840[1],digits=2)), $(sys[3]): $(round(p2020_3.mean_commuting_speed_1840[1],digits=2))"

    end
    println()
    p

end

"""

infer common component in regional growth from baseline data until 2020
====================================================================

* baseline data has θrkt = θrt * θkt, and we observe θrt.
* hence can extract regional component: θrkt / θrt = θkt
* compute growth rate of θrt: gθrt , t <= 2020
* modify gθrt s.t. new rate is gθrt * newg
* reconstruct new θrt series θrt_new by appliying gθrt * newg to initial point 1.0
* rebuild new θrkt series as θrkt_new = θrt_new * θkt
* supply a matrix of rural growth rates for each region and year to the HetCountry constructor

"""
function change_θ_growth(d::DataFrame,p::Param; newrg = 0.1,newug = 1.0,  Tmax = 2020, agglo_adjust = nothing)
    # get aggregate theta and it's growth rate first
    θagg = DataFrame(year = p.T, it = 1:length(p.T), θrt = p.θrt, θut = p.θut)
    transform!(θagg, :θrt => (x -> (lag(x,-1) .- x) ./ x) => :θrt_g )
    transform!(θagg, :θut => (x -> (lag(x,-1) .- x) ./ x) => :θut_g )
    transform!(θagg, :θrt_g => (x -> x .* newrg) => :θrt_gnew )  # new, transformed growth rate of agg component
    transform!(θagg, :θut_g => (x -> x .* newug) => :θut_gnew )  
    # new agg component
    θagg.θrt_new = ones(nrow(θagg))
    θagg.θut_new = ones(nrow(θagg))
    for i in 2:nrow(θagg)
        θagg.θrt_new[i] = (1 + θagg.θrt_gnew[i-1]) * θagg.θrt_new[i-1]
        θagg.θut_new[i] = (1 + θagg.θut_gnew[i-1]) * θagg.θut_new[i-1]
    end
    # return θagg
    # baseline data has θrkt = θrt * θkt, and we observe θrt.
    
    d = @chain d begin
        subset(:year => leq(Tmax))
        select(:year, :θr => :θrkt, :θu => :θukt, :region, :LIBGEO,:r,:pr,:Sr,:Lu,:Lr,:ϕ, :Srh)  
        leftjoin(θagg, on = :year)
        # hence can extract regional component: θrkt / θrt = θkt
        transform([:θrkt, :θrt] => ((x,y) -> x ./ y ) => :θkt_r)   
        transform([:θukt, :θut] => ((x,y) -> x ./ y ) => :θkt_u)   
        # rebuild new θrkt series as θrkt_new = θrt_new * θkt
        transform([:θrt_new, :θkt_r] => ((x,y) -> x .* y) => :θrkt_new )
        transform([:θut_new, :θkt_u] => ((x,y) -> x .* y) => :θukt_new )
    end
    if !isnothing(agglo_adjust)
        @assert size(agglo_adjust,1) == length(unique(d.LIBGEO))
        # divide theta_ukt by Lu_k1 from baseline
        d = @chain d begin
            leftjoin(agglo_adjust, on = :LIBGEO)
            transform([:θukt, :Lu_baseline] => ((x,y) -> x ./ (y .^ p.λ)) => :θukt_new)
        end
    end
    d
end



function sensitivity_θr_impl2(K; Tmax = 2020, newrg = 0.05,newug = 1.0)

    @info "sensitivity θr with $(newrg * 100)% of baseline growth"
    # get the baseline 
    d = khet_run(readdisk = true,K = K)

    # get a baseline param
    p0 = Param(par = Dict(:T => 1840:10:Tmax, :K => K))

    # get a new param with lower growth rate after 2020 in rural
    pars = Dict(:T => 1840:10:Tmax, :magr => newrg * p0.magr, :K => K)
    p = Param(par = pars)

    # infer common component in rural growth from baseline data until 2020
    dd = change_θ_growth(d,p,newrg = newrg, newug = newug, Tmax = Tmax)
    

 
    # return subset(dd, :region => x -> x .< 6)
    # make plot old vs new setting 
    pl1 = @df dd plot(:year,:θkt_r, group = :LIBGEO, yscale = :log10, ylab = L"\log \theta_{k,r}",xticks = 1840:40:2020, leg = :outerright, yformatter = x -> string(round(x,digits = 2)), color = dcol(20), lw = 2)
    # @df dd plot!(pl1,:year, :θrt_new, label = "", color = :red, lw = 4)
    savefig(pl1, joinpath(dbplots(),"revision$(revision())","sensitivity-thetar","setup-theta-k-r.pdf"))

    pl1u = @df dd plot(:year,:θkt_u, group = :LIBGEO, yscale = :log10,ylab = L"\log \theta_{k,u}",xticks = 1840:40:2020, leg = :outerright, yformatter = x -> string(round(x,digits = 2)), color = dcol(20), lw = 2)
    # @df dd plot!(pl1u,:year, :θut_new, label = "", color = :red, lw = 4)
    savefig(pl1u, joinpath(dbplots(),"revision$(revision())","sensitivity-thetar","setup-theta-k-u.pdf"))

    pl1u = @df dd plot(:year,:θukt_new, group = :LIBGEO, yscale = :log10,ylab = L"\log \theta_{k,u}",xticks = 1840:40:2020, leg = :outerright, yformatter = x -> string(round(x,digits = 2)), color = dcol(20), lw = 2)


    pl0 = @df dd plot(:year,:θrkt, group = :LIBGEO, yscale = :log10, title = "Baseline", ylab = L"\log \theta_r", label = "", color = dcol(20))
    @df dd plot!(pl0,:year, :θrt,  color = :red, lw = 4,leg = :topleft,label = L"\theta_{rt}" , legendfontsize = 12)

    pl0u = @df dd plot(:year,:θukt, group = :LIBGEO, yscale = :log10, title = "Baseline", ylab = L"\log \theta_u", label = "", color = dcol(20))
    @df dd plot!(pl0u,:year, :θut,  color = :red, lw = 4,leg = :topleft,label = L"\theta_{ut}" , legendfontsize = 12)

    pl = plot(pl0, pl1, size = (800,400),left_margin = 0.5Plots.cm)
    plu = plot(pl0u, pl1u, size = (800,400),left_margin = 0.5Plots.cm, layout = @layout [a{0.4w} b{0.6w}])
    plu_ = plot(pl0u, pl1u, size = (800,400),left_margin = 0.5Plots.cm, link = :y, layout = @layout [a{0.4w} b{0.6w}])
    pl_ = plot(pl0, pl1, size = (800,400),left_margin = 0.5Plots.cm, link = :y)
    savefig(pl, joinpath(dbplots(),"revision$(revision())","sensitivity-thetar","setup-growth=$(newrg).pdf"))
    savefig(plu, joinpath(dbplots(),"revision$(revision())","sensitivity-thetar","setup-ugrowth=$(newug).pdf"))
    savefig(pl_, joinpath(dbplots(),"revision$(revision())","sensitivity-thetar","setup-growth=$(newrg)-link.pdf"))
    savefig(plu_, joinpath(dbplots(),"revision$(revision())","sensitivity-thetar","setup-ugrowth=$(newug)-link.pdf"))

    # run model
    # =========
    # return dd
    # supply a matrix of rural growth rates for each region and year to the HetCountry constructor

    # khet_estimate_impl(K;tol = 1e-5,par = Dict(:T => 1840:10:Tmax),verbose = true, iterateP = 2,estimateθu = true,estimateθr = false,θdf = dd,lbL = 0.0)
    pp = Param(par = Dict(:T => 1840:10:Tmax, :gsr => zeros(K),:gs => zeros(K),:kshare => [1/K for i in 1:K],:factors => ones(K),:K => K,:𝕊 => zeros(60,K)))
    # runk_impl_lowθr(pp, dd, overdrive = 12 , estimateθu= true)

    res = θu_iteration(pp, dd)

    nothetas = try 
        runk_impl_lowθr(pp, dd, estimateθu = false)
    catch e
        @warn """
        🚨 Could not run model $newrg without θu estimation; This is fine (consistent with our results in the paper). I leave this in for documentation reasons. We tried to run that without re-estimation, but failed for numerical issues as demonstrated.

        👉 Nothing to worry about, keep going!
        """
        nothing
    end

    Dict(:result => res, :growth_data => dd, :nothetas => nothetas)
end



"""
    θu_iterator(x0::NamedTuple, θr::Vector, p::Param, L::Float64, relpop::Vector; λ = 0.5, tol = 1e-5)

Iterate on difference in urban population until theta u has converged.

## Details

This algorithm has 4 steps.

1. Initiate with θuk₀ = θu₀ * θk₀
2. Evaluate [`system!`](@ref) to obtain implied urban population share of each city
3. Compute difference to urban population share in data (`relpop`): diff = model - data
4. Update θk to obtain θk₁ = θk₀ λ + (1-λ) θk₀ (1 - diff)
5. Rescale implied aggregate urban producitivity. We impose this:
   ∑ θuk₀ (Luk / ∑ Luk) = θu(data)

   Fill in on the RHS from point 1 the definition of θuk₀ and take out the aggregate:
   ∑ (θu₀ * θk₀) (Luk / ∑ Luk) = θu(data)
   θu₀ *  ∑ (θk₀) (Luk / ∑ Luk) = θu(data)

   This suggests to get the rescaling

   θu₁ = θu(data) / ∑ (θk₀) (Luk / ∑ Luk)

   Go back to point 1.

Explain convergence criterion.

"""
function θu_iterator(x0::NamedTuple, θr::Vector, p::Param, L::Float64, Lushare_data::Vector; λ = 0.5, tol = 1e-5)

    K = length(θr)

    θk = ones(K) # regional component all ones
    θu = p.θu    # initial aggregate as in data = ones(K)
    θuk = ones(K)  # initiate θuk₀ vector


    # lower bound on choice variables
    lb = [fill(0.001, length(x0.Sr))...,
          fill(0.0001, length(x0.Lu))...,
          fill(0.0001, length(x0.Lr))...,
          0.01,  # r
          0.01   # pr
          ]
    ub = fill(Inf,length(nt2x(x0)))

    # set a differece
    Δ = 100.0
    diffvec = zeros(K)  

    prog = ProgressThresh(tol; desc="Pop dist convergence it=$(p.it):")
    while Δ > tol
    # for i in 1:2

        # 1. initiate θuk 
        θuk[:] = θu .* θk

        # 2. evaluate system given θuk
        xx = nlboxsolve((x,y) -> system!(x,y,p,L, θuk, θr),nt2x(x0), lb,ub)
        # compute difference between observed and actual Lu for each city 
        x0 = x2nt(xx.zero,K)

        # 3. Compute Difference in urban shares to data
        Lushare = x0.Lu ./ sum(x0.Lu)
        diffvec[:] = Lushare .- Lushare_data
        Δ = maximum(abs, diffvec)
        
        ProgressMeter.update!(prog,Δ)
        

        # 4. Update θk to obtain θk₁
        θk[:] = ((1 - λ) .* θk) .+ (λ .* θk .* (1 .- diffvec))

        # 5. Resacle implied aggregate component
        θu = p.θu / sum(Lushare .* θk)

    end
    
    sysout = system!(zeros(3 + 2K + K),nt2x(x0),p,L,θuk,θr)
    return sysout,θuk
    # println(diffvec)
end

function θu_iteration(p::Param, θrdf::DataFrame)
    # get starting point from baseline

    sols = NamedTuple[]
    C = HetCountry[]
    edatas = DataFrame[]


    Tmax = length(p.T)
    dampener = fill(0.25, Tmax)
    dampener[[14,15,16,17,18,19]] .= 0.15


    ϕs = zeros(p.K)


    d0 = @chain θrdf begin
        subset(:it => x -> x .== 1)  # period == it
        sort(:region)  # just to make sure
        select(:region,:r,:pr,:Sr,:Lu,:Lr,:θukt_new => :θu,:θrkt_new => :θr, :ϕ, :Srh)
        end
    push!(sols,(Sr = d0.Sr,
        Lu = d0.Lu,
        Lr = d0.Lr,
        r = unique(d0.r)[1],
        pr = unique(d0.pr)[1]))

    for it in 1:Tmax
        setperiod!(p,it)
        # println(it)
        c = HetCountry(p) 
        push!(edatas, areaprice_estim(p))

        dt = @chain θrdf begin
        subset(:it => x -> x .== it)  # period == it
        sort(:region)  # just to make sure
        select(:region,:r,:pr,:Sr,:Lu,:Lr,:θukt_new => :θu,:θrkt_new => :θr, :ϕ, :Srh)
        end

        sysout,θu = θu_iterator(sols[end],dt.θr,p,sum(dt.Lu .+ dt.Lr), edatas[end].popwgt, λ = dampener[it])
        push!(sols, sysout)
        ϕs[:] = sysout.ϕ

        # set thetas on country
        for ik in 1:p.K
            # println("k = $ik, t = $it, idx = $idx")
            c.pp[ik].θr = dt.θr[ik][1]
            c.pp[ik].θu = θu[ik]
        end
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
    end
    post_proc(C,edatas)

end

# goes until 2020
function runk_impl_lowθr(p::Param,θrdf::DataFrame; overdrive = 9, estimateθr = false, estimateθu = true)
	sols = NamedTuple[]
	objs = Float64[]
	edatas = DataFrame[]
	# ϕvs =Vector{Float64}[]
	# dϕvs =Vector{Float64}[]

	# get starting point from baseline
	d0 = @chain θrdf begin
		subset(:year => x -> x .== minimum(x))  # period == it
		sort(:region)  # just to make sure
		select(:region,:r,:pr,:Sr,:Lu,:Lr,:θukt_new => :θu,:θrkt_new => :θr, :ϕ, :Srh)
	end
	x0 = (Sr = d0.Sr,
		  Lu = d0.Lu,
		  Lr = d0.Lr,
		  r = unique(d0.r)[1],
		  pr = unique(d0.pr)[1])
	push!(sols, x0)

	# push!(sols,x0)
	C = HetCountry[]

	years = sort(unique(θrdf.year))

    ϕs = zeros(p.K)

	prog = Progress(length(p.T), desc = "low θr	Country Model", barglyphs=BarGlyphs("[=> ]"), barlen = 50, color = :green) 
	# @showprogress "Country Model" for it in 1:length(p.T)
	for it in 1:length(p.T)
		@debug it
		setperiod!(p,it)
		c = HetCountry(p) 
		push!(edatas, areaprice_estim(p))

        # modify the thetas for each region on their own parameter. The macro param c.p0 is not touched!
        for ik in 1:p.K
            idx = (θrdf.it .== it) .&  (θrdf.region .== ik)
            # println("k = $ik, t = $it, idx = $idx")
            c.pp[ik].θr = θrdf.θrkt_new[idx][1]
            c.pp[ik].θu = θrdf.θukt_new[idx][1]
        end

		
		if it < overdrive
			d0 = @chain θrdf begin
				subset(:year => ieq(years[it]))  # period == it
				sort(:region)  # just to make sure
				select(:region,:r,:pr,:Sr,:Lu,:Lr,:θukt_new => :θu,:θrkt_new => :θr, :ϕ, :Srh)
			end
			# xx = nlsolve((x,y) -> system!(x,y,p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr),nt2x(sols[end]), autodiff = :forward)
			# push!(sols, x2nt(xx.zero,K))
            xx = jc(c,sols[end], estimateθr = estimateθr, estimateθu = estimateθu)
            # xx = jumpsystem(sols[end],p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr)
			push!(sols, xx[1])
            # println(xx[1].U)
            ϕs[:] = xx[2]  # store phis
			# converged(xx) || error("not converged $(years[it])")
		else
			@info "turning on 4x4 drive 🚚" maxlog=1
			# take convex combinations of thetau and thetar between both time periods 
            # d0.θu[1] = d0.θu[1] * 0.95
			θu0 = d0.θu  # last period
			θr0 = d0.θr  # last period
			d0 = @chain θrdf begin
				subset(:year => ieq(years[it]))  # period == it
				sort(:region)  # just to make sure
				select(:region,:r,:pr,:Sr,:Lu,:Lr,:θukt_new => :θu,:θrkt_new => :θr, :ϕ, :Srh)
			end
            # d0.θu[1] = d0.θu[1] * 0.95

            tvals = if it > 9
                # [range(0.01, 0.95, 20)..., range(0.95, 1, 100)...]
                range(0.0, 1, 20)
            else
                range(0.0, 1, 5)
            end
			for convex in tvals
			# for convex in range(0.1,1, length = 10)
				@debug "convex combo of thetas $convex"
				θu = (1 - convex) .* θu0 .+ convex .* d0.θu
				θr = (1 - convex) .* θr0 .+ convex .* d0.θr
                # fill into country object
                for ik in 1:p.K
                    # idx = (θrdf.it .== it) .&  (θrdf.region .== ik)
                    # println("k = $ik, t = $it, idx = $idx")
                    c.pp[ik].θr = θr[ik]
                    c.pp[ik].θu = θu[ik]
                end
                xx = jc(c,sols[end], estimateθr = estimateθr , estimateθu = estimateθu)
                # xx = jc(c,sols[end], estimateθr = false, lbL = it == 18 ? 0.001 : 0.05 )
                # xx = jumpsystem(sols[end],p,sum(d0.Lu .+ d0.Lr),θu, θr)

                if length(xx) == 1
                    # return (xx[1],JuMP.primal_feasibility_report(xx[1]), xx[2])
                    return xx[1]
                else
			        push!(sols, xx[1])
                end
                ϕs[:] = xx[2]  # store phis
			end
		end

        if it == 1
			for ik in 1:p.K
				c.pp[ik].ϕ1 = ϕs[ik] * c.pp[ik].ϕ1x
			end
		else
			for ik in 1:p.K
				c.pp[ik].ϕ1 = C[1].R[ik].ϕ * c.pp[ik].ϕ1x
			end
		end
		update!(c,sols[end],estimateθ = estimateθu)
		push!(C,c)
        next!(prog)
	end
	post_proc(C,edatas)
end

