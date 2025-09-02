
"""
# d1-d2 Extension: relax monocentricity

In this extension we relax the monocentric assumption somewhat
by allowing the commuting distance to be smaller than the difference 
of residence to central city location. In the baseline model, Commuting
distance is equal to residential distance; in the data, it is not. In this Extension
we incorporate a relationship between residential and commuting distance that we 
recover from commuting data.
"""
function sensitivity_d1d2(;save = false,readdisk = true,returnplots = true,estimateθ = true, d2 = 2.0)
    
    pth = joinpath(dbplots(), "revision$(revision())","sensitivity-d1d2")
    mkpath(pth)


    if !readdisk
        d = sensitivity_d1d2_(estimateθ = estimateθ,d2 = d2)
        JLD2.jldsave(joinpath(pth,"data.jld2"); d)
    else
        d = JLD2.load_object(joinpath(pth,"data.jld2"))
    end
    # return d

    if returnplots
        pl = sensitivity_d1d2_plot(d)
        if save
            
            for (k,v) in filter(x -> x.first != :Baseline,d)
                export_any_csv(v, joinpath(pth, "$k.csv"))
            end

            for (k,v) in pl
                savefig(v, joinpath(pth,"$k.pdf"))
            end
        end
        d1d2 = collect(values(filter(x -> x.first != :Baseline, d)))
        return (pl, sensitivity_d1d2_getd1(d1d2[1]))
    else
        return d
    end
end


function sensitivity_d1d2_trap()
    # save empty plots
    files = [pf_sens("d1d2","cityarea_agg_1840.pdf") ,
    pf_sens("d1d2","citydensity_1840.pdf")  ,
    pf_sens("d1d2","d0_1840.pdf")           ,
    pf_sens("d1d2","dr_1840.pdf")           ,
    pf_sens("d1d2","dens_decile.pdf")       ,
    pf_sens("d1d2","ρr_1840.pdf")           ,

    pf_sens("d1d2","Lu_allyears_d1d2.pdf")  ,
    pf_sens("d1d2","area_allyears_d1d2.pdf"),
    pf_sens("d1d2","dens_allyears_d1d2.pdf"),

    pf_sens("d1d2","Lu_allyears.pdf")       ,
    pf_sens("d1d2","area_allyears.pdf")     ,
    pf_sens("d1d2","dens_allyears.pdf")  ]   

    for f in files
        pl = plot(title = "not converged")
        savefig(pl,f)
    end

end

function sensitivity_d1d2_(;d1 = 0.05,d2 = 2.0, estimateθ = true, constr_viol_tol = nothing)
    b = khet_run(readdisk = true)
    # pars = merge(parsd1d2(),Dict(:K => 20,:d1 => d1, :d2 => d2,:T => 1840:10:2020))

    pars = Dict(:K => 20,:d1 => d1, :d2 => d2,:T => 1840:10:2020, :a => 2.29)
    if estimateθ
        d1d2 = khet_estimate_impl(20,par = pars,iterateP = 0, LUwgt_data = false,estimateθu = estimateθ,estimateθr = estimateθ, constr_viol_tol = constr_viol_tol)
    else
        # get thetas from baseline
        θdf = select(b, :region, :year, :θu => :θukt_new, :θr => :θrkt_new, :it)
        pars = Dict(:K => 20,:d1 => d1, :d2 => d2,:T => 1840:10:2020, :a => 2.5)

        d1d2 = khet_estimate_impl(20,par = pars,iterateP = 0, LUwgt_data = false,estimateθu = false,estimateθr = false, θdf = θdf, constr_viol_tol = constr_viol_tol)
    end

    OrderedDict(:Baseline => b,
         Symbol("d0=$(d1), d1=$(d2)") => d1d2)
end


function sensitivity_d1d2_plot(d::OrderedDict)
   
    def_theme()
    o = Dict()

    pls = Dict(k => plot_het(v) for (k,v) in d)

    sys = setdiff(collect(keys(d)), [:Baseline])


    o[:dens_allyears] = pls[:Baseline][:het][:mod_data_density_allyears_square]
    o[:dens_allyears_color] = pls[:Baseline][:het][:mod_data_density_allyears_color_square]
    o[:dens_allyears_d1d2] = pls[sys[1]][:het][:mod_data_density_allyears_square]
    o[:dens_allyears_d1d2_color] = pls[sys[1]][:het][:mod_data_density_allyears_color_square]

    o[:area_allyear_color] = pls[:Baseline][:het][:mod_data_area_allyears_square_color]
    o[:area_allyears] = pls[:Baseline][:het][:mod_data_area_allyears_square]
    o[:area_allyears_d1d2] = pls[sys[1]][:het][:mod_data_area_allyears_square]
    o[:area_allyears_d1d2_color] = pls[sys[1]][:het][:mod_data_area_allyears_square_color]

    o[:Lu_allyears_color] = pls[:Baseline][:het][:mod_data_pop_allyears_square_color]
    o[:Lu_allyears] = pls[:Baseline][:het][:mod_data_pop_allyears_square]
    o[:Lu_allyears_d1d2] = pls[sys[1]][:het][:mod_data_pop_allyears_square]
    o[:Lu_allyears_d1d2_color] = pls[sys[1]][:het][:mod_data_pop_allyears_square_color]

    # cross sectional stuff 
    # o[:dens] = pls[:Baseline][:het][:mod_data_density]
    # o[:dens_d1d2] = pls[sys[1]][:het][:mod_data_density]
    # o[:area] = pls[:Baseline][:het][:mod_data_area]
    # o[:area_d1d2] = pls[sys[1]][:het][:mod_data_area]


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

    
    # make 3 plots 
    # return a
    # avd_n, dr_1840, ℙ_n for baseline vs both counterfactuals
    pvars = [:cityarea_agg_1840,:citydensity_1840,:d0_1840,:dr_1840,:ρr_1840]
    # pvars = [:cityarea_agg_1840,:citydensity_1840,:d0_1840,:pr_1840]
    ticks = Dict(:cityarea_agg_1840 => [1,10,25,50,90],
                 :citydensity_1840 => [1, 0.4, 0.2,0.1],
                 :d0_1840 => [0.1 , 0.2, 0.5, 1],
                 :ρr_1840 => [0.8,1,1.3,1.7],
                 :dr_1840 => [ 0.05, 0.2, 0.5, 1],
                 :HPI_1840 => 100:50:300
                 )
    scales = Dict(:cityarea_agg_1840 => :log10,
        :citydensity_1840 => :log10,
        :d0_1840 => :log10,
        :ρr_1840 => :log10,
        :dr_1840 => :log10,
        :HPI_1840 => :identity
        )
    lims = Dict(:cityarea_agg_1840 => :auto, #(0,95),
        :citydensity_1840 => :auto,#(0,1.05),
        :d0_1840 => :auto, #(0,1.05),
        :ρr_1840 => :auto,
        :dr_1840 => :auto,
        :HPI_1840 => (90,Inf)
        )
    cold = Dict(:cityarea_agg_1840 => golds(),
                :citydensity_1840 => reds(),
                :d0_1840 => golds(),
                :ρr_1840 => greens(),
                :dr_1840 => blues(),
                :HPI_1840 => reds()
                 )
    legs = Dict(:cityarea_agg_1840 => :topleft,
                 :citydensity_1840 => :bottomleft,
                 :d0_1840 => :bottomleft,
                 :dr_1840 => :bottomleft,
                 :ρr_1840 => :bottomright,
                 :HPI_1840 => :topleft
                  )
    namepref = Dict(:cityarea_agg_1840 => "",
    :citydensity_1840 => "mean_",
    :d0_1840 => "mean_",
    :dr_1840 => "mean_",
    :ρr_1840 => "mean_",
    :HPI_1840 => "mean_"
    )

    for ip in pvars
        
        ss = Symbol("$(namepref[ip])$(ip)")
        println(ss)

        blab = ip == :cityarea_agg_1840 ? string("Urban Area Baseline:") : "Baseline"
        plab = ip == :cityarea_agg_1840 ? string("Urban Area ",sys[1]) : String(sys[1])


        o[ip] = @df a[:Baseline] plot(:year, cols(ss), label = blab, color = cold[ip][1],yscale = scales[ip], yticks = ticks[ip],
        yformatter = HPI_fmt(ip),
        ylims = lims[ip],linestyle = :dot, size = panelsizef(), leg = legs[ip], xticks = 1840:20:2020, yminorgrid = true)

        @df a[sys[1]] plot!(o[ip],:year, cols(ss), label = plab, color = cold[ip][2],ylims = lims[ip], linestyle = :solid, markershape = :circle)

    end
    
    # retro-fit :cityarea_agg_1840 with population
    ddd = a[:Baseline]
    bbb = a[sys[1]]
    blab = "Urban Pop Baseline"
    plab = string("Urban Pop ",sys[1])

    plot!(o[:cityarea_agg_1840], ddd.year, ddd.Lu_agg_1840, color = reds()[1], ls = :dot, label = blab)
    plot!(o[:cityarea_agg_1840], bbb.year, bbb.Lu_agg_1840, color = reds()[2], ls = :solid,label = plab)

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

    o[:dens_decile] = @df dbase plot(:bin, :avg_density, linestyle = :dot, color = reds()[1], label = "Baseline", size = panelsizef())
    @df dd1d2 plot!(o[:dens_decile],:bin, :avg_density, linestyle = :solid, color = reds()[2], label = "$(sys[1])",markershape = :circle)


    pθu = plot_diagnostic_thetas(d[:Baseline], d[sys[1]],sys[1])
    # pθr = sensitivity_plot_thetas(d, sys[2])
    pθuagg = @df a[:Baseline] plot(:year, :mean_θu, yticks = 1:2:15, color = :red, label = "baseline",lw = 1)
    plot!(pθuagg, a[sys[1]].year, a[sys[1]].mean_θu , color = :blue, label = string(sys[1]),lw = 1)
    # plot!(pθuagg, a[sys[2]].year, a[sys[2]].mean_θu , color = :green, label = string(sys[2]),lw = 1)

    pθragg = @df a[:Baseline] plot(:year, :mean_θr, color = :red, label = "baseline",lw = 1)
    plot!(pθragg, a[sys[1]].year, a[sys[1]].mean_θr , color = :blue, label = string(sys[1]),lw = 1)


    for (k,v) in pθu
        o["diag_$k"] = v
    end
    # for (k,v) in pθr
    #     p["diag_$k"] = v
    # end
    o["diag_θuagg"] = pθuagg
    o["diag_θragg"] = pθragg

    # more output to check differences to baseline
    # iv = ["Lu", "Lr", "Sr", "Srh", "cityarea", "citydensity"]
    # d2 = @chain d[Symbol("d0=0.05, d1=2.25")] begin
    #     select("year", "LIBGEO", [x => x * "_d1d2" for x in iv])
    #     leftjoin(select(d[:Baseline], "year", "LIBGEO",iv...), on = ["year","LIBGEO"])
    #     @aside @chain _ begin
    #         CSV.write(joinpath(@__DIR__,"d1d2detail.csv") , _)
    #     end
    # end

    # o[:detail_Lu] = modvsdata(d2, :Lu_d1d2, :Lu,dolog = true,xlab = L"$\log L_u$ d1d2",ylab = L"$\log L_u$ baseline ",size = panelsizef(), colorby = :year, title = "" )
    # o[:detail_Lr] = modvsdata(d2, :Lr_d1d2, :Lr,dolog = true,xlab = L"$\log L_r$ d1d2",ylab = L"$\log L_r$ baseline ",size = panelsizef(), colorby = :year , title = "")
    # o[:detail_Sr] = modvsdata(d2, :Sr_d1d2, :Sr,dolog = true,xlab = L"$\log S_r$ d1d2",ylab = L"$\log S_r$ baseline ",size = panelsizef(), colorby = :year , title = "")
    # o[:detail_area] = modvsdata(d2, :cityarea_d1d2, :cityarea,dolog = true,xlab = L"$\log $ area d1d2",ylab = L"$\log $ area baseline ",size = panelsizef(), colorby = :year , title = "")
    # o[:detail_density] = modvsdata(d2, :citydensity_d1d2, :citydensity,dolog = true,xlab = L"$\log$ density d1d2",ylab = L"$\log$ density baseline ",size = panelsizef(), colorby = :year , title = "")
    o

end


function sensitivity_d1d2_getd1(d::DataFrame; d1 = 2.25)
    x = @chain d begin
        subset(:year => ieq(2020))
        select(:year, :LIBGEO, :ϕ, :ϕ => ByRow(x -> 1 / (1 .+ (d1 .* x))) => :d1)
    end
    p =  @chain begin x
        subset(:LIBGEO => ieq("Paris"))
        combine(:LIBGEO, [:ϕ,:d1] )
    end
    o = @chain begin x
        subset(:LIBGEO => neq("Paris"))
        combine(:LIBGEO => (x -> "others") => :LIBGEO, [:ϕ,:d1] .=> [var mean])
        rename(Dict(:ϕ_mean => :ϕ, :d1_mean => :d1))
    end
    append!(p,o,cols = :union)
end