
function sensitivity_all(; save = false, overwrite = false, readdisk = true)
    o = Dict()
    @info "agglomeration"
    o[:agglo] = sensitivity_agglo(save = save,overwrite = overwrite,readdisk = readdisk)
    @info "low r growth"
    o[:θr] = sensitivity_θr(save = save,overwrite = overwrite,readdisk = readdisk)
    @info "changing σ"
    o[:σ] = sensitivity_σ(save  = save,overwrite = overwrite,readdisk = readdisk)
    @info "changing ξw"
    o[:ξw] = sensitivity_ξw(save = save,overwrite = overwrite,readdisk = readdisk)
    @info "changing ϵ"
    o[:ϵ] = sensitivity_ϵ(save  = save,overwrite = overwrite,readdisk = readdisk)
    @info "doing d1d2"
    o[:d1d2] = sensitivity_d1d2(save = save,overwrite = overwrite,readdisk = readdisk)
    o
end
















"""
https://github.com/floswald/LandUse.jl/issues/98
"""
function issue98()
    x,M,p = runm()  # baseline
    d = dataframe(M,p)
    @df d plot(:year, :H0, lw = 3, c = :black, leg = false, title = L"H_0")
end


"""
https://github.com/floswald/LandUse.jl/issues/90
"""
function issue90()
    x,M,p = runm()  # baseline
    d = dataframe(M,p)

    flat = run(Param(par = Dict(:Lflat => true)))
    dflatr = dataframe(flat[2],flat[3])

    pl = Dict()
    pl[:ndens] = @df d plot(:year, :d0_n, color = :black, linewidth = 2, marker = (:circle, 4), label = "Baseline", title = "Normalized Central Density")
    plot!(pl[:ndens], dflatr.year, dflatr.d0_n, color = :grey,linewidth = 2, label = "No Population Growth")

    pl[:dens] = @df d plot(:year, :avgd_n, color = :black, linewidth = 2, marker = (:circle, 4), label = "Baseline", title = "Normalized Average Density")
    plot!(pl[:dens], dflatr.year, dflatr.avgd_n, color = :grey,linewidth = 2, label = "No Population Growth")

    pl[:rho] = @df d plot(:year, :ρr , color = :black, linewidth = 2, marker = (:circle, 4), label = "Baseline", title = "Rural Land Value",ylab = L"\rho_r", leg = :topleft)
    plot!(pl[:rho], dflatr.year, dflatr.ρr, color = :grey,linewidth = 2, label = "No Population Growth")

    pl[:Lr] = @df d plot(:year, [:Lr :Lu] , color = :black, linewidth = 2, markershape = [:none :circle], markersize = [0 4], label = [L"L_r" L"L_u"], title = "Population Reallocation", leg = :topleft)
    plot!(pl[:Lr], dflatr.year, [dflatr.Lr dflatr.Lu], ls = :dash, markershape = [:none :circle], color = :grey,linewidth = 2, label = ["No Pop Growth" ""])
 

    savefig(pl[:ndens],  joinpath(dbplots(), "issue90-ndensity.pdf"))
    savefig(pl[:dens],  joinpath(dbplots(), "issue90-avgdensity.pdf"))
    savefig(pl[:rho],  joinpath(dbplots(), "issue90-rhor.pdf"))
    savefig(pl[:Lr],  joinpath(dbplots(), "issue90-Lu-Lr.pdf"))
    
    savefig(pl[:ndens],  joinpath(dbplots(), "issue90-ndensity.png"))
    savefig(pl[:dens],  joinpath(dbplots(), "issue90-avgdensity.png"))
    savefig(pl[:rho],  joinpath(dbplots(), "issue90-rhor.png"))
    savefig(pl[:Lr],  joinpath(dbplots(), "issue90-Lu-Lr.png"))

    pl
end




"""
https://github.com/floswald/LandUse.jl/issues/92
"""
function issue92()
    x,M,p = runm()  # baseline
    d = dataframe(M,p)
    
    sbar = run(Param(par = Dict(:sbar => 0.37)))
    dsbar = dataframe(sbar[2],sbar[3])

    xil = run(Param(par = Dict(:ξl => 1.0)))
    dxil = dataframe(xil[2],xil[3])

    xiw = run(Param(par = Dict(:ξw => 1.1)))
    dxiw = dataframe(xiw[2],xiw[3])


    ## plots

    # sbar
    pl = Dict()
    pl[:sbar] = Dict()

    # density
    pl[:sbar][:ndens] = @df d plot(:year, :d0_n, color = :black, linewidth = 2, marker = (:circle, 4), label = L"Baseline: $\bar{s}=%$(round(p.sbar,digits = 2))$", title = "Normalized Central Density")
    plot!(pl[:sbar][:ndens], dsbar.year, dsbar.d0_n, color = :grey,linewidth = 2, label = L"\bar{s}=%$(sbar[3].sbar)")

    # spending share
    pl[:sbar][:hshare] = @df d plot(:year, :hshare, color = :black, linewidth = 2, marker = (:circle, 4), label = L"Baseline: $\bar{s}=%$(round(p.sbar,digits = 2))$", title = "Housing Spending Share", leg = :bottomright)
    plot!(pl[:sbar][:hshare], dsbar.year, dsbar.hshare, color = :grey,linewidth = 2, label = L"\bar{s}=%$(sbar[3].sbar)")


    # ξl
    # D(l,2020)
    nd = M[end].iDensities ./ M[end].iDensities[1]
    ndxi = xil[2][end].iDensities ./ xil[2][end].iDensities[1]
    pl[:xil] = Dict()
    pl[:xil][:Dl] = plot(1:p.int_bins, nd,lc = :black, lw = 2, m = (:circle, :black, 4), title = latexstring("D(l,2020)"),
                         label = L"Baseline: $\xi_l=%$(round(p.ξl,digits = 2))$" ,xlab = L"Distance $l$", ylab = "Normalized Density")
    plot!(pl[:xil][:Dl],1:p.int_bins, ndxi,lw = 2, lc = :grey, label = L"\xi_l=%$(round(xil[3].ξl,digits = 2))"  )

    pl[:xil][:D0] = @df d plot(:year, :d0_n,lc = :black, lw = 2, m = (:circle, :black, 4), title = "Normalized Central Density",
                         label = L"Baseline: $\xi_l=%$(round(p.ξl,digits = 2))$" )
    plot!(pl[:xil][:D0],dxil.year, dxil.d0_n,lw = 2, lc = :grey, label = L"\xi_l=%$(round(xil[3].ξl,digits = 2))"  )
    pl[:xil][:D] = @df d plot(:year, :avgd_n,lc = :black, lw = 2, m = (:circle, :black, 4), title = "Normalized Average Density",
    label = L"Baseline: $\xi_l=%$(round(p.ξl,digits = 2))$" )
    plot!(pl[:xil][:D],dxil.year, dxil.avgd_n,lw = 2, lc = :grey, label = L"\xi_l=%$(round(xil[3].ξl,digits = 2))"  )


    # ξw
    pl[:xiw] = Dict()

    pl[:xiw][:D0] =  @df d plot(:year, :d0_n,lc = :black, lw = 2, m = (:circle, :black, 4), title = "Normalized Central Density",
    label = L"Baseline: $\xi_w=%$(round(p.ξw,digits = 2))$" , leg = :topleft)
    plot!(pl[:xiw][:D0],dxiw.year, dxiw.d0_n,lw = 2, lc = :grey, label = L"\xi_w=%$(round(xiw[3].ξw,digits = 2))")

    pl[:xiw][:D] =  @df d plot(:year, :avgd_n,lc = :black, lw = 2, m = (:circle, :black, 4), title = "Normalized Average Density",
    label = L"Baseline: $\xi_w=%$(round(p.ξw,digits = 2))$", leg = :topleft )
    plot!(pl[:xiw][:D],dxiw.year, dxiw.avgd_n,lw = 2, lc = :grey, label = L"\xi_w=%$(round(xiw[3].ξw,digits = 2))")


    savefig(pl[:sbar][:ndens],  joinpath(dbplots(), "issue92-sbar-ndensity.pdf"))
    savefig(pl[:sbar][:hshare],  joinpath(dbplots(), "issue92-sbar-hshare.pdf"))
    savefig(pl[:xil][:D],  joinpath(dbplots(), "issue92-xil-avgd.pdf"))
    savefig(pl[:xil][:D0],  joinpath(dbplots(), "issue92-xil-d0.pdf"))
    savefig(pl[:xil][:Dl],  joinpath(dbplots(), "issue92-xil-Dl.pdf"))
    savefig(pl[:xiw][:D],  joinpath(dbplots(), "issue92-xiw-avgd.pdf"))
    savefig(pl[:xiw][:D0],  joinpath(dbplots(), "issue92-xiw-d0.pdf"))

    savefig(pl[:sbar][:ndens],  joinpath(dbplots(), "issue92-sbar-ndensity.png"))
    savefig(pl[:sbar][:hshare],  joinpath(dbplots(), "issue92-sbar-hshare.png"))
    savefig(pl[:xil][:D],  joinpath(dbplots(), "issue92-xil-avgd.png"))
    savefig(pl[:xil][:D0],  joinpath(dbplots(), "issue92-xil-d0.png"))
    savefig(pl[:xil][:Dl],  joinpath(dbplots(), "issue92-xil-Dl.png"))
    savefig(pl[:xiw][:D],  joinpath(dbplots(), "issue92-xiw-avgd.png"))
    savefig(pl[:xiw][:D0],  joinpath(dbplots(), "issue92-xiw-d0.png"))
    


    pl 


end



"""

# Plot Land To Labor Ratio vs Data

https://github.com/floswald/LandUse.jl/issues/128
"""
function issue128(; save = false)
    # get data
    d = @chain DataFrame(load(joinpath(dbpath(),"data","nico-output","LandToLabor1840.dta"))) begin
        rename(:Land_LandRatio_normalized => :Land_LaborRatio_normalized)
        dropmissing(:Land_LaborRatio_normalized)
        select(:year , :Land_LaborRatio_normalized)
    end
    d[!,:year] = convert.(Int, d.year)

    # get baseline 
    b = khet_run(readdisk = true)
    a = @chain aggregator(b) begin
        select(:year, [:Sr_agg ,:Lr_agg] => ((x,y) -> (x ./ y)) => :Land_Labor,
                      [:Sr_agg ,:Srh_agg,:Lr_agg] => ((x,y,z) -> ((x .+ y) ./ z)) => :Land_Labor2)
        subset(:year => leq(2020))
        transform([:Land_Labor,:year] => ((x,y) -> normx_by_y(x,y .== 1840)) => :Land_Labor)
        transform([:Land_Labor2,:year] => ((x,y) -> normx_by_y(x,y .== 1840)) => :Land_Labor2)
    end
    anop = @chain b begin
        subset(:region => geq(2))
        aggregator(_)
        select(:year, [:Sr_agg ,:Lr_agg] => ((x,y) -> x ./ y) => :Land_Labor)
        subset(:year => leq(2020))
        transform([:Land_Labor,:year] => ((x,y) -> normx_by_y(x,y .== 1840)) => :Land_Labor)
    end 

    p = @df a plot(:year, :Land_Labor, lw = 3, color = :black, ylab = L"S_r / L_r", label = L"Model: $S_r / L_r$", leg = :topleft,xticks = 1840:20:2020, size = panelsizef(), yscale = :log10,yformatter = x -> string(floor(Int,x)), yticks = [1,3,9], markershape = :circle, markersize = 4)
    @df a plot!(p, :year, :Land_Labor2, label = label = L"Model: $(S_r + S_{rh}) / L_r$", color = :grey, lw = 3)
    plot!(p, d.year, d.Land_LaborRatio_normalized, label = L"Data: $S_r / L_r$", lw = 1, linestyle = :dashdot, color = :black)

    # nop = @df anop plot(:year, :Land_Labor, lw = 3, color = :black, ylab = L"S_r / L_r", label = "Model", leg = :topleft,xticks = 1840:20:2020, size = panelsizef(), title = "no paris")
    # plot!(nop, d.year, d.Land_LaborRatio_normalized, label = "Data", lw = 1, linestyle = :dashdot, color = :black)

    # lr = @df subset(b, :year => leq(2020)) plot(:year, :Lr, group = :region, ylab = "Lr", color = reshape(StatsPlots.distinguishable_colors(20), 1,20), leg = false)
    # sr = @df subset(b, :year => leq(2020)) plot(:year, :Sr, group = :region, ylab = "Sr", color = reshape(StatsPlots.distinguishable_colors(20), 1,20), leg = :outerright)

    # p2 = plot(lr,sr, size = (800,400))

    if save
        savefig(p, joinpath(dbplots(), "revision$(revision())","mod_data-Land-Labor-Ratio.pdf"))	
    end

    p
end



"""
https://github.com/floswald/LandUse.jl/issues/91
"""
function issue91(;alow = 0.1, ahigh = 3.0, save = true)
    x,M,p = runm()  # baseline
    d = dataframe(M,p)

    x,M2,p2 = run(Param(par = Dict(:a => alow)))
    d2 = dataframe(M2,p2)

    x,M3,p3 = run(Param(par = Dict(:a => ahigh)))
    d3 = dataframe(M3,p3)

    # normalize to first data point
    d.LS = d.Lr ./ d.Sr
    d.SL = d.Sr ./ d.Lr
    d2.LS = d2.Lr ./ d2.Sr
    d3.LS = d3.Lr ./ d3.Sr

    transform!(d, :LS => firstnorm => :LS)
    transform!(d, :SL => firstnorm => :SL)
    transform!(d2, :LS => firstnorm => :LS)
    transform!(d3, :LS => firstnorm => :LS)

    lrsr = Dict()
    # plot Lr over Sr
    lrsr[:LrSr] = @df d plot(:year, :LS, linewidth = 2, color = "black", title = L"L_r/S_r", marker = (:circle, 4) , label = L"Baseline $a=%$(round(p.a,digits = 2))$")
    plot!(lrsr[:LrSr], d2.year, d2.LS, linewidth = 2, color = "grey", label = L"a=%$(p2.a)")
    plot!(lrsr[:LrSr], d3.year, d3.LS, linewidth = 2, color = "grey", label = L"a=%$(p3.a)", ls = :dash)

    lrsr[:SL] = @df d plot(:year, :SL,linewidth = 2, color = "black",ylab = L"S_r / L_r", leg = false, title = "single city", yscale = :log10, yticks = [1,3,8],yformatter = x -> string(round(x,digits = 0)))

    # plot Lr over Sr + Srh
    # normalize to first data point
    d.LSr = d.Lr ./ (d.Sr .+ d.Srh)
    d2.LSr = d2.Lr ./ (d2.Sr .+ d2.Srh)
    d3.LSr = d3.Lr ./ (d3.Sr .+ d3.Srh)

    transform!(d, :LSr => firstnorm => :LSr)
    transform!(d2, :LSr => firstnorm => :LSr)
    transform!(d3, :LSr => firstnorm => :LSr)
    lrsr[:LrSrh] = @df d plot(:year, :LSr, linewidth = 2, color = "black", title = L"L_r/(S_r + S_{rh})", marker = (:circle, 4) , label = "Baseline")
    plot!(lrsr[:LrSrh], d2.year, d2.LSr, linewidth = 2, color = "grey", label = L"a=%$(p2.a)")
    plot!(lrsr[:LrSrh], d3.year, d3.LSr, linewidth = 2, color = "grey", label = L"a=%$(p3.a)", ls = :dash)

    


    if save savefig(lrsr[:LrSr], joinpath(dbplots(), "issue91-Lr-over-Sr-last=$(round(d.LS[end], digits = 2)).pdf")) end
    if save savefig(lrsr[:SL], joinpath(dbplots(), "issue91-Sr-over-Lr.pdf")) end
    if save savefig(lrsr[:LrSrh], joinpath(dbplots(), "issue91-Lr-over-SrSrh-last=$(round(d.LSr[end], digits = 2)).pdf")) end
    lrsr
end


function khet20(; overwrite = false, save = false)
    if overwrite
        sols,C,p,objs,edatas = khet(20)  # (sols,C,p,objs,edatas) 
        ed = vcat(edatas...)

        # return ed, dataframe(C)

        d = @chain dataframe(C) begin
            groupby(:year)
            transform(:ρr => maxnorm => :ρr_n )
            innerjoin(ed, on = [:year, :region => :rank])
        end
        # return d
      
        # # make a dataframe with model vs data of prices
        # d = @chain dataframe(z[2]) begin
        #     filter(row -> row.year == 1990, _ )
        #     transform(_, :ρr => maxnorm => :ρr_n )
        # end

        rename!(d, Dict(:ρr => :rhor, :θu => :thetau))
        CSV.write(joinpath(intables,"khet20-d.csv"), d )
        CSV.write(joinpath(dbtables(),"khet20-d.csv"), d )
        CSV.write(joinpath(intables,"khet20-ed.csv"), ed )
    else
        d = CSV.read(joinpath(intables,"khet20-d.csv"), DataFrame)
        ed = CSV.read(joinpath(intables,"khet20-ed.csv"), DataFrame)
    end

    trans_dict = Dict("log(rhor)" => "\$\\log(\\rho_r)\$", "log(thetau)" => "\$\\log(\\theta_u)\$", "log(Lu)" => "\$\\log(L_u)\$")
    reg1 = lm(@formula(log(citydensity) ~ log(rhor) + log(thetau)), d )
    reg2 = lm(@formula(log(citydensity) ~ log(rhor) + log(Lu)), d )
    if save regtable(reg1, reg2, renderSettings = latexOutput(joinpath(dbtables(), "hetr-density-price.tex")), transform_labels = trans_dict, print_estimator_section = false) end

    # data relieable only here:
    densyears = [1870, 1950,1970,1990,2010]

    pl = @df d scatter(:ρr_n, :s_price_data, leg = false, title = L"\rho_r", xlab = "model", ylab = "data")
    pl1 = @chain d begin
        subset(:year => x -> in.(x,Ref(densyears)))
        transform([:Lu, :pop_data] .=> (x -> log.(x)) .=> [:logmodel, :logdata] )
        modvsdata(_, :logmodel, :logdata, title = "Urban Population",xan = 0, yan = 15)
    end
    pl2 = @chain d begin
        subset(:year => x -> in.(x,Ref(densyears)))
        transform([:citydensity, :density_data] .=> (x -> log.(x)) .=> [:logmodel, :logdata] )
        modvsdata(_, :logmodel, :logdata, title = "Urban Density",xan = 2.5, yan = 11.2)
    end
    pl3 = @chain d begin
        subset(:year => x -> in.(x,Ref(densyears)))
        transform([:cityarea, :area_data] .=> (x -> log.(x)) .=> [:logmodel, :logdata] )
        modvsdata(_, :logmodel, :logdata, title = "Urban Area",xan = -2.5, yan = 1)
    end
    # @df d scatter(log.(:citydensity), log.(:density_data), smooth = true, l = (:red, 3), leg = false, xlab = "Log Model Density", ylab = "Log Data Density")
    pall = plot(pl1,pl3, pl2, layout = (1,3), size = (900,300), bottom_margin = 0.5Plots.cm, left_margin = 0.5Plots.cm)
    if save 
        savefig(pl, joinpath(dbplots(), "khet-model-vs-data-rhor.pdf")) 
        savefig(pl1, joinpath(dbplots(), "khet-model-vs-data-pop.pdf")) 
        savefig(pl2, joinpath(dbplots(), "khet-model-vs-data-area.pdf")) 
        savefig(pl3, joinpath(dbplots(), "khet-model-vs-data-density.pdf")) 
        savefig(pall, joinpath(dbplots(), "khet-model-vs-data-all.pdf")) 
    end
    (d,reg1, reg2,pl, pl1, pl2, pl3,pall)
end


function k_hetr(K;it = 10, estimate = false)
    x = runk(par = Dict(:K => 2,:kshare => [0.5,0.5], :factors => [1.0,1.0],:gs => zeros(2), :gsr => zeros(2)), hetθr = true, estimateθ = estimate)
    d = dataframe(x[2])
    dd = select(d, :year, :region, :θr,:θu, :ρr,:Lu,:Lr, :cityarea)
    a = @df dd scatter(:θr, :cityarea, group = :region, leg = false, title = "Area", xlab = L"\theta_r")
    r = @df dd scatter(:θr, :ρr, group = :region, leg = false, title = L"\rho_r", xlab = L"\theta_r")
    u = @df dd scatter(:θr, :θu , group = :region, leg = :topleft, title = L"\theta_u", xlab = L"\theta_r")
    Lu = @df dd scatter(:θr, :Lu , group = :region, leg = false, title = "Lu", xlab = L"\theta_r")
    Lr = @df dd scatter(:θr, :Lr , group = :region, leg = false, title = L"L_r", xlab = L"\theta_r")

    fext = estimate ? "" : "estimate"
    o = plot(a,r,Lu,Lr)
    savefig(o,joinpath(dbplots(),string("hetr-01",fext,".pdf")))


    # cs stuff
    m = [x[2][it].R[i] for i in 1:2]
    p = [x[2][it].pp[i] for i in 1:2]
    setperiod!(p[1],it)
    setperiod!(p[2],it)

    # density
    # lvecs = [collect(range(0.,x[2][it].R[i].ϕ,length=100)) for i in 1:2]  # space for region ik
    lvecs = [collect(range(0.,x[2][it].R[i].ϕ + 0.02,length=100)) for i in 1:2]  # space for region ik
    Dd = [D.(lvecs[i],Ref(p[i]),Ref(m[i])) for i in 1:2]
    Dp = plot(lvecs[1],Dd[1], label = "city 1", title = "density", yscale = :log10, 
           leg = false, right_margin = 20Plots.mm,ylab = "Density", grid = :off)
    plot!(Dp,lvecs[2],Dd[2], label = "city 2")
    hline!([D(0.0,p[1],m[1])], lc = :black, ls = :dash)

    pp = twinx(Dp)
    plot!(pp, lvecs[2], Dd[1] .- Dd[2], leg = :right, lc = :black, label = "diff", 
          xaxis=false, ylab = "difference", grid = :on)

    # wage 
    Ww = [w.(Ref(m[i].Lu),lvecs[i],Ref(m[i].ϕ),Ref(p[i])) for i in 1:2]
    Wp = plot(lvecs[1],Ww[1], label = "city 1", title = "wage")
    plot!(Wp,lvecs[2],Ww[2], label = "city 2")

     # q
     qq = [q.(lvecs[i],Ref(p[i]),Ref(m[i])) for i in 1:2]
     Qp = plot(lvecs[1],qq[1], label = "city 1", title = L"q")
     plot!(Qp,lvecs[2],qq[2], label = "city 2")

    # rho
    rq = [ρ.(lvecs[i],Ref(p[i]),Ref(m[i])) for i in 1:2]
    Qr = plot(lvecs[1],rq[1], label = "city 1", title = L"\rho")
    plot!(Qr,lvecs[2],rq[2], label = "city 2")

    # U
    uq = [utility.(lvecs[i],Ref(p[i]),Ref(m[i])) for i in 1:2]

    o2 = plot(Dp,Wp,Qp,Qr)

    savefig(o2,joinpath(dbplots(),string("hetr-02",fext,".pdf")))
    o,o2
end


function k2_hetr_growth(;it = 10, estimate = false)
    x = runk(par = Dict(:K => 2,:kshare => [0.5,0.5], :factors => [1.0,1.0],:gs => [1.15,1.15], :gsr => [1.1,1.14]), hetθr = true, estimateθ = estimate)
    d = dataframe(x[2])
    # dd = select(d, :year, :region, :θr,:θu, :ρr,:Lu,:Lr, :cityarea)
    thetas = @chain d begin
        filter(row -> row.region == 1,_ )
        @df _ plot(:year, [:θu :θr],label = [L"City 1 and 2 $\theta_u$" L"City 1 $\theta_r$"], lw = 2, color = [3 2], leg = :topleft, title = L"$\theta_u$ and $\theta_r$")
    end
    @chain d begin
        filter(row -> row.region == 2,_ )
        plot!(thetas,_.year, [_.θu _.θr],label = ["" L"City 2 $\theta_r$"], lw = 2, color = [2 1], ls = :dash)
    end
    # return thetas
    
    a = @df  d plot(:year, :citydensity, group = :region, leg = :topright, title = "Density", yscale = :log10, label = ["City 1" L"City 2 (high $\theta_r$)"], color = [2 1],lw = 2, ls = [:solid :dash])
    r = @df  d plot(:year, :ρr, group = :region, leg = false, title = L"\rho_r", color = [2 1],lw = 2, ls = [:solid :dash])
    Lu = @df d plot(:year, :Lu , group = :region, leg = false, title = L"L_u", color = [2 1],lw = 2, ls = [:solid :dash])
    Lr = @df d plot(:year, :Lr , group = :region, leg = false, title = L"L_r", color = [2 1],lw = 2, ls = [:solid :dash])

    fext = !estimate ? "" : "-estimate"
    o = plot(thetas,a, r,Lu,Lr, size = (900,800), layout = @layout [a{0.3h} ; grid(2,2)])
    savefig(o,joinpath(dbplots(),string("hetr2-time-series-grow",fext,".pdf")))
    savefig(o,joinpath(dbplots(),string("hetr2-time-series-grow",fext,".png")))


    # cs stuff
    m = [x[2][it].R[i] for i in 1:2]
    p = [x[2][it].pp[i] for i in 1:2]
    setperiod!(p[1],it)
    setperiod!(p[2],it)

    # density
    # lvecs = [collect(range(0.,x[2][it].R[i].ϕ,length=100)) for i in 1:2]  # space for region ik
    lvecs = [collect(range(0.,x[2][it].R[i].ϕ + 0.02,length=100)) for i in 1:2]  # space for region ik
    Dd = [D.(lvecs[i],Ref(p[i]),Ref(m[i])) for i in 1:2]
    Dp = plot(lvecs[1],Dd[1], label = "city 1", title = "density", yscale = :log10, 
           leg = false, right_margin = 20Plots.mm,ylab = "Density", grid = :off, color = 2,ls = :solid, lw = 2)
    plot!(Dp,lvecs[2],Dd[2], label = "city 2", color = 1,ls = :dash, lw = 2)
    hline!([D(0.0,p[1],m[1])], lc = :black, ls = :dash)

    pp = twinx(Dp)
    plot!(pp, lvecs[2], Dd[1] .- Dd[2], leg = :right, lc = :black, label = "diff", 
          xaxis=false, ylab = "difference", grid = :on, lw = 2)

    # wage 
    Ww = [w.(Ref(m[i].Lu),lvecs[i],Ref(m[i].ϕ),Ref(p[i])) for i in 1:2]
    Wp = plot(lvecs[1],Ww[1], label = "City 1", title = "wage", lw = 2, yscale = :log10, color = 2, leg = :bottomleft)
    plot!(Wp,lvecs[2],Ww[2], label = L"City 2 (high $\theta_r$)", lw = 2, color = 1,ls = :dash)
    vline!(Wp, [m[1].ϕ m[2].ϕ], label = "" ,color = [2 1] , lw = 2, ls = [:solid :dash])

     # q
     qq = [q.(lvecs[i],Ref(p[i]),Ref(m[i])) for i in 1:2]
     Qp = plot(lvecs[1],qq[1], label = "city 1", title = L"q", lw = 2, leg = false, yscale = :log10, xlab = "Location", color = 2)
     plot!(Qp,lvecs[2],qq[2], label = "city 2", lw = 2, color = 1,ls = :dash)

    # rho
    rq = [ρ.(lvecs[i],Ref(p[i]),Ref(m[i])) for i in 1:2]
    Qr = plot(lvecs[1],rq[1], label = "city 1", title = L"\rho", leg = false,color = 2 ,
    right_margin = 20Plots.mm, yscale = :log10, grid = :off, xlab = "Location", lw = 2)
    plot!(Qr,lvecs[2],rq[2], label = "city 2", color = 1,ls = :dash, lw = 2)
    hline!([ρ(0.0,p[1],m[1])], lc = :black, ls = :dash)

    pq = twinx(Qr)
    plot!(pq, lvecs[2], rq[1] .- rq[2], leg = :right, lc = :black, label = "diff", 
          xaxis=false, ylab = "difference", grid = :on, lw = 2)

    # U
    uq = [utility.(lvecs[i],Ref(p[i]),Ref(m[i])) for i in 1:2]

    o2 = plot(Wp,Dp,Qp,Qr, size = (800,500))

    savefig(o2,joinpath(dbplots(),string("hetr2-cross-section-grow",fext,".pdf")))
    savefig(o2,joinpath(dbplots(),string("hetr2-cross-section-grow",fext,".png")))
    o,o2
end


function k2_hetr(;it = 10, estimate = false)
    x = runk(par = Dict(:K => 2,:kshare => [0.5,0.5], :factors => [1.0,1.2], :gs => zeros(2)), hetθr = true, estimateθ = estimate)
    d = dataframe(x[2])
    # dd = select(d, :year, :region, :θr,:θu, :ρr,:Lu,:Lr, :cityarea)
    a = @df  d scatter(:θr, :citydensity, group = :region, leg = :topright, title = "Density", xlab = L"\theta_r", yscale = :log10, label = ["City 1" L"City 2 (high $\theta_r$)"])
    r = @df  d scatter(:θr, :ρr, group = :region, leg = false, title = L"\rho_r", xlab = L"\theta_r")
    u = @df  d scatter(:θr, :θu , group = :region, leg = :topleft, title = L"\theta_u", xlab = L"\theta_r")
    Lu = @df d scatter(:θr, :Lu , group = :region, leg = false, title = "Lu", xlab = L"\theta_r")
    Lr = @df d scatter(:θr, :Lr , group = :region, leg = false, title = L"L_r", xlab = L"\theta_r")

    fext = !estimate ? "" : "-estimate"
    o = plot(a,r,Lu,Lr, size = (800,500))
    savefig(o,joinpath(dbplots(),string("hetr2-time-series",fext,".pdf")))


    # cs stuff
    m = [x[2][it].R[i] for i in 1:2]
    p = [x[2][it].pp[i] for i in 1:2]
    setperiod!(p[1],it)
    setperiod!(p[2],it)

    # density
    # lvecs = [collect(range(0.,x[2][it].R[i].ϕ,length=100)) for i in 1:2]  # space for region ik
    lvecs = [collect(range(0.,x[2][it].R[i].ϕ + 0.02,length=100)) for i in 1:2]  # space for region ik
    Dd = [D.(lvecs[i],Ref(p[i]),Ref(m[i])) for i in 1:2]
    Dp = plot(lvecs[1],Dd[1], label = "city 1", title = "density", yscale = :log10, 
           leg = false, right_margin = 20Plots.mm,ylab = "Density", grid = :off)
    plot!(Dp,lvecs[2],Dd[2], label = "city 2")
    hline!([D(0.0,p[1],m[1])], lc = :black, ls = :dash)

    pp = twinx(Dp)
    plot!(pp, lvecs[2], Dd[1] .- Dd[2], leg = :right, lc = :black, label = "diff", 
          xaxis=false, ylab = "difference", grid = :on)

    # wage 
    Ww = [w.(Ref(m[i].Lu),lvecs[i],Ref(m[i].ϕ),Ref(p[i])) for i in 1:2]
    Wp = plot(lvecs[1],Ww[1], label = "City 1", title = "wage", lw = 2, yscale = :log10, color = 1)
    plot!(Wp,lvecs[2],Ww[2], label = L"City 2 (high $\theta_r$)", lw = 2, color = 2)
    vline!(Wp, [m[1].ϕ m[2].ϕ], label = "" ,color = [1 2] , lw = 2)

     # q
     qq = [q.(lvecs[i],Ref(p[i]),Ref(m[i])) for i in 1:2]
     Qp = plot(lvecs[1],qq[1], label = "city 1", title = L"q", lw = 2, leg = false, yscale = :log10)
     plot!(Qp,lvecs[2],qq[2], label = "city 2", lw = 2)

    # rho
    rq = [ρ.(lvecs[i],Ref(p[i]),Ref(m[i])) for i in 1:2]
    Qr = plot(lvecs[1],rq[1], label = "city 1", title = L"\rho", leg = false, 
    right_margin = 20Plots.mm, yscale = :log10, grid = :off)
    plot!(Qr,lvecs[2],rq[2], label = "city 2")
    hline!([ρ(0.0,p[1],m[1])], lc = :black, ls = :dash)

    pq = twinx(Qr)
    plot!(pq, lvecs[2], rq[1] .- rq[2], leg = :right, lc = :black, label = "diff", 
          xaxis=false, ylab = "difference", grid = :on)

    # U
    uq = [utility.(lvecs[i],Ref(p[i]),Ref(m[i])) for i in 1:2]

    o2 = plot(Wp,Dp,Qp,Qr, size = (800,500))

    savefig(o2,joinpath(dbplots(),string("hetr2-cross-section",fext,".pdf")))
    o,o2
end



function k20_dfexport(;overwrite = false)
    if overwrite
        df = FileIO.load(joinpath(intables, "d1d220.jld2"))
        d0 = df["d0"]
        d1 = df["d1"]
        C0 = df["C0"]
        C1 = df["C1"]
        p1 = df["p1"]
        p0 = df["p0"]

        # get exponential coefs for each city
        coefs0 = [Array(hcat(expmodel(c)[1]...)')[:,2] for c in C0]
        coefs1 = [Array(hcat(expmodel(c)[1]...)')[:,2] for c in C1]

        dc0 = DataFrame(gridmake(1:20, p0.T) , [:region, :year])
        dc0.exp_coef = vcat(coefs0...)
        dc0[!,:d1] .= 0.0
        dc0[!,:d2] .= 0.0
        dc0[!,:a ] .= p0.a

        dc1 = DataFrame(gridmake(1:20, p0.T) , [:region, :year])
        dc1.exp_coef = vcat(coefs1...)
        dc1[!,:d1] .= p1.d1
        dc1[!,:d2] .= p1.d2
        dc1[!,:a ] .= p1.a

        d0 = leftjoin(d0,dc0,on = [:region, :year])
        d1 = leftjoin(d1,dc1,on = [:region, :year])

        # build data export
        yearmap = popdata_mapyears(p0)
        # popdata_mapyears(p0)
        d0 = leftjoin(d0, yearmap, on = :year => :modelyears)
        d1 = leftjoin(d1, yearmap, on = :year => :modelyears)
        d0 = leftjoin(d0, p0.citylist, on = [:region => :rank, :datayears => :year],makeunique=true)
        d1 = leftjoin(d1, p0.citylist, on = [:region => :rank, :datayears => :year],makeunique=true)

        CSV.write(joinpath(dbtables(), "k20-baseline.csv"), d0)
        CSV.write(joinpath(dbtables(), "k20-d1d2.csv"), d1)
        FileIO.save(joinpath(intables, "k20-d1d2.jld2"), Dict("d0" => d0, "d1" => d1))

    else
		df = FileIO.load(joinpath(intables, "k20-d1d2.jld2"))
		d0 = df["d0"]
		d1 = df["d1"]
    end
    d0,d1
end


"""
Produces output from 20-city model. In particular comparison with single city case, and extension with d1, d2.
"""
function k20output(k;d1_ = 0.05,d2_ = 2.0, a = 2.67, overwrite = false)

    K = k
    if overwrite
        x,C0,p0 = runk(par = Dict(:K => K,:kshare => [1/K for i in 1:K], :factors => ones(k), :gs => zeros(k)))
        println("baseline done")
        x,C1,p1 = runk(par = Dict(:K => K,:kshare => [1/K for i in 1:K], :factors => ones(k), :gs => zeros(k), :d1 => d1_, :d2 => d2_, :a => a))
        d0 = dataframe(C0)
        d1 = dataframe(C1)

		FileIO.save(joinpath(intables, "d1d2$k.jld2"), Dict("d0" => d0, "d1" => d1, "p0" => p0, "p1" => p1, "C1" => C1, "C0" => C0))	
       
    else
        df = FileIO.load(joinpath(intables, "d1d2$k.jld2"))
		d0 = df["d0"]
		d1 = df["d1"]
        C0 = df["C0"]
		C1 = df["C1"]
		p1 = df["p1"]
		p0 = df["p0"]

    end

    # run a single city
    x,M,p = runm()

    p0x = select(subset(d0, :year => x->x.== 2020), :year, :Lu, :citydensity => firstnorm => :fn, :region) 
    pl0 = @df p0x bar(:fn,xticks = ([1,2,3],["Paris","Lyon","Marseille"]), ylab = "rel density", title = "baseline")
    annotate!(pl0, [(2,0.5, Plots.text("$(round(p0x[2,:fn],digits = 6))"))])

    p1x = select(subset(d1, :year => x->x.== 2020), :year, :Lu, :citydensity => firstnorm => :fn, :region)
    pl1 = @df p1x bar(:fn,xticks = ([1,2,3],["Paris","Lyon","Marseille"]), ylab = "rel density", title = "d1 = $d1_, d2 = $d2_")
    annotate!(pl1, [(2,0.5, Plots.text("$(round(p1x[2,:fn],digits = 6))"))])

    dd0 = select(subset(d0, :year => x->x.== 2020), :year, :Lu, :cityarea, :citydensity, :region)
    dd1 = select(subset(d1, :year => x->x.== 2020), :year, :Lu, :cityarea, :citydensity, :region)
    xx0 = lm(@formula( log(cityarea) ~ log(Lu) ), dd0)
    xx1 = lm(@formula( log(cityarea) ~ log(Lu) ), dd1)
    # return (d0, d1)
    # (xx0, xx1, d0, d1)
    b0 = bar([coef(xx0)[2]],ylims = (0,1), title = "baseline",annotations = (1.0, 0.8, Plots.text("coef = $(round(coef(xx0)[2],digits = 6))")))
    b1 = bar([coef(xx1)[2]],ylims = (0,1), title = "d1 = $d1_, d2 = $d2_",annotations = (1.0, 0.8, Plots.text("coef = $(round(coef(xx1)[2],digits = 6))")))

    s0 = @df dd0 scatter(:Lu, :cityarea, title = "baseline")
    s1 = @df dd1 scatter(:Lu, :cityarea, title = "d1 = $d1_, d2 = $d2_, a=$a")

    ts0 = ts_plots([C0[i].R[1] for i in 1:length(p0.T)], p0)
    ts1 = ts_plots([C1[i].R[1] for i in 1:length(p0.T)], p0)

    ts20 = ts_plots([C0[i].R[2] for i in 1:length(p0.T)], p0)
    ts21 = ts_plots([C1[i].R[2] for i in 1:length(p0.T)], p0)

    avg0 = select(d0, :year, :region, :citydensity, :cityarea, :d0)
    avg1 = select(d1, :year, :region, :citydensity, :cityarea, :d0)

    a0 = @df avg0 plot(:year, :citydensity, group = :region, title = "baseline av density" ,ylims = (0,maximum(avg0.citydensity)), leg = false)
    a1 = @df avg1 plot(:year, :citydensity, group = :region, title = "d1 = $d1_, d2 = $d2_, a=$a",ylims = (0,maximum(avg0.citydensity)), leg = false)

    phi0 = @df avg0 plot(:year, :cityarea, group = :region, title = "baseline cityarea", ylims = (0,maximum(avg1.cityarea)), leg = false)
    phi1 = @df avg1 plot(:year, :cityarea, group = :region, title = "d1 = $d1_, d2 = $d2_, a=$a", ylims = (0,maximum(avg1.cityarea)), leg = false)

    pout = plot(b0,b1, 
        #  plot(ts0[:n_densities],title = "baseline, k=1"), 
        #  plot(ts1[:n_densities], title = "d1 = $d1_, d2 = $d2_, k=1"),
        #  plot(ts20[:n_densities],title = "baseline, k=2"),
        #  plot(ts21[:n_densities],title = "d1 = $d1_, d2 = $d2_, k=2"),
         a0,a1,
         phi0,phi1, 
        #  layout = (5,2), size = (800,1100))
         layout = (3,2), size = (800,800))

    # compare single city to top 5 after 1880
    k5 = subset(d0, :region => x -> x .< 6, :year => x -> x .> 1870)

    # get single city plots
    tsp = ts_plots(M,p)

    # area 
    area1 = tsp[:phi]
    for ik in 1:5
        tk5 = subset(k5, :region => x -> x .== ik)
        plot!(area1, tk5.year, tk5.rel_cityarea, color = :grey, label = tk5)
    end

    area1

    # Dict(:C0 => C0,:p0=>p0, :C1 => C1, :p1=>p1, :d0=>d0 , :d1=>d1 ,:plot=>pout)
end




"""
run model with flat epsilon

https://github.com/floswald/LandUse.jl/issues/59
"""
function issue59(;save = false)
    p1 = Param()
    p2 = Param(par = Dict(:ϵflat => true, :ϵr => 4.0))

    x1,m1,p1 = run(p1)
    x2,m2,p2 = run(p2)

    d1 = dataframe(m1,p1); d2 = dataframe(m2,p2)
    d1[!,:type] .= "baseline"
    d2[!,:type] .= "ϵ(l) = 4"
    d = vcat(d1,d2)

    pl = Dict()
    pl[:density] = @df d plot(:year, :citydensity, group = :type, title = "Average Density")
    pl[:size] = @df d plot(:year, :cityarea, group = :type, title = "Urban Area", leg=false)
    pl[:qbar] = @df d plot(:year, :qbar_real, group = :type, title = "Avg House Price", leg=false)

    pl[:ndens0] = @df d plot(:year, :d0_n, group = :type, title = "Central n density", leg=false)
    pl[:ndensr] = @df d plot(:year, :dr_n, group = :type, title = "Fringe n density", leg=false)
    pl[:ndensa] = @df d plot(:year, :avgd_n, group = :type, title = "Avg n density", leg=false)
    o = plot(pl[:density], pl[:size], pl[:qbar], pl[:ndens0], pl[:ndensa], pl[:ndensr], layout = (2,3), size = (800,400))

    cs1 = cs_plots(m1[19],p1,19)
    cs2 = cs_plots(m2[19],p2,19)
    pl[:gradients] = plot(cs1[:D],cs2[:D], title = ["baseline" "flat ϵ"])

    if save 
        savefig(o,joinpath(dbplots(),"issue59.pdf"))
        savefig(pl[:gradients],joinpath(dbplots(),"issue59-gradients.pdf"))
    end
    pl
end


"""
run model with agglomeration forces

https://github.com/floswald/LandUse.jl/issues/60
"""
function issue60(;save = false)
    p1 = Param()
    p2 = Param(par = Dict(:η => 0.1))

    x1,m1,p1 = run(p1)
    x2,m2,p2 = run(p2)

    d1 = dataframe(m1,p1); d2 = dataframe(m2,p2)
    d1[!,:type] .= "baseline"
    d2[!,:type] .= "η = $(p2.η)"
    d = vcat(d1,d2)

    pl = Dict()
    pl[:density] = @df d plot(:year, :citydensity, group = :type, title = "Average Density")
    pl[:size] = @df d plot(:year, :cityarea, group = :type, title = "Urban Area", leg=false)
    pl[:qbar] = @df d plot(:year, :qbar_real, group = :type, title = "Avg House Price", leg=false)

    pl[:ndens0] = @df d plot(:year, :d0_n, group = :type, title = "Central n density", leg=false)
    pl[:ndensr] = @df d plot(:year, :dr_n, group = :type, title = "Fringe n density", leg=false)
    pl[:ndensa] = @df d plot(:year, :avgd_n, group = :type, title = "Avg n density", leg=false)
    o = plot(pl[:density], pl[:size], pl[:qbar], pl[:ndens0], pl[:ndensa], pl[:ndensr], layout = (2,3), size = (800,400))


    if save 
        savefig(o,joinpath(dbplots(),"issue60.pdf"))
        # for (k,v) in pl
        #     savefig(v,joinpath(dbplots(),"issue60-$(string(k)).pdf"))
        # end 
    end
    o
end


"""
run model with congestion forces

https://github.com/floswald/LandUse.jl/issues/64
"""
function issue64(;save = false)
    p1 = Param()
    p2 = Param(par = Dict(:ηa => 0.1))

    x1,m1,p1 = run(p1)
    x2,m2,p2 = run(p2)

    d1 = dataframe(m1,p1); d2 = dataframe(m2,p2)
    d1[!,:type] .= "baseline"
    d2[!,:type] .= "ηa = $(p2.μ)"
    transform!(d1, :imode => (x -> x ./ x[1]) => :imode_n)
    transform!(d2, :imode => (x -> x ./ x[1]) => :imode_n)
    d = vcat(d1,d2)

    pl = Dict()
    pl[:density] = @df d plot(:year, :citydensity, group = :type, title = "Average Density")
    pl[:size] = @df d plot(:year, :cityarea, group = :type, title = "Urban Area", leg=false)
    pl[:mode] = @df d plot(:year, :imode_n, group = :type, title = "Mode increase", leg=false)

    pl[:ndens0] = @df d plot(:year, :d0_n, group = :type, title = "Central n density", leg=false)
    pl[:ndensr] = @df d plot(:year, :dr_n, group = :type, title = "Fringe n density", leg=false)
    pl[:ndensa] = @df d plot(:year, :avgd_n, group = :type, title = "Avg n density", leg=false)
    o = plot(pl[:density], pl[:size], pl[:mode], pl[:ndens0], pl[:ndensa], pl[:ndensr], layout = (2,3), size = (800,400))


    if save 
        savefig(o,joinpath(dbplots(),"issue64.pdf"))
        # for (k,v) in pl
        #     savefig(v,joinpath(dbplots(),"issue60-$(string(k)).pdf"))
        # end 
    end
    o
end


"""
run multi city with congestion

https://github.com/floswald/LandUse.jl/issues/66
"""
function issue66(;save = false)
    p1 = Param()
    p2 = Param(par = Dict(:ηa => 0.1))
    d1 = Dict(:K => 3, :kshare => [1/3,1/3,1/3], :factors => [1.0,1.01,1.1])
    d2 = Dict(:K => 3, :kshare => [1/3,1/3,1/3], :factors => [1.0,1.01,1.1], :ηa => 0.1)
    

    x1,m1,p1 = runk(par = d1)
    x2,m2,p2 = runk(par = d2)

    c1 = dashboard(m1, 19)
    c2 = dashboard(m2, 19)

    o = plot(c1, c2, size = (2200,700))


    if save 
        savefig(o,joinpath(dbplots(),"issue64.pdf"))
        # for (k,v) in pl
        #     savefig(v,joinpath(dbplots(),"issue60-$(string(k)).pdf"))
        # end 
    end
    o
end


"""
five city cross section

https://github.com/floswald/LandUse.jl/issues/67
"""
function issue67()
    x,C,p = k5()
    d = dataframe(C)
    g = groupby(d, :region)

    dd = select(transform(g, :Lu => firstnorm => :Lun, 
                             :cityarea => firstnorm => :cityarean,
                             :citydensity => firstnorm => :citydensityn), 
                 :year, :Lu, :cityarea, :Lun, :cityarean,:citydensity,:citydensityn, :region)

    pl = Dict()

    # ratio of smallest to largest city's population over time
    gg = groupby(select(filter(x -> x.region .∈ Ref([1,5]), d), :region, :year, :Lu), :year)
    dg = combine(gg, :Lu => (x -> maximum(x) / minimum(x)) => :rel_Lu)

    # Relative Population and Area in final period
    # ============================================

    data = CSV.File(joinpath(dboutdata(),"top5poparea.csv")) |> DataFrame
    data[!,:region] = 1:5
    data[!,:group] .= "data"
    sort!(data, :region)
    
    m = select(subset(d, :year => x -> x .== 2020), :region, :Lu => (x -> x ./ maximum(x)) => :relative_pop, :cityarea => (x -> x ./ maximum(x)) => :relative_area)
    m[!,:group] .= "model"

    dm = vcat(select(data,Not(:LIBGEO)),m)
    dm = leftjoin(dm, select(data,:region, :LIBGEO => :city), on = :region)

    pl[:relpop] = @df dm groupedbar(:relative_pop, group = :group, legend = :left, xticks = (:region,:city), title = "2020 Population relative to Paris")
    pl[:relarea] = @df dm groupedbar(:relative_area, group = :group, legend = :left, xticks = (:region,:city), title = "2020 Area relative to Paris")
    savefig(pl[:relpop], joinpath(dbplots(),"five-city-relpop.pdf"))
    savefig(pl[:relarea], joinpath(dbplots(),"five-city-relarea.pdf"))


    # commuting time
    # ==============
    ct = combine(g, :ictime => last, :imode => last)
    rct = transform(ct, :ictime_last => maxnorm => :reltime, :imode_last => maxnorm => :relspeed)
    rct = leftjoin(rct, select(data,:region,:LIBGEO => :city), on = :region)
    sct = stack(select(rct,:city, :region, :reltime, :relspeed), Not([:region, :city]))

    pl[:relspeedtime] = @df sct groupedbar(:value, group = :variable, legend = :topleft, xticks = (:region,:city), title = "Average Speed and Commute Time rel to Paris")
    savefig(pl[:relspeedtime], joinpath(dbplots(),"five-city-relspeedtime.pdf"))





    # Density
    # =======

    # cross section: bigger cities are denser, in all periods
    pl[:cross] = @df dd plot(:year, :citydensity, group = :region, yaxis = :log10, ylab = "log density", title = "Bigger cities are always denser.")
    savefig(pl[:cross], joinpath(dbplots(),"five-city-cross.pdf"))
    
    # over time, the fall in density is more pronounced in large cities than in smaller ones
    pl[:time] = @df dd plot(:year, :citydensityn, group = :region, ylab = "normalized density (1840 = 1)", title = "...but they become less dense faster!")
    savefig(pl[:time], joinpath(dbplots(),"five-city-time.pdf"))

    # show relative density and population over time
    pk = @df subset(dd, :region => x -> x .== 1) plot(:citydensityn, :Lun, series_annotation = Plots.text.(:year, 8, :right),xlab = "relative density (1840 = 1)",
     ylab = "relative Population (1840 = 1)", title = "Density vs Urban Population", label = "1")
    for ik in 2:5
        ds = subset(dd, :region => x -> x .== ik)
        if ik < 5
            plot!(pk, ds.citydensityn, ds.Lun, label = "$ik")
        else
            years = string.(collect(p.T))
            years[Not([1,5,10,15,19])] .= ""
            plot!(pk, ds.citydensityn, ds.Lun, label = "$ik", series_annotation = Plots.text.(years, 8, :right))
        end
    end
    pl[:rel] = pk
    savefig(pl[:rel], joinpath(dbplots(),"five-city-rel.pdf"))
    (pl,d,g,dg)
end



"""
plot ``\\phi_k`` vs ``\\L{u,k}`` for all regions ``k``.
relates to https://github.com/floswald/LandUse.jl/issues/9
"""
function issue9()
    cpar = Dict(:S => 1.0, :L => 1.0,
                :K => 4,
                :θprop => [1.0,0.99,0.98,0.97],
                :kshare => [0.25 for i in 1:4])
    sols,C,cpar,pp = runk(cpar = cpar)

    K = C[1].K

    open(joinpath(dbtables(),"phi-vs-Lu.txt"),"w") do io
        @printf(io,"Year   k    θu      Lu       ϕ\n")
        @printf(io,"----------------------------------\n")

        anim = Animation()
        for (jt,it) in enumerate(C[1].T)
            ϕs = [C[jt].R[ik].ϕ for ik in 1:K]; Lus = [C[jt].R[ik].Lu for ik in 1:K]
            for ik in 1:K @printf(io,"%d   %d    %1.2f    %1.3f    %1.3f\n",it,ik,pp[ik].θus[jt]*pp[ik].θprop,Lus[ik],ϕs[ik]) end

            pl = plot(ϕs, Lus, title = "$it",
                      leg = false,
                      l = (:black,2),
                      m = (:circle, 5, :red),
                      xaxis = (L"\phi", (0.001,0.045)),
                      yaxis = (L"L_u", (0.01,0.3)))
            frame(anim)
        end
        g = gif(anim, joinpath(dbplots(),"phi-vs-Lu.gif"),fps=0.5)
    end

end

"""
plot ``\\phi_k`` vs ``L_{u,k}`` for all regions ``k`` and different eps slopes.
relates to https://github.com/floswald/LandUse.jl/issues/10
"""
function issue10()
    cpar = Dict(:S => 1.0, :L => 1.0,
                :K => 4,
                :θprop => [1.0,0.97,0.95,0.94],
                :kshare => [0.25 for i in 1:4])
    sols,C,cp,p0 = runk(cpar = cpar)

    K = C[1].K
    ϵ1 = 5.0
    ϵ2 = 1.0
    ϵ3 = 0.0
    ϵs = [p0[1].ϵs,ϵ1,ϵ2,ϵ3]

    # version with lower eps slope
    sols,C2,cp,pp = runk(cpar = cpar,
                                 par = Dict(ik => Dict(:ϵsmax => ϵs[2]) for ik in 1:K))

    # version with lower eps slope
    sols,C3,cp,pp = runk(cpar = cpar,
                                 par = Dict(ik => Dict(:ϵsmax => ϵs[3]) for ik in 1:K))

    # version with lower eps slope
    sols,C4,cp,pp = runk(cpar = cpar,
                              par = Dict(ik => Dict(:ϵsmax => ϵs[4]) for ik in 1:K))

    df = DataFrame()
    fmt = FormatExpr("{1:d}    {2:d}   {3:>4.1f}    {4:>1.2f}    {5:>1.2f}    {6:>1.2f}")
    # open(joinpath(dbtables(),"phi-vs-Lu-eps.txt"),"w") do io
        # print(io,"Year    k   ϵs      θu      Lu      ϕ\n")
        # print(io,"----------------------------------------\n")

        anim = Animation()
        for (jt,it) in enumerate(C[1].T)
            # ϕs =  log.(1000 .* [C[jt].R[ik].ϕ for ik in 1:K]);   Lus = log.(1000 .* [C[jt].R[ik].Lu for ik in 1:K])
            # ϕs2 = log.(1000 .* [C2[jt].R[ik].ϕ for ik in 1:K]); Lus2 = log.(1000 .* [C2[jt].R[ik].Lu for ik in 1:K])
            # ϕs3 = log.(1000 .* [C3[jt].R[ik].ϕ for ik in 1:K]); Lus3 = log.(1000 .* [C3[jt].R[ik].Lu for ik in 1:K])
            # ϕs4 = log.(1000 .* [C4[jt].R[ik].ϕ for ik in 1:K]); Lus4 = log.(1000 .* [C4[jt].R[ik].Lu for ik in 1:K])

            ϕs =  [C[jt].R[ik].ϕ for ik in 1:K];   Lus= [C[jt].R[ik].Lu for ik in 1:K]
            ϕs2 = [C2[jt].R[ik].ϕ for ik in 1:K]; Lus2 =[C2[jt].R[ik].Lu for ik in 1:K]
            ϕs3 = [C3[jt].R[ik].ϕ for ik in 1:K]; Lus3 =[C3[jt].R[ik].Lu for ik in 1:K]
            ϕs4 = [C4[jt].R[ik].ϕ for ik in 1:K]; Lus4 =[C4[jt].R[ik].Lu for ik in 1:K]

            df1 = DataFrame(year = it, k = 1:K , ϵs = ϵs[1], θu = [pp[ik].θus[jt]*pp[ik].θprop for ik in 1:K] , Lu = Lus, ϕ= ϕs)
            df2 = DataFrame(year = it, k = 1:K , ϵs = ϵs[2], θu = [pp[ik].θus[jt]*pp[ik].θprop for ik in 1:K] , Lu = Lus2, ϕ= ϕs2)
            df3 = DataFrame(year = it, k = 1:K , ϵs = ϵs[3], θu = [pp[ik].θus[jt]*pp[ik].θprop for ik in 1:K] , Lu = Lus3, ϕ= ϕs3)
            df4 = DataFrame(year = it, k = 1:K , ϵs = ϵs[4], θu = [pp[ik].θus[jt]*pp[ik].θprop for ik in 1:K] , Lu = Lus4, ϕ= ϕs4)
            append!(df,df1)
            append!(df,df2)
            append!(df,df3)
            append!(df,df4)
            pl = plot(Lus, ϕs, title = "$it",
                      label = latexstring("\\epsilon = $(ϵs[1])"),
                      legend = :bottomright,
                      l = (:black,2),
                      m = (:circle, 5, :red),
                      yaxis = (L"\log \phi", :log10, (0.0001, 0.06)),
                      xaxis = (L"\log L_u", :log10, (0.001, 0.6)))
                      # yaxis = (L"\phi", (-1.,6)),
                      # xaxis = (L"L_u", (1.01,7)))
            plot!(pl,Lus2, ϕs2,
                      label = latexstring("\\epsilon = $(ϵs[2])"),
                      l = (:green,2),
                      m = (:circle, 5, :red))
            plot!(pl,  Lus3,ϕs3,
                     label = latexstring("\\epsilon = $(ϵs[3])"),
                     l = (:blue,2),
                     m = (:circle, 5, :red))
            plot!(pl,Lus4, ϕs4,
                   label = latexstring("\\epsilon = $(ϵs[4])"),
                   l = (:orange,2),
                   m = (:circle, 5, :red))

            frame(anim)
            # for ik in 1:K printfmtln(io,fmt,it,ik,ϵs[ik],pp[ik].θus[jt]*pp[ik].θprop,Lus[ik],ϕs[ik]) end

    end
    g = gif(anim, joinpath(dbplots(),"phi-vs-Lu-eps.gif"),fps=0.5)
    df
end

"""
measure increase in land value at fringe. by default starts in 1960 and measures
increase up to 2020.

https://github.com/floswald/LandUse.jl/issues/11
"""
function issue11(;istart = 8, istop = 12, discount::Float64 = 0.03)
    (x,M,p) = run(Param())  # run standard single region

    # get location of fringe in start
    ϕstart = M[istart].ϕ
    ϕstop = M[istop].ϕ

    # land value in year `stop` at that distance
    setperiod!(p,istop)
    ρstart = ρ(ϕstart,p,M[istop])
    ρrstop = M[istop].ρr  # ρ at fringe in year stop

    # difference discounted back to 1990
    dd = (ρstart - ρrstop) / ((1 + discount)^5)^(istop-istart)
    out1 = dd / M[istart].ρr
    println("land value at fringe increased by $(round(out1,digits = 2)) percent \n from $(p.T[istart]) to $(p.T[istop]), discounted at $(100*discount) percent p.a.")

    # with flat epsilon
    (x,M,p) = run(Param(par = Dict(:ϵsmax => 0.0)))

    # get location of fringe in start
    ϕstart = M[istart].ϕ
    ϕstop = M[istop].ϕ

    # land value in year `stop` at that distance
    setperiod!(p,istop)
    ρstart = ρ(ϕstart,p,M[istop])
    ρrstop = M[istop].ρr  # ρ at fringe in year stop

    # difference discounted back to 1990
    dd = (ρstart - ρrstop) / ((1 + discount)^5)^(istop-istart)
    out2 = dd / M[istart].ρr
    println("with ϵ flat:")
    println("land value at fringe increased by $(round(out2,digits = 2)) percent \n from $(p.T[istart]) to $(p.T[istop]), discounted at $(100*discount) percent p.a.")
    out1,out2
end

function issue12_gif(n)
    us = range(1.01,1.04,length = n)
    ϵs = round.(range(1.0,5,length=8),digits=1)
    anim = Animation()
    for ie in ϵs
        x = issue12(ie,gu = us)
        frame(anim, x[6])
    end
    g = gif(anim, joinpath(dbplots(),"issue12-$n.gif"),fps=0.5)
    g
end


# LandUse.runk(par =
#     Dict(1 => Dict(:θut=> 1.0, :θrt=> 1.0,:θu_g => 1.2, :θr_g => 1.2),
#          2 => Dict(:θut=> 1.0, :θrt=> 1.0,:θu_g => 1.19, :θr_g => 1.2)))


# multik(Dict(1 => Dict(:θut=> 1.0, :θrt=> 1.0,:θu_g => 1.2, :θr_g => 1.2), 2 => Dict(:θut=> 1.0, :θrt=> 1.0,:θu_g => 1.19, :θr_g => 1.2)))


"""
Runs K regions as a country and produces 2 plots
"""
function multik(ppar::Dict; save = false)
    K = length(ppar)

    cpar = Dict(:S => 1.0, :L => 1.0,
                :K => K,
                :kshare => [1/K for i in 1:K])

    sols,C,cpar,pp = runk(cpar = cpar,par = ppar)
    pl = plot(impl_plot_ts_all(C)..., layout = (2,2))

    if save savefig(pl,joinpath(dbplots(),"multi-$K-TS.pdf")) end

    pl2 = impl_plot_slopes(C)
                    # title = latexstring("City size vs pop \$\\epsilon\$ = $ϵ"))
    if save savefig(pl2,joinpath(dbplots(),"multi-$K-phi-Lu.pdf")) end
    sols,C,cpar,pp,pl,pl2
end

# function issue12_1(ϵ; gu = [1.06,1.05], gr = 1.06)
#     cpar = Dict(:S => 1.0, :L => 1.0,
#                 :K => 2,
#                 :kshare => [1/2 for i in 1:2])
#
#     ppar = Dict(i => Dict(:ϵsmax => 0.0,:θagg => [0.32],
#                            :θrt => [0.32],:θut => [0.32], :θu_g => gu[i] ,
#                            :θr_g => gr , :ϵr => ϵ ,:ϵs => 0.0)
#                            for i in 1:2)
#     sols,C,cpar,pp = LandUse.runk(cpar = cpar,par = ppar)
#     pl = LandUse.plot_ts_all(C)
#     savefig(pl,joinpath(dbplots(),"multi-2-TS.pdf"))
#
#     d = dataframe(C)
#     d.lϕ = log.(d.ϕ)
#     d.lu = log.(d.Lu)
#     gd = groupby(d,:year)
#     gd = combine(gd, AsTable([:lϕ, :lu]) => (x -> round.(diff(vcat(extrema(x.lu)...)) ./ diff(vcat(extrema(x.lϕ)...)),digits = 1)) => :slope)
#     d  = innerjoin(d,gd,on = :year)
#     transform!(d, AsTable([:year, :slope]) => (x -> string.(x.year) .* ": slope=" .* string.(x.slope) ) => :year_s)
#
#     cols = range(colorant"red",colorant"blue",length = length(pp[1].T))
#     dd = select(d, :year_s, :region, :lϕ, :lu)
#     pl2 = @df dd plot(:lϕ,:lu,group = :year_s,
#                     xlab = L"\log \phi",
#                     ylab = L"\log L_u",
#                     marker = (:circle, 4),
#                     colour = cols',
#                     legend = :topleft)
#     savefig(pl2,joinpath(dbplots(),"multi-2-phi-Lu.pdf"))
#     sols,C,cpar,pp,pl,pl2
# end

# """
# https://github.com/floswald/LandUse.jl/issues/22
# """
# function issue_22()
#     x,M,p = run(Region,Param())
#     d = dataframe(M,p)
#     df = @linq d |>
#          select(:year,:Ch,:Cu,:Cr,:C ) |>
#          transform(:h = :Ch ./ :C,:u = :Cu ./ :C,:r = :Cr ./ :C) |>
#          select(:year, :h, :u , :r)
#     ds = stack(df, Not(:year))
#     pl = @df ds plot(:year,:value, group = :variable,
#                linewidth = 2, title = "Spending Shares")
#     savefig(pl, joinpath(dbplots(),"spending-shares.pdf"))
#     pl
# end


"""
https://github.com/floswald/LandUse.jl/issues/21
"""
function issue_21(n=8)

    es = range(0.5,8,length = n)
    e = es[1]
    x,M,p = run(Region,Param(par = Dict(:ϵsmax => 0.0, :ϵr => e)))
    # d = select(dataframe(M,p),:year,:Hr,:H0,:hr,:h0,:ρr,:qr,:ρ0,:q0, :ϕ, :Sr, :Srh)
    d = dataframe(M,p)
    d[!,:ϵr] .= e

    for (i,e) in enumerate(es[2:end])
        # println("e = $e")
        x,M,p = run(Region,Param(par = Dict(:ϵsmax => 0.0, :ϵr => e)))
        d0 = dataframe(M,p)
        d0[!,:ϵr] .= e
        append!(d,d0)
    end

    ds = stack(d,Not([:year,:ϵr]))
    # sims = [[:ϕ],[:q0, :ρ0] , [:qr, :ρr], [:Hr , :hr],[:H0 , :h0],[:Sr , :Srh]]
    sims = [:ϕ,:Sr,:Srh,:d0,
            :dr,:q0 ,:qr,:ρ0 ,
            :ρr, :H0,:Hr,:h0,
            :hr , :Lu, :Lr, :r]
    titles = ["phi"; "Sr"; "Srh"; "Central Density";
               "Fringe Density";"Central price q0"; "Fringe prices qr"; "Central land rho 0";
              "Fringe land rho r"; "Central H supply"; "Fringe H supply"; "Central H demand";
              "Fringe H demand"; "Lu"; "Lr"; "r"]

    plt = Any[]
    colors = setup_color(n)
    # colors = colormap("blues",n)
    for i in 1:length(sims)
        x = @linq ds |>
            where((:variable .== sims[i]))
            # where((:variable .∈ Ref(sims[i])))
        px = @df x plot(:year, :value, group = :ϵr,
                        title = titles[i],
                        titlefontsize=10,
                        color = colors',
                        # label=nms[i],
                        legend = false,
                        linewidth=2,marker = (:circle,3))
        push!(plt, px)
    end
    # plot(plt...,layout = (2,3))
    # ds

    plt_l = scatter(zeros(n), 1e-32 .* ones(n), zcolor = es,
            color = :inferno, colorbar = true, colorbar_title = L"\epsilon_r", legend = false,
            grid = false, axis = false, markerstrokealpha = 0, markersize = 0.1)
    psize=(1200,700)
    l = @layout [ [a b c d
                  e f g h
                  i j k m
                  q r s t] u{0.05w}]
    p = plot(plt..., plt_l, size=psize, layout = l)
    savefig(p,joinpath(dbplots(),"varyepsr.pdf"))
    p

    # @df plot(:year,)
end


"""
https://github.com/floswald/LandUse.jl/issues/15
"""
function issue15()
    cpar = Dict(:S => 1.0, :L => 1.0,
                :K => 4,
                :θg => [1.1,1.0,0.98,0.96],
                :kshare => [0.25 for i in 1:4])
    sols,C,cpar,pp = runk(cpar = cpar)
end

function fixed_ρ()
    p0=Param(par = Dict(:ϵsmax =>0.0))
    fixed_rho(p0, fi = "fixed-rho")
    p0=Param(par = Dict(:θug => [1.2 for i in 1:14],
                  :θrg => [1.2 for i in 1:14],
                   :ϵsmax =>0.0))
    fixed_rho(p0, fi = "fixed-rho-constg")
    p0=Param(par = Dict(:θug => [1.2 for i in 1:14],
                  :θrg => [1.15 for i in 1:14],
                   :ϵsmax =>0.0))
    fixed_rho(p0, fi = "fixed-rho-highu-g")
    p0=Param(par = Dict(:θug => [1.15 for i in 1:14],
                  :θrg => [1.2 for i in 1:14],
                   :ϵsmax =>0.0))
    fixed_rho(p0, fi = "fixed-rho-highr-g")

    p0=Param(par = Dict(:ϵsmax =>0.0,:σ => 0.4))
    fixed_rho(p0, fi = "fixed-rho-lowsig")
    p0=Param(par = Dict(:θug => [1.2 for i in 1:14],
                  :θrg => [1.2 for i in 1:14],
                   :ϵsmax =>0.0,:σ => 0.4))
    fixed_rho(p0, fi = "fixed-rho-constg-lowsig")
    p0=Param(par = Dict(:θug => [1.2 for i in 1:14],
                  :θrg => [1.15 for i in 1:14],
                   :ϵsmax =>0.0,:σ => 0.4))
    fixed_rho(p0, fi = "fixed-rho-highu-g-lowsig")
    p0=Param(par = Dict(:θug => [1.15 for i in 1:14],
                  :θrg => [1.2 for i in 1:14],
                   :ϵsmax =>0.0,:σ => 0.4))
    fixed_rho(p0, fi = "fixed-rho-highr-g-lowsig")

end

function fixed_rho(p::Param; fi = nothing)
    pyplot()

    x,Mr,p = run(Region,p)
    p.ρrbar = Mr[1].ρr
    x,Mu,p = run(Urban,p)

    dr = dataframe(Mr,p)
    du = dataframe(Mu,p)
    dr.model = ["baseline" for i in 1:nrow(dr)]
    du.model = ["urban" for i in 1:nrow(du)]

    # nms = [:year,:model,:area,:Lu,:ϕ, :ρr]
    # nms = [:year,:model,:area, :ρ0 ,:ϕ, :ρr]
    sims = [:Lu, :ϕ, :r, :q0, :ρ0 ,:qr, :ρr, :wr , :wu0]
    lat1 = latexstring("\$L_u\$; \$\\sigma\$=$(p.σ)")
    tis = [lat1, L"\phi", "r", L"q_0", L"\rho_0" ,L"q_r", L"\rho_r", "wr, gr=$(p.θrg[1])" ,"wu0, gu=$(p.θug[1])"]
    nms = [:year , :model , sims...]

    drs = stack(select(dr, nms, Not([:year,:model])))
    dus = stack(select(du, nms, Not([:year,:model])))
    dd = [drs; dus]

    pnms = sims
    plt = Any[]
    # tis = [:area :Lu :phi :rho_r]
    # tis = [:area :ρ0 :phi :rho_r]
    for i in 1:length(pnms)
        x = @linq dd |>
            where(:variable .== pnms[i])
        px = @df x plot(:year, :value, group = :model,
                        title = tis[i],
                        titlefontsize=12,
                        label = i == 1 ? ["baseline" "urban"] : "",
                        # legend = :topleft,
                        linewidth=2,marker = (:circle,4))
        push!(plt, px)
    end
    pl = plot(plt...)
    if isnothing(fi)
        fin = "fixed-rho.pdf"
    else
        fin = "$fi.pdf"
    end
    savefig(pl,joinpath(dbplots(),fin))
    pl
end


function output_3Ms()

    # single region
    x,M,p = run(Region,Param(par = Dict(:ζ => 0.5, :τ1 => 0.98)))
    dd = ts_plots(M,p)

    si = Dict()
    wihe = (700,340)
    si[:alloc] = plot(dd[:pop],dd[:spending], size = wihe)
    si[:space] = plot(dd[:Sr],dd[:phi], size = wihe)
    si[:h]     = plot(dd[:hr100],dd[:Hr100], size = wihe)
    si[:dens]  = plot(dd[:avdensity],dd[:densities], size = wihe)
    si[:r_rho] = plot(dd[:r_rho], size = wihe)
    si[:r_y] = plot(dd[:r_y], size = wihe)

    for (k,v) in si
        savefig(v, joinpath(dbplots(),"$k.pdf"))
    end

    return si





    # multi regions
    mm = multik(
               Dict(1 => Dict(:θut=> 1.01, :θrt=> 1.0,:θu_g => 1.31, :θr_g => 1.2, :ζ => 0.5),
                    2 => Dict(:θut=> 1.0, :θrt=> 1.0,:θu_g => 1.3, :θr_g => 1.2, :ζ => 0.5)))
    savefig(mm[5],joinpath(dbplots(),"multi2.pdf"))
    # Labor Alloc and City size
    # p1 = plot(dd[:pop])

end

"""
    https://github.com/floswald/LandUse.jl/issues/36

1. same growth in sectors
    i. high cbar vs low sbar: show implied city density time series to see that only that config works
    ii. show falling housing spending share as well
2. implications of growth in either sector only
3. identify commuting cost params by matching time series data
"""
function issue36( ; save=false)
    r = Dict()

    emptyplot = plot(legend=false,grid=false,foreground_color_subplot=:white)
    size1by2 = (800,400)

    # 1. same growth in sectors
    #     i. high cbar vs low sbar: show implied city density time series to see that only that config works
    #     ii. show falling housing spending share as well
    r[1] = Dict()

    p1  = Param() # baseline param: high cbar and low sbar
    x,M,p0  = run(p1)
    pl1 = ts_plots(M,p1)

    r[1][:baselinestats] = Dict(:ruralpop => Dict(:from => M[1].Lr , :to => M[end].Lr / p1.Lt[end]),
                                :rspend => Dict(:from => M[1].Cr / M[1].C, :to => M[end].Cr / M[end].C),
                                :phi => Dict(:from => M[1].ϕ, :to => M[end].ϕ ))
    r[1][:baseline1] = plot(pl1[:Lr_data],pl1[:pop],pl1[:n_densities],pl1[:r_y],
                                layout = (2,2))
    r[1][:baseline2] = plot(pl1[:avdensity_pop],pl1[:n_densities],layout = (1,2),
                            size = size1by2)

    r[1][:prices] = plot(pl1[:r_real],pl1[:r_y], layout = (1,2))
    r[1][:baseline_fit] = plot(pl1[:Lr_data],pl1[:spending],pl1[:n_densities],pl1[:avdensity],
                                layout = (2,2)) #




    p2 = Param(par = Dict(:cbar => 0.4, :sbar => 0.7)) # low cbar and high sbar
    x,M,p0  = run(p2)
    pl2 = ts_plots(M,p2)
    r[1][:low_cbar] = plot(pl2[:Lr_data],pl2[:spending],pl2[:avdensity],pl2[:r_y],
                                layout = (2,2),link = :x)


    # 2. implications of growth in either sector only
    # i. u grows faster than r
    r[2] = Dict()
    p3 = Param(par = Dict(:θrt => 1.0,:θr_g => 1.05))
    # p2 = Param(par = Dict(:θu_g => 1.08,:θut => 1.0, :θrt => 1.0,:θr_g => 1.01))
    x,M,p0  = run(p3)
    pl3 = ts_plots(M,p3)
    r[2][:u_fast1] = plot(pl3[:Lr_data],pl3[:spending],pl3[:r_y],emptyplot,
                                layout = (2,2))
    r[2][:u_fast2] = plot(pl3[:avdensity_pop],plot(pl3[:n_densities],leg = :bottomleft),layout = (1,2),
                            size = size1by2)

    # p3 = Param(par = Dict(:θu_g => 1.01,:θut => 1.0, :θrt => 1.0,:θr_g => 1.09))
    p4 = Param(par = Dict(:θu_g => 1.0,:θut => 1.0))
    x,M,p0  = run(p4)
    pl4 = ts_plots(M,p4)
    r[2][:r_fast1] = plot(pl4[:Lr_data],pl4[:spending],pl4[:r_y],emptyplot,
                                layout = (2,2))
    r[2][:r_fast2] = plot(pl4[:avdensity_pop],plot(pl4[:n_densities],leg = :bottomleft),layout = (1,2),
                            size = size1by2)

    r[3] = plot(pl1[:mode],pl1[:ctime],layout = (2,1), link = :x)

    # 4. constant growth in both sectors
    r[4] = Dict()
    p = Param(par = Dict(:θu_g => 1.25,:θut => 1.0, :θrt => 1.0,:θr_g => 1.25))
    x,M,p0  = run(p)
    pl5 = ts_plots(M,p)
    r[4] = plot(pl5[:Lr_data],pl5[:spending],pl5[:avdensity],pl5[:r_y],
                                layout = (2,2),link = :x)



    # save plots
    if save
        savefig(r[1][:baseline1], joinpath(dbplots(),"issue36-baseline.pdf"))
        savefig(r[1][:baseline_fit], joinpath(dbplots(),"issue36-baseline-fit.pdf"))

        savefig(r[1][:low_cbar], joinpath(dbplots(),"issue36-low-cbar.pdf"))
        savefig(r[1][:prices], joinpath(dbplots(),"issue36-prices.pdf"))
        savefig(r[2][:u_fast1], joinpath(dbplots(),"issue36-u-fast.pdf"))
        savefig(r[2][:r_fast1], joinpath(dbplots(),"issue36-r-fast.pdf"))
        savefig(r[4], joinpath(dbplots(),"issue36-constant.pdf"))
        savefig(r[3], joinpath(dbplots(),"issue36-commute.pdf"))

        # changing size
        savefig(r[1][:baseline2], joinpath(dbplots(),"issue36-baseline2.pdf"))
        savefig(r[2][:u_fast2], joinpath(dbplots(),"issue36-u-fast2.pdf"))
        savefig(r[2][:r_fast2], joinpath(dbplots(),"issue36-r-fast2.pdf"))



    end
    return r

end


"""
produces all output from the model needed to compile the paper.

This uses current baseline parameters defined in `params.json`
"""
function output_paper(;save = false)

    p1  = Param() # baseline param: high cbar and low sbar
    x,M,p0  = run(Region,p1)
    pl1 = ts_plots(M,p1)

    # names of plots we want
    key = [:pop,:spending,:avdensity,:phi,:densities,:r_y,:r_rho,:q0_qr,:mode,:ctime]

    # subset to those
    r = filter( x -> x.first ∈ key , pl1)

    # save if wanted
    if save
        for (k,v) in r
            savefig(v, joinpath(dbplots(),"$k.pdf"))
        end
    end
    r
end
