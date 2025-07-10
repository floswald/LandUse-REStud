# this file contains functions which make a high sbar version of numerical illustration 1.

# make 5 versions of the single region model.
# city 3 is baseline city
# there are 2 values high/low for each sector offset
# cities 1,2,4,5 are [
#             urban
#         low       high
#     low   
# rural
#     high
# ]
# constant growth in each sector
function param_4_cities_sbar(sbar; g = 1.2)

    # baseline param 
    K = 4
    p_search = Param(par = Dict(:T => 1840:10:2020, :gsr => zeros(K),:gs => zeros(K),:kshare => [1/K for i in 1:K],:factors => ones(K),:K => K,:𝕊 => zeros(60,K), :Lflat => true,
    :cbar => 0.6,
    :sbar => 0.6,  # start here, then move up
    :a => 0.5,
    :γ => 0.3,
    :ν => 0.025,
    :ϵflat => true,
    :ϵr => 4.0,
    :ξl => 0.8,
    :ξw => 0.8
    ))

    p = Param(par = Dict(:T => 1840:10:2020, :gsr => zeros(K),:gs => zeros(K),:kshare => [1/K for i in 1:K],:factors => ones(K),:K => K,:𝕊 => zeros(60,K), :Lflat => true,
    :cbar => 0.6,
    :sbar => sbar,
    :a => 3.0,
    :γ => 0.3,
    :ν => 0.025,
    :ϵflat => true,
    :ϵr => 4.0,
    :ξl => 0.8,
    :ξw => 0.8
    ))

    # set growth to g
    constant_growth_θs!(p_search, g, g)
    constant_growth_θs!(p, g, g)

    return p,p_search
end


function start_4cities_sbar(p::Param,sbar)
    # start a single city
    setperiod!(p,1)
	x0 = nearstart(p)
	m = Region(p)
	x0 = jm(p,x0, estimateθ = false)

    # step-wise increase sbar now
    s0 = p.sbar
    for isbar in s0:0.01:sbar
        p.sbar = isbar
        @debug p.sbar
        x0 = jm(p,x0, estimateθ = false)
    end

	update!(m,p,x0)

    (p = p,x0 = (Lr = [m.Lr for i in 1:p.K],
		r = m.r,
		pr = m.pr,
		Sr = [m.Sr for i in 1:p.K],
		ϕ = [m.ϕ for i in 1:p.K],
		Lu = [m.Lu for i in 1:p.K], 
		ρr = [m.ρr for i in 1:p.K]
	))

end



function app_numillustration_sbar(; overwrite = false, save = false, sbar = 0.9)
    pth = joinpath(dbplots(), "revision$(revision())","appendix-numillust-sbar")
    mkpath(pth)

    offs = OrderedDict("low θr" => 0.95,"high θr" => 1.05,"low θu" => 0.95,"high θu" => 1.05)

    if overwrite
    
        # 5 city param
        p,psearch = param_4_cities_sbar( sbar )

    
        # get a starting values
        out = start_4cities_sbar(psearch,0.98)  # 1.4 = 2*cbar

            
        # get artificial θ series 
        arti = artificialθ(p,offs)

        # run model 
        d = app_numillustration_impl_(p,out.x0,arti)

        # save
        JLD2.jldsave(joinpath(pth,"data.jld2"); d)
    else
        d = JLD2.load_object(joinpath(pth,"data.jld2"))
    end
    # return d
    # make desired plot panels, each with 5 lines (one for each city)
    # return d
    f = app_numillustration_plot(d[1],offs, highsbar = true)

    if save
        for (k,v) in f
            savefig(v, joinpath(pth,"$k.pdf"))
        end
    end
    f

end

# no pop growth
# same values as described
# 4 cities, and their aggregation
# density: normalize wrt first value of aggregate
# pop Lu: normalize wrt first value of aggregate. 
# urban area: normalize wrt first value of aggregate

# 9 panels in total
# row 1, agg only: Lr, spending, food price
# row 2, agg only: Lu+area, densities, rural and urban land rent share 
# row 3, spreads: Lu, cityarea, avg density
function app_numillustration_sbar_plot(d::DataFrame,offs::OrderedDict)

    K = length(unique(d.region))
    
    d[!,:LIBGEO] .= ""
    ko = collect(keys(offs))
    for i in 1:length(ko)
        d[d.region .== i, :LIBGEO] .= ko[i]
    end
    # compute shares of rural and urban land rent over income
    d.rural_rent = 100 .* d.ρr .* (d.Sr .+ d.Srh) ./ d.GDP
    d.urban_rent = 100 .* d.iq ./ d.GDP

    # create the aggregate/average city
    agg_city = combine(
        groupby(d, :year),
            :Lu => mean => :Lu,
            :Lr => mean => :Lr,
            :ρr => mean => :ρr,
            :pr => mean => :pr,
            :cityarea => mean => :cityarea,
            [:Cr,:Cu,:Ch] => ((x,y,z) -> (rshare = sum(x) / sum((x + y + z)),
                                          ushare = sum(y) / sum((x + y + z)),
                                          hshare = sum(z) / sum((x + y + z)))) => AsTable,
            [:citydensity,:Lu] => ((x,y) -> mean(x)) => :density,
            [:avgd_n,:Lu] => ((x,y) -> mean(x)) => :density_n,
            [:dr_n,:Lu] => ((x,y) -> mean(x)) => :dr_n,
            [:d0_n,:Lu] => ((x,y) -> mean(x)) => :d0_n,
            [:ρr,:Sr,:Srh,:GDP] => ((x,y,z,g) -> 100 * sum(x .* (y .+ z)) / sum(g)) => :rural_rent,
            [:iq,:GDP] => ((r,p) -> 100 * sum(r) / sum(p)) => :urban_rent,
        )

    normalizers = @chain agg_city begin
        subset(:year => ByRow(==(1840)))
        select(:Lu,:cityarea,:density, :ρr)
    end    

    # plotter setup
    idx = subset(d, :it => ieq(1))
    labs = reshape(idx.LIBGEO, 1,K)
    cols = reshape([:darkgreen,:darkgreen,:firebrick,:firebrick], 1,K)
    styles = reshape([:solid,:dot,:solid,:dot], 1,K)
    widths = reshape([3,3,3,3], 1,K)

    def_theme()
    
    pl = Dict()

    # row 1
    # Lr
    pl[:Lr] = plot(agg_city.year, agg_city.Lr, color = :darkgreen, legend = false, size = panelsizef(npanels = 3))

    # spending
    pl[:spending] = @df agg_city plot(:year, [:rshare, :ushare, :hshare], color = [:darkgreen :firebrick :darkblue], linestyle = [:solid :dot :dashdot], legend = :topleft, label = ["Rural Good" "Urban Good" "Housing"],ylims = (0,0.9), size = panelsizef(npanels = 3))

    # food price - only aggregate
    # pl[:pr] = @df agg_city plot(:year, :pr, color = :darkgreen, leg = false, size = panelsizef(npanels = 3))

    # row 2
    # Urban area and population
    pl[:LuArea] = @df agg_city plot(:year, [:Lu ./ normalizers.Lu,
                                            :cityarea ./ normalizers.cityarea], yscale = :log10,yticks = [1,2,10,50,100], yformatter = x -> string(round(Int,x)),
                                            color = [reds()[1] golds()[1]],
                                            linestyle = [:solid :dash],
                                            label = ["Urban population" "Urban area"], size = panelsizef(npanels = 3))

    # Urban densities 
    pl[:aggDensities] = @df agg_city plot(:year, [:density_n ,
                                                  :d0_n ,
                                                  :dr_n ],
                                                  color = [reds()[1] golds()[1] blues()[3]],
                                                  linestyle = [:solid :dash :dashdot], size = panelsizef(npanels = 3),
                                                  label = ["Average" "Central" "Fringe"])
    pl[:aggDensities_log] = @df agg_city plot(:year, [:density_n ,
                                                  :d0_n ,
                                                  :dr_n ],
                                                  color = [reds()[1] golds()[1] blues()[3]],
                                                  linestyle = [:solid :dash :dashdot], size = panelsizef(npanels = 3),
                                                  label = ["Average" "Central" "Fringe"], yscale = :log10, yticks = [0.05,0.1,0.25,1], yformatter = x -> string(round(x,digits=2)))                                                  
    # pl[:aggDensities] = @df agg_city plot(:year, [:density ,
    # :d0 ,
    # :dr],
    # color = [reds()[1] golds()[1] blues()[3]],
    # linestyle = [:solid :dash :dashdot])

     # rural rent and urban rent
    # pl[:landrents] = @df agg_city plot(:year, [:rural_rent :urban_rent],  size = panelsizef(npanels = 3), labels = ["Rural Rents" "Urban Rents"], color = [greens()[3] reds()[1]], yticks = 0:2:18, linestyle = [:solid :dot])
    # plot!(pl[:ruralrents],agg_city.year, agg_city.rural_rent, color = :grey, lw = 3, label = "")

    # third row
    # spreads 
    # pl[:Lu_spread] = @df d plot(:year, :Lu ./ normalizers.Lu, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 3))

    # pl[:cityarea_spread] = @df d plot(:year, :cityarea ./ normalizers.cityarea, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 3),  yscale = :log10,yticks = [1,2,5,30,100], yformatter = x -> string(round(x, digits = 2)))

    # normalize by first obs of aggregated city.
    # and do a second panel with fringe and center
    # average density
    # pl[:density_spread_log] = @df d plot(:year, :citydensity ./ normalizers.density, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 3), yscale = :log10, yticks = [0.01,0.05,0.2,0.5,1], yformatter = x -> string(round(x,digits=2)))

    # pl[:density_spread] = @df d plot(:year, :citydensity ./ normalizers.density, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 3))
    # # add aggregate
    # plot!(pl[:avg_density], agg_city.year, agg_city.density ./ normalizers.density, color = :grey, lw = 3, label = "")

    # # fringe density
    # pl[:fringe_density] = @df d plot(:year, :dr ./ normalizers.dr, group = :region, color = cols, label = labs, linestyle = styles, linewidth = widths, size = panelsizef(npanels = 3), yscale = :log10, yticks = [0.01,0.05,0.2,0.5,1], yformatter = x -> string(round(x,digits=2)))
    # # add aggregate
    # plot!(pl[:fringe_density], agg_city.year, agg_city.dr ./ normalizers.dr, color = :grey, lw = 3, label = "")

    # # central density
    # pl[:central_density] = @df d plot(:year, :d0 ./ normalizers.d0, group = :region, color = cols, label = labs, linestyle = styles, linewidth = widths, size = panelsizef(npanels = 3), yscale = :log10, yticks = [0.01,0.05,0.2,0.5,1], yformatter = x -> string(round(x,digits=2)))
    # # add aggregate
    # plot!(pl[:central_density], agg_city.year, agg_city.d0 ./ normalizers.d0, color = :grey, lw = 3, label = "")

    # # Urban population
    # pl[:Lu] = @df d plot(:year, :Lu ./ normalizers.Lu, group = :region, color = cols, label = labs, linestyle = styles, linewidth = widths, size = panelsizef(npanels = 3))
    # # add aggregate
    # plot!(pl[:Lu], agg_city.year, agg_city.Lu ./ normalizers.Lu, color = :grey, lw = 3, label = "")

   
    # # agg
    # plot!(pl[:cityarea], agg_city.year, agg_city.cityarea ./ normalizers.cityarea, color = :grey, lw = 3, label = "")

    # # rural rent and urban rent
    # pl[:ruralrents] = @df d plot(:year, :rural_rent, group = :region, color = cols, label = labs, linestyle = styles, linewidth = widths, size = panelsizef(npanels = 3))
    # plot!(pl[:ruralrents],agg_city.year, agg_city.rural_rent, color = :grey, lw = 3, label = "")


    # # Rural Population
    # pl[:Lr] = @df d plot(:year, :Lr, group = :region, color = cols, label = labs, linestyle = styles, linewidth = widths, size = panelsizef(npanels = 3))
    # # add aggregate
    # plot!(pl[:Lr], agg_city.year, agg_city.Lr, color = :grey, lw = 3, label = "")

    # # spending shares - only aggregate 
    # pl[:spending] = @df agg_city plot(:year, [:rshare, :ushare, :hshare], color = [:darkgreen :firebrick :darkblue], linestyle = [:solid :dot :dashdot], legend = :topleft, label = ["Rural Good" "Urban Good" "Housing"],ylims = (0,0.9))

    # # food price - only aggregate
    # pl[:pr] = @df agg_city plot(:year, :pr, color = :darkgreen, lw = 3, leg = false)
    pl
end
