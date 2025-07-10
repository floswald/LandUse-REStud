
# choose sbar so that housing spending share is flat (and density decreases less as a result)

function start_bs_sbar(; sbar = 0.3,nsteps= 20)

        # baseline param 
        K = 20
        p = Param(par = Dict(:T => 1840:10:2020, :gsr => zeros(K),:gs => zeros(K),:kshare => [1/K for i in 1:K],:factors => ones(K),:K => K,:𝕊 => zeros(60,K)))
        
    # start a single city
    setperiod!(p,1)
	x0 = nearstart(p)
	m = Region(p)
	x0 = jm(p,x0, estimateθ = false)

    # step-wise increase sbar now
    s0 = p.sbar

    sbars = range(s0,stop = sbar,length = nsteps)
    cbars = range(p.cbar, stop = 0.6, length = nsteps)
    gammas = range(p.γ, stop = 0.2, length = nsteps)
    for istep in 1:nsteps
    
        p.γ = gammas[istep]
        p.sbar = sbars[istep]
        p.cbar = cbars[istep]
        @info "sbar = $(p.sbar),cbar = $(p.cbar), γ = $(p.γ)"
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



function sensi_flat_hshare(; overwrite = false, save = false, sbar = 0.3)
    pth = joinpath(dbplots(), "revision$(revision())","sensitivity-flat-hshare")
    apth = joinpath(dbplots(), "revision$(revision())","sensitivity-flat-hshare","aggregation")
    mkpath(apth)
    mkpath(pth)

    if overwrite
    
        # get a starting values
        out = start_bs_sbar(sbar = sbar)  
 
        # get baseline θ series
        arti = khet_run(readdisk = true)
        # run model 
        d = app_numillustration_impl_(out.p,out.x0,arti, with_estim_data = true)

        # save
        JLD2.jldsave(joinpath(pth,"data.jld2"); d)
    else
        d = JLD2.load_object(joinpath(pth,"data.jld2"))
    end

    # make intial plot for setup of sbar and spendign share
    b_share = @chain khet_run(readdisk = true) begin
        groupby(:year)
        combine([ix => sum => "$(ix)_agg" for ix in sumvars()])
        transform([["$(ix)_agg", "C_agg"] => ((x,y) -> x ./ y) => "$(ix)_share" for ix in [:Ch, :Cr, :Cu]])
        select(:Ch_share => :Ch_share_baseline, :year)
    end

    x = @chain d begin
        groupby(:year)
        combine([ix => sum => "$(ix)_agg" for ix in sumvars()])
        transform([["$(ix)_agg", "C_agg"] => ((x,y) -> x ./ y) => "$(ix)_share" for ix in [:Ch, :Cr, :Cu]])
        select(:Ch_share, :year)
        innerjoin(b_share, on = :year)
        plot(_.year, [_.Ch_share_baseline, _.Ch_share], labels = ["Baseline" "High sbar"], title = "Housing spending share")
    end

    plts = plot_het(d)
    plts[:spendingshares] = x


    if save
        savefig(plts[:spendingshares], joinpath(pth,"spending-share-sbar.pdf"))
        for (k,v) in plts[:het]
            savefig(v, joinpath(pth,"$k.pdf"))
        end
        for (k,v) in plts[:agg]
            savefig(v, joinpath(apth,"$k.pdf"))
        end
    end
    plts

end

# function app_numillustration_sbar_plot(d::DataFrame,offs::OrderedDict)

#     K = length(unique(d.region))
    
#     d[!,:LIBGEO] .= ""
#     ko = collect(keys(offs))
#     for i in 1:length(ko)
#         d[d.region .== i, :LIBGEO] .= ko[i]
#     end
#     # compute shares of rural and urban land rent over income
#     d.rural_rent = 100 .* d.ρr .* (d.Sr .+ d.Srh) ./ d.GDP
#     d.urban_rent = 100 .* d.iq ./ d.GDP

#     # create the aggregate/average city
#     agg_city = combine(
#         groupby(d, :year),
#             :Lu => mean => :Lu,
#             :Lr => mean => :Lr,
#             :ρr => mean => :ρr,
#             :pr => mean => :pr,
#             :cityarea => mean => :cityarea,
#             [:Cr,:Cu,:Ch] => ((x,y,z) -> (rshare = sum(x) / sum((x + y + z)),
#                                           ushare = sum(y) / sum((x + y + z)),
#                                           hshare = sum(z) / sum((x + y + z)))) => AsTable,
#             [:citydensity,:Lu] => ((x,y) -> mean(x)) => :density,
#             [:avgd_n,:Lu] => ((x,y) -> mean(x)) => :density_n,
#             [:dr_n,:Lu] => ((x,y) -> mean(x)) => :dr_n,
#             [:d0_n,:Lu] => ((x,y) -> mean(x)) => :d0_n,
#             [:ρr,:Sr,:Srh,:GDP] => ((x,y,z,g) -> 100 * sum(x .* (y .+ z)) / sum(g)) => :rural_rent,
#             [:iq,:GDP] => ((r,p) -> 100 * sum(r) / sum(p)) => :urban_rent,
#         )

#     normalizers = @chain agg_city begin
#         subset(:year => ByRow(==(1840)))
#         select(:Lu,:cityarea,:density, :ρr)
#     end    

#     # plotter setup
#     idx = subset(d, :it => ieq(1))
#     labs = reshape(idx.LIBGEO, 1,K)
#     cols = reshape([:darkgreen,:darkgreen,:firebrick,:firebrick], 1,K)
#     styles = reshape([:solid,:dot,:solid,:dot], 1,K)
#     widths = reshape([3,3,3,3], 1,K)

#     def_theme()
    
#     pl = Dict()

#     # row 1
#     # Lr
#     pl[:Lr] = plot(agg_city.year, agg_city.Lr, color = :darkgreen, legend = false, size = panelsizef(npanels = 3))

#     # spending
#     pl[:spending] = @df agg_city plot(:year, [:rshare, :ushare, :hshare], color = [:darkgreen :firebrick :darkblue], linestyle = [:solid :dot :dashdot], legend = :topleft, label = ["Rural Good" "Urban Good" "Housing"],ylims = (0,0.9), size = panelsizef(npanels = 3))

#     # food price - only aggregate
#     # pl[:pr] = @df agg_city plot(:year, :pr, color = :darkgreen, leg = false, size = panelsizef(npanels = 3))

#     # row 2
#     # Urban area and population
#     pl[:LuArea] = @df agg_city plot(:year, [:Lu ./ normalizers.Lu,
#                                             :cityarea ./ normalizers.cityarea], yscale = :log10,yticks = [1,2,10,50,100], yformatter = x -> string(round(Int,x)),
#                                             color = [reds()[1] golds()[1]],
#                                             linestyle = [:solid :dash],
#                                             label = ["Urban population" "Urban area"], size = panelsizef(npanels = 3))

#     # Urban densities 
#     pl[:aggDensities] = @df agg_city plot(:year, [:density_n ,
#                                                   :d0_n ,
#                                                   :dr_n ],
#                                                   color = [reds()[1] golds()[1] blues()[3]],
#                                                   linestyle = [:solid :dash :dashdot], size = panelsizef(npanels = 3),
#                                                   label = ["Average" "Central" "Fringe"])
#     pl[:aggDensities_log] = @df agg_city plot(:year, [:density_n ,
#                                                   :d0_n ,
#                                                   :dr_n ],
#                                                   color = [reds()[1] golds()[1] blues()[3]],
#                                                   linestyle = [:solid :dash :dashdot], size = panelsizef(npanels = 3),
#                                                   label = ["Average" "Central" "Fringe"], yscale = :log10, yticks = [0.05,0.1,0.25,1], yformatter = x -> string(round(x,digits=2)))                                                  
#     # pl[:aggDensities] = @df agg_city plot(:year, [:density ,
#     # :d0 ,
#     # :dr],
#     # color = [reds()[1] golds()[1] blues()[3]],
#     # linestyle = [:solid :dash :dashdot])

#      # rural rent and urban rent
#     # pl[:landrents] = @df agg_city plot(:year, [:rural_rent :urban_rent],  size = panelsizef(npanels = 3), labels = ["Rural Rents" "Urban Rents"], color = [greens()[3] reds()[1]], yticks = 0:2:18, linestyle = [:solid :dot])
#     # plot!(pl[:ruralrents],agg_city.year, agg_city.rural_rent, color = :grey, lw = 3, label = "")

#     # third row
#     # spreads 
#     # pl[:Lu_spread] = @df d plot(:year, :Lu ./ normalizers.Lu, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 3))

#     # pl[:cityarea_spread] = @df d plot(:year, :cityarea ./ normalizers.cityarea, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 3),  yscale = :log10,yticks = [1,2,5,30,100], yformatter = x -> string(round(x, digits = 2)))

#     # normalize by first obs of aggregated city.
#     # and do a second panel with fringe and center
#     # average density
#     # pl[:density_spread_log] = @df d plot(:year, :citydensity ./ normalizers.density, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 3), yscale = :log10, yticks = [0.01,0.05,0.2,0.5,1], yformatter = x -> string(round(x,digits=2)))

#     # pl[:density_spread] = @df d plot(:year, :citydensity ./ normalizers.density, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 3))
#     # # add aggregate
#     # plot!(pl[:avg_density], agg_city.year, agg_city.density ./ normalizers.density, color = :grey, lw = 3, label = "")

#     # # fringe density
#     # pl[:fringe_density] = @df d plot(:year, :dr ./ normalizers.dr, group = :region, color = cols, label = labs, linestyle = styles, linewidth = widths, size = panelsizef(npanels = 3), yscale = :log10, yticks = [0.01,0.05,0.2,0.5,1], yformatter = x -> string(round(x,digits=2)))
#     # # add aggregate
#     # plot!(pl[:fringe_density], agg_city.year, agg_city.dr ./ normalizers.dr, color = :grey, lw = 3, label = "")

#     # # central density
#     # pl[:central_density] = @df d plot(:year, :d0 ./ normalizers.d0, group = :region, color = cols, label = labs, linestyle = styles, linewidth = widths, size = panelsizef(npanels = 3), yscale = :log10, yticks = [0.01,0.05,0.2,0.5,1], yformatter = x -> string(round(x,digits=2)))
#     # # add aggregate
#     # plot!(pl[:central_density], agg_city.year, agg_city.d0 ./ normalizers.d0, color = :grey, lw = 3, label = "")

#     # # Urban population
#     # pl[:Lu] = @df d plot(:year, :Lu ./ normalizers.Lu, group = :region, color = cols, label = labs, linestyle = styles, linewidth = widths, size = panelsizef(npanels = 3))
#     # # add aggregate
#     # plot!(pl[:Lu], agg_city.year, agg_city.Lu ./ normalizers.Lu, color = :grey, lw = 3, label = "")

   
#     # # agg
#     # plot!(pl[:cityarea], agg_city.year, agg_city.cityarea ./ normalizers.cityarea, color = :grey, lw = 3, label = "")

#     # # rural rent and urban rent
#     # pl[:ruralrents] = @df d plot(:year, :rural_rent, group = :region, color = cols, label = labs, linestyle = styles, linewidth = widths, size = panelsizef(npanels = 3))
#     # plot!(pl[:ruralrents],agg_city.year, agg_city.rural_rent, color = :grey, lw = 3, label = "")


#     # # Rural Population
#     # pl[:Lr] = @df d plot(:year, :Lr, group = :region, color = cols, label = labs, linestyle = styles, linewidth = widths, size = panelsizef(npanels = 3))
#     # # add aggregate
#     # plot!(pl[:Lr], agg_city.year, agg_city.Lr, color = :grey, lw = 3, label = "")

#     # # spending shares - only aggregate 
#     # pl[:spending] = @df agg_city plot(:year, [:rshare, :ushare, :hshare], color = [:darkgreen :firebrick :darkblue], linestyle = [:solid :dot :dashdot], legend = :topleft, label = ["Rural Good" "Urban Good" "Housing"],ylims = (0,0.9))

#     # # food price - only aggregate
#     # pl[:pr] = @df agg_city plot(:year, :pr, color = :darkgreen, lw = 3, leg = false)
#     pl
# end
