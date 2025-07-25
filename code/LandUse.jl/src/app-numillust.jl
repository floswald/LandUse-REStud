# this file contains functions which produce figure X in appendix

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
function param_4_cities(; g = 1.2)

    # baseline param 
    K = 4
    p = Param(par = Dict(:T => 1840:10:2020, :gsr => zeros(K),:gs => zeros(K),:kshare => [1/K for i in 1:K],:factors => ones(K),:K => K,:𝕊 => zeros(60,K), :Lflat => true,
    :cbar => 0.7,
    :sbar => 0.0,
    :a => 3.0,
    :γ => 0.3,
    :ν => 0.025,
    :ϵflat => true,
    :ϵr => 4.0,
    :ξl => 0.8,
    :ξw => 0.8
    ))

    # set growth to g
    constant_growth_θs!(p, g, g)

    return p
end




# appendix figure with numerical illustration
# 9 panels
# add separate one for population spread
# one for land rent spread under suitable normalization
# one for density spread for prediction
function app_numillustration(; overwrite = false, save = false,tryfor = 10)
    pth = joinpath(dbplots(), "revision$(revision())","appendix-numillust")
    mkpath(pth)

    offs = OrderedDict("low θr" => 0.95,"high θr" => 1.05,"low θu" => 0.95,"high θu" => 1.05)

    if overwrite
    
        # 5 city param
        p4 = param_4_cities()
    
        # get artificial θ series 
        arti = artificialθ(p4,offs)
    
        # get a starting value
        x0 = nothing
        for i in 1:tryfor
            try
                x0 = start_4cities(p4)
            catch e
                if i == tryfor
                    println("tried for $tryfor steps with reducing p4.a , then failed")
                    @warn "app_numillustration did not find valid starting value - no results produced"
                    return 0
                end
                # next try
                p4.a -= 0.01
            end
        end

        # run model 
        d = app_numillustration_impl_(p4,x0,arti)

        # save
        JLD2.jldsave(joinpath(pth,"data.jld2"); d)
    else
        d = JLD2.load_object(joinpath(pth,"data.jld2"))
    end
    # return d
    # make desired plot panels, each with 5 lines (one for each city)
    # return d
    f = app_numillustration_plot(d[1],offs)

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
function app_numillustration_plot(d::DataFrame,offs::OrderedDict; highsbar = false, lwpres = 5)

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
    pl[:pr] = @df agg_city plot(:year, :pr, color = :darkgreen, leg = false, size = panelsizef(npanels = 3))

   

    # row 2
    # Urban area and population
    pl[:LuArea] = @df agg_city plot(:year, [:Lu ./ normalizers.Lu,
                                            :cityarea ./ normalizers.cityarea], yscale = :log10,yticks = highsbar ? [1,2,4] : [1,2,10,50,100], yformatter = x -> string(round(Int,x)),
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
                                                  
     # rural rent and urban rent
    pl[:landrents] = @df agg_city plot(:year, [:rural_rent :urban_rent],  size = panelsizef(npanels = 3), labels = ["Rural Rents" "Urban Rents"], color = [greens()[3] reds()[1]], yticks = 0:2:18, linestyle = [:solid :dot])
    # plot!(pl[:ruralrents],agg_city.year, agg_city.rural_rent, color = :grey, lw = 3, label = "")

    # third row
    # spreads 
    pl[:Lu_spread] = @df d plot(:year, :Lu ./ normalizers.Lu, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 3))
    
    pl[:Lr_spread] = @df d plot(:year, (:Lr ./ (:Lr .+ :Lu)), group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 3))

    pl[:cityarea_spread] = @df d plot(:year, :cityarea ./ normalizers.cityarea, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 3),  yscale = :log10,yticks = [1,2,5,30,100], yformatter = x -> string(round(x, digits = 2)))

    # normalize by first obs of aggregated city.
    # and do a second panel with fringe and center
    # average density
    pl[:density_spread_log] = @df d plot(:year, :citydensity ./ normalizers.density, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 3), yscale = :log10, yticks = [0.01,0.05,0.2,0.5,1], yformatter = x -> string(round(x,digits=2)))

    pl[:density_spread] = @df d plot(:year, :citydensity ./ normalizers.density, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 3))

    pl[:ρr_spread] = @df d plot(:year, :ρr ./ normalizers.ρr, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 3))
    
    
    # presentation versions

    pres_theme()

    pl[:Lr_rect] = plot(agg_city.year, agg_city.Lr, color = :darkgreen, legend = false, size = panelsizef(npanels = 2))

    pl[:spending_rect] = @df agg_city plot(:year, [:rshare, :ushare, :hshare], color = [:darkgreen :firebrick :darkblue], linestyle = [:solid :dot :dashdot], legend = :topleft, label = ["Rural Good" "Urban Good" "Housing"],ylims = (0,0.9), size = panelsizef(npanels = 2))

    pl[:pr_rect] = @df agg_city plot(:year, :pr, color = :darkgreen, leg = false, size = panelsizef(npanels = 2))                                            
    pl[:LuArea_rect] = @df agg_city plot(:year, [:Lu ./ normalizers.Lu,
                                            :cityarea ./ normalizers.cityarea], yscale = :log10,yticks = highsbar ? [1,2,4] : [1,2,10,50,100], yformatter = x -> string(round(Int,x)),
                                            color = [reds()[1] golds()[1]],
                                            linestyle = [:solid :dash],
                                            label = ["Urban population" "Urban area"], size = panelsizef(npanels = 2)) 
    pl[:aggDensities_rect] = @df agg_city plot(:year, [:density_n ,
                                                  :d0_n ,
                                                  :dr_n ],
                                                  color = [reds()[1] golds()[1] blues()[3]],
                                                  linestyle = [:solid :dash :dashdot], size = panelsizef(npanels = 2),
                                                  label = ["Average" "Central" "Fringe"])         
    

    pl[:aggDensities_log_rect] = @df agg_city plot(:year, [:density_n ,
    :d0_n ,
    :dr_n ],
    color = [reds()[1] golds()[1] blues()[3]],
    linestyle = [:solid :dash :dashdot], size = panelsizef(npanels = 2),
    label = ["Average" "Central" "Fringe"], yscale = :log10, yticks = [0.05,0.1,0.25,1], yformatter = x -> string(round(x,digits=2)))                                                
    
    pl[:landrents_rect] = @df agg_city plot(:year, [:rural_rent :urban_rent],  size = panelsizef(npanels = 2), labels = ["Rural Rents" "Urban Rents"], color = [greens()[3] reds()[1]], yticks = 0:2:18, linestyle = [:solid :dot])


    # identification argument

    cols = reshape([:blue,:blue,:red,:red], 1,K)


    pl[:Lu_spread_rect] = @df d plot(:year, :Lu ./ normalizers.Lu, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 2))

    pl[:Lr_spread_rect] = @df d plot(:year, (:Lr ./ (:Lr .+ :Lu)), group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 2))


    pl[:cityarea_spread_rect] = @df d plot(:year, :cityarea ./ normalizers.cityarea, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 2),  yscale = :log10,yticks = [1,2,5,30,100], yformatter = x -> string(round(x, digits = 2)))

    pl[:density_spread_log_rect] = @df d plot(:year, :citydensity ./ normalizers.density, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 2), yscale = :log10, yticks = [0.01,0.05,0.2,0.5,1], yformatter = x -> string(round(x,digits=2)))

    pl[:density_spread_rect] = @df d plot(:year, :citydensity ./ normalizers.density, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 2))

    pl[:ρr_spread_rect] = @df d plot(:year, :ρr ./ normalizers.ρr, group = :region, color = cols, label = labs, linestyle = styles, size = panelsizef(npanels = 2))



    pl
end

function start_4cities(p::Param)
    # start a single city
    setperiod!(p,1)
	x0 = nearstart(p)
	m = Region(p)
	x0 = jm(p,x0, estimateθ = false)
	update!(m,p,x0)

    (Lr = [m.Lr for i in 1:p.K],
		r = m.r,
		pr = m.pr,
		Sr = [m.Sr for i in 1:p.K],
		ϕ = [m.ϕ for i in 1:p.K],
		Lu = [m.Lu for i in 1:p.K], 
		ρr = [m.ρr for i in 1:p.K]
	)

end

function app_numillustration_impl_(p::Param,x0::NamedTuple, arti::DataFrame; with_estim_data = false)
	sols = NamedTuple[]
	
	push!(sols, x0)
    C = HetCountry[]
    edatas = DataFrame[]

    is_ρr_given = "ρr_given" ∈ names(arti)

    ϕs = zeros(p.K)

	for it in 1:length(p.T)
		setperiod!(p,it)
        # println("Year: $(p.T[it])")
		c = HetCountry(p) 
        edata = areaprice_estim(p)

		
        # overwrite region specific productivities
        for ik in 1:p.K
            idx = (arti.it .== it) .&  (arti.region .== ik)
            c.pp[ik].θr = arti.θr[idx][1]
            c.pp[ik].θu = arti.θu[idx][1]
        end

        if is_ρr_given
            ρr_given = arti[arti.it .== it, "ρr_given"]
            # if it > 1
                λSr = arti[arti.it .== it, "λSr"]
            # else
            #     λSr = nothing
            # end
        else
            ρr_given = nothing
            λSr = nothing
        end

        xx = jc(c,sols[end], estimateθr = false, estimateθu = false, ρr_given = ρr_given,λSr = λSr)
        if xx isa JuMP.Model
            return (C,xx)
        end

        push!(sols, xx[1])    
        ϕs[:] = xx[2]  # store phis

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
        push!(edatas,edata)

	end
    if with_estim_data
        post_proc(C,edatas)
    else 
        dataframe(C), sols
    end
	
end