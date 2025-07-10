

# color setup

greens() = [:olivedrab, :green3, :darkgreen, :darkseagreen]
reds() = [:firebrick, :red3, :red4, :salmon]
blues() = [:dodgerblue, :blue3, :navy, :aliceblue]
golds() = [:gold, :goldenrod, :goldenrod4, :lemonchiffon]
dcol(n) =  reshape(distinguishable_colors(n),1,n)


# themes

def_theme(; xticks = 1840:20:2020, legfontsize = 11) = theme(:default, linewidth = 3, titlefont = 10,guidefont = 10,tickfontsize = 10, legendfontsize = legfontsize, xticks = xticks)
pres_theme(; xticks = 1840:20:2020, legfontsize = 13) = theme(:default, linewidth = 5, titlefont = 10,guidefont = 10,tickfontsize = 12, legendfontsize = legfontsize, xticks = xticks)

# formats
HPI_fmt(ip) =  ((ip == :HPI_1840) | (ip == :cityarea_agg_1840)) ? x -> string.(round(Int,x)) : x -> string.(round(x,digits = 2))

function symlims(x,y,limpad)
	xr,yr = extrema(x), extrema(y)
	min(xr[1], yr[1]) - (xr[2]-xr[1])*limpad, max(xr[2], yr[2]) + (yr[2]-yr[1])*limpad
end


function agg_gdp_growth(b::DataFrame)
	d = @chain b begin
		aggregator(_)
		subset(:year => ByRow(∈([1840,2020])))
		select(:year, :GDP_agg, :GDP_agg_pc, :mean_ρr_1840)
		sort!(:year)
	end
	Dict(:agg_growth => d.GDP_agg[2] / d.GDP_agg[1],
		 :agg_growth_pc => d.GDP_agg_pc[2] / d.GDP_agg_pc[1],
		 :ρr_growth => d.mean_ρr_1840[2] / d.mean_ρr_1840[1])
end

# ========================
# HETEROGENEOUS THETAR Model Aggregation
# ========================

"""
## Heterogeneous Model Aggregator

The model (outcome of [`khet_estimate()`](@ref)) produces `K` outcomes for each variable `x` - one for each of `K` regions in year `t`. This function
aggregates over regions within a given period `t` to produce an _aggregate city/region_.

## Naming Convention:

1. `0`: center (e.g. `d0` is density in center)
2. `r`: rural part (or at fringe - `dr` is density in rural part/fringe)
3. `x_agg`: variable computed by summation (no averaging) over `x` within a given year
4. `mean_x`: variable computed by averaging (potentially with weights) over `x` within a given year
5. `x_yyyy`: series of `x` but normalized to its value in year `yyyy`

## Main Variable Names

* `GDP_agg`: sum_k (Yu_k + pr * Yr_k) by year
* `itau_agg`: sum_k itau_k by year, iτ = ∫ τ(l) D(l) dl

"""
function aggregator(d::DataFrame)

	@chain d begin
		transform(:imode => :commuting_speed)  # rename

		# create constant Lu weights
		innerjoin(_ , 
				select(@subset(_, :year .== 1870), :Lu => :Lu_1870, :region), on = :region)
		innerjoin(_ , 
		select(@subset(_, :year .== 2020), :Lu => :Lu_2020, :region), on = :region)

		# actual aggregation within each year
		groupby(:year)
		combine(
			[agg_popw(z) for z in popweightvars()],   # aggregation via weighted mean (population weights)
			[agg_luw(z) for z in Luweightvars()],
			[agg_luw(z,constant = 1870) for z in Luconstweightvars()],
			[agg_luw(z,constant = 2020) for z in Luconstweightvars()],
			[agg_lrw(z) for z in Lrweightvars()],
			[agg_srw(z) for z in srweightvars()],
			[(z => (x -> mean(x)) => "mean_$z") for z in meanvars()],

			[:𝕎l, :𝕎lu, :𝕎lr] => ((x,y,z) -> (𝕎lushare = sum(y) / sum(x), 𝕎lrshare = sum(z) / sum(x))) => AsTable,  # urban and rural wealth shares
			[:Yu, :Yr, :pop, :pr] => ((yu,yr,pop,pr) -> sum((pr .* yr) .+ yu) / sum(pop) ) => :GDP_agg_pc,             # GDP per capita
			[:Yu, :Yr, :pr] => ((yu,yr,pr) -> sum((pr .* yr) .+ yu)  ) => :GDP_agg,                                    # GDP
			:θu => sum  => :θu_agg,                                    
			[ix => sum => "$(ix)_agg" for ix in sumvars()]
		)
		
		# now this is one row per year.

		# normalizations variables with respect to its values in a given year (1840, 1870 etc)
		transform([:Lu_agg, :year]            => ((x,y) -> normx_by_y(x, y .== 1870)) => :Lu_agg_1870,
				  [:Lu_agg, :year]            => ((x,y) -> normx_by_y(x, y .== 1840)) => :Lu_agg_1840,
				  [:mean_ρr, :year]           => ((x,y) -> normx_by_y(x, y .== 1840)) => :mean_ρr_1840,
				  [:mean_pr, :year]           => ((x,y) -> normx_by_y(x, y .== 1950)) => :mean_pr_1950,   
				  [:mean_pr, :year]           => ((x,y) -> normx_by_y(x, y .== 1840)) => :mean_pr_1840,   
				  [:mean_ρr_noSrh, :year]     => ((x,y) -> normx_by_y(x, y .== 1840)) => :mean_ρr_noSrh_1840,
		          [:cityarea_agg, :year]      => ((x,y) -> normx_by_y(x, y .== 1870)) => :cityarea_agg_1870,
		          [:cityarea_agg, :year]      => ((x,y) -> normx_by_y(x, y .== 1840)) => :cityarea_agg_1840,
				  [:mean_HPI, :year]          => ((x,y) -> 100 .* normx_by_y(x, y .== 1870)) => :mean_HPI_1870,
				  [:mean_HPI, :year]          => ((x,y) -> 100 .* normx_by_y(x, y .== 1840)) => :mean_HPI_1840,
				  [:mean_HPI_center, :year]   => ((x,y) -> 100 .* normx_by_y(x, y .== 1870)) => :mean_HPI_center_1870,
				  [:mean_HPI_center, :year]   => ((x,y) -> 100 .* normx_by_y(x, y .== 1840)) => :mean_HPI_center_1840,

				  # commuting speed
				  [:mean_commuting_speed, :year] => ((x,y) -> normx_by_y(x, y .== 1840)) => :mean_commuting_speed_1840,

				  # density time normalizations
				  [:mean_avgd_n, :year] => ((x,y) -> normx_by_y(x, y .== 1840)) => :BAD_mean_avgd_n_1840, 
				  [:mean_citydensity, :year] => ((x,y) -> normx_by_y(x, y .== 1840)) => :mean_citydensity_1840,  # TODO sub mean_avgd_n_1840 with mean_citydensity_1840 in latex
				  [:mean_citydensity, :year] => ((x,y) -> normx_by_y(x, y .== 1870)) => :mean_citydensity_1870,
				  [:mean_citydensity_Lu_1870, :year] => ((x,y) -> normx_by_y(x, y .== 1840)) => :mean_citydensity_Lu_1870_1840,  
				  [:mean_citydensity_Lu_1870, :year] => ((x,y) -> normx_by_y(x, y .== 1870)) => :mean_citydensity_Lu_1870_1870,  
				  [:mean_citydensity_Lu_2020, :year] => ((x,y) -> normx_by_y(x, y .== 1870)) => :mean_citydensity_Lu_2020_1870,  
				  [:mean_citydensity_Lu_2020, :year] => ((x,y) -> normx_by_y(x, y .== 1840)) => :mean_citydensity_Lu_2020_1840,  
				  [:mean_d0, :year] => ((x,y) -> normx_by_y(x, y .== 1840)) => :mean_d0_1840,  # TODO sub mean_d0_n with mean_d0_1840 in latex
				  [:mean_dr, :year] => ((x,y) -> normx_by_y(x, y .== 1840)) => :mean_dr_1840,  # TODO sub mean_dr_n with mean_dr_1840 in latex
				 
				  [:GDP_agg, :iτ_agg] => ((x,y) -> x .- y) => :y_disposable_agg,
				  [:Yu_agg, :pop_agg] => ((x,y) -> (x ./ y)) => :Yu_pc,
				  [:Yu_agg, :Lu_agg] => ((x,y) -> (x ./ y)) => :Yu_Lu,
				  [:θu_agg, :Lu_agg] => ((x,y) -> (x ./ y)) => :θu_Lu,
				  

				  [:GDP_agg, :iτ_agg, :pop_agg] => ((x,y,z) -> (x .- y) ./ z) => :y_disposable_pc,
				  [:Lr_agg, :pop_agg] => ((x,y) -> x ./ y) => :Lrshare_agg,
				  [:Lu_agg, :cityarea_agg] => ((x,y) -> x ./ y) => :citydensity_agg,
				  [:cityarea_agg, :Sr_agg, :Srh_agg] => ((x,y,z) -> x ./ (y + z)) => :rel_cityarea,
				  [["$(ix)_agg", "C_agg"] => ((x,y) -> x ./ y) => "$(ix)_share" for ix in [:Ch, :Cr, :Cu]])
			# need to do a new transform to use those new variables further.
			transform(
				[:Yu_pc, :year] => ((x,y) -> normx_by_y(x, y .== 1870)) => :Yu_pc_1870,
				[:Yu_Lu, :year] => ((x,y) -> normx_by_y(x, y .== 1870)) => :Yu_Lu_1870,
				[:θu_Lu, :year] => ((x,y) -> normx_by_y(x, y .== 1870)) => :θu_Lu_1870,
				[:GDP_agg_pc, :year] => ((x,y) -> normx_by_y(x, y .== 1870)) => :GDP_agg_pc_1870,
				[:GDP_agg, :year] => ((x,y) -> normx_by_y(x, y .== 1870)) => :GDP_agg_1870,
				[:Yu_agg, :year] => ((x,y) -> normx_by_y(x, y .== 1870)) => :Yu_agg_1870,
				[:θu_agg, :year] => ((x,y) -> normx_by_y(x, y .== 1870)) => :θu_agg_1870,

				[:mean_ρr, :GDP_agg] => ((x,y) -> x ./ y) => :mean_ρr_GDP,
				[:mean_ρr, :GDP_agg_pc] => ((x,y) -> x ./ y) => :mean_ρr_GDP_pc,
				[:mean_ρr, :y_disposable_agg] => ((x,y) -> x ./ y) => :mean_ρr_y_disposable,
				[:mean_ρr, :y_disposable_pc] => ((x,y) -> x ./ y) => :mean_ρr_y_pc
			)
	end
end


"Variables to aggregate single regions over to recover an *average* region by population weights"
popweightvars() = [:HPI, :APG]

"Variables to be aggregated by Urban Population weights"
Luweightvars() = [:HPI_center, :d0, :commuting_speed, :citydensity, :avgd_n, :dr, :θu]

"Variables to be aggregated by *constant* Urban Population weights"
Luconstweightvars() = ["citydensity"]

"Variables to be aggregated by Rural Population weights"
Lrweightvars() = [:θr]

"Variables weighted by Size or Rural Area"
srweightvars() = [:ρr]

"Variables aggregated by unweighted mean"
meanvars() = [:pr]

"Variables aggregated by summation without weighting"
sumvars() = [:Lu, :cityarea, :Sr , :Srh, :Lr, :pop, :C, :Ch, :Cr, :Cu, :y, :iτ, :Yu]


"computes aggregation by population weighted mean"
function agg_popw(n) 
	([n, :pop] => ((x,y) -> mean(x, Weights(y))) => "mean_$n")
end

"computes aggregation by Lu weighted mean"
function agg_luw(n; constant = nothing) 
	if isnothing(constant)
		([n, :Lu] => ((x,y) -> mean(x, Weights(y)) ) => "mean_$n")
	elseif constant ∈ [1870,2020]
		([n, "Lu_$(constant)"] => ((x,y) -> mean(x, Weights(y)) ) => "mean_$(n)_Lu_$(constant)")
	else
		error("only constant weights in [1870,2020] implemented.")
	end
end

"computes aggregation by Lr weighted mean"
function agg_lrw(n) 
	([n, :Lr] => ((x,y) -> mean(x, Weights(y)) ) => "mean_$n")
end

"computes aggregation by population weighted mean with 1950 pop weights"
function agg_popw1950(n) 
	([n, :popw_1950] => ((x,y) -> mean(x, Weights(y))) => "mean1950_$n")
end

"computes aggregation by rural area weights - with and without Srh as weight"
function agg_srw(n) 
	([n, :Sr,:Srh] => 
		((x,y,z) -> [mean(x, Weights(y .+ z))  mean(x, Weights(y))]) => 
		["mean_$n","mean_$(n)_noSrh"])

end

"computes aggregation by simple mean"
function agg_mean(n) 
	([n] => ((x) -> mean(x)) => "mean_$n")
end

panelsizef(;npanels = 3) = if npanels == 3
	(600,500)
elseif npanels == 2
	(700,500)
end


function checkw(a::DataFrame)
	aa = @chain a begin
		subset(:year => x -> x .< 2030)
		end
	r = @chain aa begin
		select(:year, :mean_dr_n, :dr_agg_1840, :mean_d0_n, :d0_agg_1840)
		end
	dr = @df r plot(:year, :mean_dr_n, label = "popw", title = "avg fringe density")
	@df r plot!(dr,:year, :dr_agg_1840, label = "fringe pop w")

	d0 = @df r plot(:year, :mean_d0_n, label = "popw", title = "avg central density")
	@df r plot!(d0,:year, :d0_agg_1840, label = "central pop w")

	d = select(aa,:year, :citydensity_agg_1840 ,:mean_avgd_1840,  [:citydensity_agg_1840 ,:mean_avgd_1840] => ((x,y) -> abs.(x .- y)) => :absdiff)
	p = @df d plot(:year, :absdiff, title = "sum pop / sum area vs wtd avg density", label = "absdiff")

	rho = @df aa plot(:year, :mean_ρr_1840 , label = "popw", title = "avg rural rent")
	@df aa plot!(rho,:year, :mean_Sρr_1840, label = "rural area w")


	plot(dr,d0,p,rho, layout = (2,2), size = (800,800), titlefontsize = 10, legendfontsize = 8)

end


densyears() = [1870, 1950,1970,1990,2000,2010]
densinterpyears() = [1870, 1950,1975,1990,2000,2015]

"""
interpolate model outcomes to match data years.
Some data years are off the 10 year steps of the model (1975 and 2015),
so we linearly interpolate the model outcomes using the closest ten-year points. Finally, this function only keeps years for which we do have data observations on urban density, i.e. it subsets to [1870, 1950,1975,1990,2000,2015].

>This is for producing **outputs** (plots and tables) only, not for solving model and computing _estimates_ of structural parameters. 

Notice that when estimating and solving the model, we keep the model's time grid fixed (steps of 10 years), and we interpolate the _data_ onto that grid. This is done in [`areaprices_estim`](@ref).
"""
function interp_model_data(d::DataFrame)
	pd = @chain LandUse.popdata() begin
				select(:CODGEO, :LIBGEO,:year, :density_data, :area_data, :pop_data)
				transform(:year => (x -> ifelse.( x.==1876 , 1870 , x)) => :year)
	end
	i = @chain d begin
		subset(:year => leq(2020))
		select(:citydensity, :cityarea, :ρr => :rhor, :θu => :thetau,:θr => :thetar, :ℙ => :pricer, :Lu, :Lr, :year, :LIBGEO, :region, :CODGEO,:ℙ_n, :s_price_data, :pop)
		outerjoin(pd,on = [:LIBGEO,:CODGEO,:year])
		sort([:LIBGEO, :year])
		Impute.interp(_)
		subset(:year => x -> x .∈ Ref(densinterpyears()))
	end
	i
end


function plot_density_ts(d::DataFrame; norm_year = 1870)

	# labels
	sizebin_labels = ["[0,25K]","[25K,50K]","[50K, 100K]","> 100K"]
	sizebins = [0,25_000,50_000,100_000,2_000_000]

	dp = @chain d begin
		subset(:year => ieq(1870))
		transform( :pop_data => (x -> cut(x, sizebins,
		labels = sizebin_labels, extend = false)) => :pop_bin)
		select(:CODGEO,:pop_bin)
		leftjoin(d, on = :CODGEO)
		groupby([:pop_bin, :year])
		combine([:Lu, :cityarea] => ((x,y) -> sum(x) / sum(y)) => :model)
	end
	# return dp
	# levels!(dp.reg_bin, [4,3,2,1])
	normalizers = if isnothing(norm_year)
		mean(dp.model),median(dp.model)
	else
		@chain dp begin
			subset(:year => x -> x .== minimum(x))
			mean(_.model),median(_.model)
		end
	end
	
	# dp = leftjoin(dp, normalizer, on = :reg_bin)
	
	dp.model_mean = log.(dp.model ./ normalizers[1])
	dp.model_median = log.(dp.model ./ normalizers[2])
	# dp.data = log.(dp.data ./ normalizer.data_1)

	cols = [:red :gold :green :blue]
	xticks = 1870:20:2010

	pmod = @df dp plot(:year, :model_mean, group = :pop_bin, color = cols,xticks = xticks, title = "(a) Model")
	pmod_median = @df dp plot(:year, :model_median, group = :pop_bin, color = cols,xticks = xticks, title = "(a) Model")
	# @df dp plot!(pl, :year, :data, group = :reg_bin, linestyle = :dash, label = "", color = [:gold :green :blue :pink :red])

	# make data panel
	da = @chain get_data() begin
		transform(:year => (x -> ifelse.(x .== 1876, 1870, x)) => :year)
	end
	da_bins = @chain da begin
		subset(:year => ieq(1870))
		transform( :pop_data => (x -> cut(x, sizebins,
		labels = sizebin_labels, extend = false)) => :pop_bin)
		select(:CODGEO,:pop_bin)
		leftjoin(da, on = :CODGEO)
		groupby([:pop_bin, :year])
		combine([:pop_data, :area_data] => ((x,y) -> sum(x) / sum(y)) => :avg_density)
	end
	# levels!(da_bins.pop_bin, sizebins)
	# return da_bins
	# normalize data
	avgd_data = if isnothing(norm_year)
		(sum(da.pop_data) / sum(da.area_data) , median(da.pop_data ./ da.area_data))
	else	
		@chain da begin
			subset(:year => x -> x .== minimum(x))
			(sum(_.pop_data) / sum(_.area_data), median(_.pop_data ./ _.area_data))
		end
	end
	da_bins.lavg_density = log.(da_bins.avg_density ./ avgd_data[1])
	da_bins.lavg_density_median = log.(da_bins.avg_density ./ avgd_data[2])

	# make data plot 
	pdata = @df da_bins plot(:year, :lavg_density, group = :pop_bin, color = cols,xticks = xticks, title = "(b) Data")
	pdata_median = @df da_bins plot(:year, :lavg_density_median, group = :pop_bin, color = cols,xticks = xticks, title = "(b) Data")
	
	Dict(:mean => plot(pmod,pdata, size = (800,400), lw = 1.5, link = :y),
	     :median => plot(pmod_median,pdata_median, size = (800,400), lw = 1.5, link = :y))


end

function check_modvsdataslopes_wtd(d::DataFrame,x::Symbol, y::Symbol)

	transform!(d, [x,y] .=> (z -> log.(z)) .=> [x,y])
	transform!(d, [x,y] => ((x,y) -> (x .- mean(x) .+ mean(y))) => x)

	# create population weights as share by year
	transform!(groupby(d, :year),
		[:pop] => (x -> x ./ sum(x)) => :popw)

	# create weights for 1950
	w1950 = @chain d begin
		subset(:year => x -> x .== 1950)
		select(:pop, :region)
		transform(:pop => (x -> x ./ sum(x)) => :popw_1950)
		select(:popw_1950, :region)
	end

	w1870 = @chain d begin
		subset(:year => x -> x .== 1870)
		select(:pop, :region)
		transform(:pop => (x -> x ./ sum(x)) => :popw_1870)
		select(:popw_1870, :region)
	end

	d = leftjoin(d, w1950, on = :region)
	d = leftjoin(d, w1870, on = :region)


	r = lm( Term(y) ~ Term(x), d)
	rw = lm( Term(y) ~ Term(x), d, wts = identity.(d.popw))
	rw1950 = lm( Term(y) ~ Term(x), d, wts = identity.(d.popw_1950))
	rw1870 = lm( Term(y) ~ Term(x), d, wts = identity.(d.popw_1870))

	OrderedDict(:unweighted => OrderedDict(:slope => coef(r)[2],:r2 => r2(r)), 
	            :weighted => OrderedDict(:slope => coef(rw)[2], :r2 => r2(rw)), 
				:weighted_1950 => OrderedDict(:slope => coef(rw1950)[2], :r2 => r2(rw1950)),
				:weighted_1870 => OrderedDict(:slope => coef(rw1870)[2], :r2 => r2(rw1870)))

end

"""
Plots cross sectional and time series implications from heterogenous θr model.
input the output of [`khet_estimate()`](@ref).
`save = true` will save plots to `output/plots/baseline`
"""
function plot_het(d; save = false, ribbons = false, tikz = false, panelsize = panelsizef(), doreg = true)
	# data relieable only here:
    # densyears = [1870, 1950,1970,1990,2010]
    rentyears = [1870, collect(1900:10:2020)...]

	cities = sort(unique(d.LIBGEO))
	K = length(cities)


	d2020 = @chain d begin
		subset(:year => x -> x .<= 2020)
	end 
	d2100 = @chain d begin
		subset(:year => x -> x .<= 2100)
	end 

	ddens = @chain d begin
        subset(:year => x -> in.(x,Ref(densyears())))
	end

	ddens_interpolated = interp_model_data(d)
	# return ddens_interpolated
	
	o = Dict()   # output dict

	# ILLUSTRATE HETEROGENEITY FROM θr
	# ================================

	# backend
	if tikz
		pgfplotsx()
	else
		gr()

	end

	pl = Dict()

	# def_theme()

	theme(:default)

	# cross section + time: density model vs data
	pl[:mod_data_density_xsect_time_allyears] = plot_density_ts(ddens_interpolated,norm_year = nothing)[:mean]
	pl[:mod_data_density_xsect_time] = plot_density_ts(ddens_interpolated)[:mean]
	pl[:mod_data_density_xsect_time_median] = plot_density_ts(ddens_interpolated)[:median]

	theme(:default, linewidth = 2, titlefont = 10,guidefont = 8)


	# cross sections: Model vs Data
	# ==============

    pl[:mod_data_landvalues] = modvsdata(groupby(ddens_interpolated,:year), :ℙ_n, :s_price_data, xlab = L"$\log \mathbb{P}$ (Model)",ylab = L"$\log$ Land Price Data",dolog = true, demean = true)

    pl[:mod_data_Lu] = modvsdata(groupby(ddens_interpolated,:year), :Lu, :pop_data, xlab = L"$\log L_u$ Model",ylab = L"$\log L_u$ Data",dolog = true, demean = true)

	pl[:mod_data_density] = modvsdata(groupby(ddens_interpolated, :year), :citydensity, :density_data, xlab = L"$\log$ Density Model",ylab = L"$\log$ Density Data",dolog = true, demean = true)



    # pl[:mod_data_density_allyears] = modvsdata(ddens_interpolated, :citydensity, :density_data, dolog = true,title = "",xan = 8, yan = 10.2,xlab = L"$\log$ Density Model",ylab = L"$\log$ Density Data", demean = true)

	# pl[:mod_data_density_allyears_color] = modvsdata(ddens_interpolated, :citydensity, :density_data, dolog = true,title = "",xan = 8, yan = 10.2,xlab = L"$\log$ Density Model",ylab = L"$\log$ Density Data",colorby = :LIBGEO, demean = true)

    pl[:mod_data_density_allyears_square] = modvsdata(ddens_interpolated, :citydensity, :density_data, dolog = true,title = "",xan = 8.5, yan = 10.8,xlab = L"$\log$ Density Model",ylab = L"$\log$ Density Data", demean = true, size = panelsizef())

	pl[:mod_data_density_allyears_square_45deg] = modvsdata(ddens_interpolated, :citydensity, :density_data, dolog = true,title = "",xan = 8.5, yan = 10.8,xlab = L"$\log$ Density Model",ylab = L"$\log$ Density Data", demean = true, size = panelsizef(),add45 = true)


	pl[:mod_data_density_allyears_libgeo_square] = modvsdata(ddens_interpolated, :citydensity, :density_data, dolog = true,title = "",xan = 8.5, yan = 10.8,xlab = L"$\log$ Density Model",ylab = L"$\log$ Density Data",colorby = :LIBGEO, demean = true,size = panelsizef(), lims = (7,12.5))
	
	pl[:mod_data_density_allyears_color_square] = modvsdata(ddens_interpolated, :citydensity, :density_data, dolog = true,title = "",xan = 8.5, yan = 10.8,xlab = L"$\log$ Density Model",ylab = L"$\log$ Density Data",colorby = :year, demean = true,size = panelsizef())

	pl[:mod_data_density_allyears_color_square_45deg] = modvsdata(ddens_interpolated, :citydensity, :density_data, dolog = true,title = "",xan = 8.5, yan = 10.8,xlab = L"$\log$ Density Model",ylab = L"$\log$ Density Data",colorby = :year, demean = true,size = panelsizef(),add45 = true)


	pl[:mod_data_area_allyears_square] = modvsdata(ddens_interpolated, :cityarea, :area_data, dolog = true,title = "",xan = 2, yan = 5,xlab = L"$\log$ Area Model",ylab = L"$\log$ Area Data", demean = true, size = panelsizef())

	pl[:mod_data_area_allyears_square_45deg] = modvsdata(ddens_interpolated, :cityarea, :area_data, dolog = true,title = "",xan = 2, yan = 5,xlab = L"$\log$ Area Model",ylab = L"$\log$ Area Data", demean = true, size = panelsizef(),add45 = true)


	pl[:mod_data_area_allyears_square_color] = modvsdata(ddens_interpolated, :cityarea, :area_data, dolog = true,title = "",xan = 2, yan = 5,xlab = L"$\log$ Area Model",ylab = L"$\log$ Area Data", demean = true, size = panelsizef(), lims = (0,6.5), colorby = :year)

	pl[:mod_data_area_allyears_square_color_45deg] = modvsdata(ddens_interpolated, :cityarea, :area_data, dolog = true,title = "",xan = 2, yan = 5,xlab = L"$\log$ Area Model",ylab = L"$\log$ Area Data", demean = true, size = panelsizef(), lims = (0,6.5), colorby = :year,add45 = true)


	pl[:mod_data_pop_allyears_square] = modvsdata(ddens_interpolated, :Lu, :pop_data, dolog = true,title = "",xan = 12, yan = 14,xlab = L"$\log L_u$ Model",ylab = L"$\log$ Pop Data", demean = true, size = panelsizef(), lims = (9.5,16.3))

	pl[:mod_data_pop_allyears_square_45deg] = modvsdata(ddens_interpolated, :Lu, :pop_data, dolog = true,title = "",xan = 12, yan = 14,xlab = L"$\log L_u$ Model",ylab = L"$\log$ Pop Data", demean = true, size = panelsizef(), lims = (9.5,16.3),add45 = true)

	
	pl[:mod_data_pop_allyears_square_color] = modvsdata(ddens_interpolated, :Lu, :pop_data, dolog = true,title = "",xan = 12, yan = 14,xlab = L"$\log L_u$ Model",ylab = L"$\log$ Pop Data", demean = true, size = panelsizef(), lims = (9.5,16.3), colorby = :year)

	pl[:mod_data_pop_allyears_square_color_45deg] = modvsdata(ddens_interpolated, :Lu, :pop_data, dolog = true,title = "",xan = 12, yan = 14,xlab = L"$\log L_u$ Model",ylab = L"$\log$ Pop Data", demean = true, size = panelsizef(), lims = (9.5,16.3), colorby = :year,add45 = true)

	# pl[:mod_data_density_allyears] = modvsdata(ddens, :citydensity, :density_data, dolog = true,title = "",xan = 2.5, yan = 11.2,xlab = L"$\log$ Density Model",ylab = L"$\log$ Density Data")

	# pl[:model_density_pop_allyears] = modvsdata(ddens_interpolated, :citydensity, :pop, dolog = true,title = "",xan = 2.5, yan = 2,xlab = L"$\log$ Density Model",ylab = L"$\log$ Pop Model", demean = true)

	pl[:model_density_Lu_allyears] = modvsdata(ddens_interpolated, :citydensity, :Lu, dolog = true,title = "",xan = 2.5, yan = 2,xlab = L"$\log$ Density Model",ylab = L"$\log L_u$ Model", demean = true)


	# pl[:mod_data_area] = modvsdata(groupby(ddens_interpolated, :year), :cityarea, :area_data, dolog = true,xlab = L"$\log$ Area Model",ylab = L"$\log$ Area Data",demean = true)

	theme(:default, linewidth = 2, titlefont = 10,guidefont = 8)

	# within model output
	# ===================

	pl[:model_rents_values] = modvsdata(groupby(ddens,:year),:ρr, :ℙ, xlab = L"\log \rho_r",ylab = L"\log \mathbb{P}", dolog = true)

	pl[:model_values_density] = modvsdata(groupby(ddens,:year),:ℙ,:citydensity, ylab = "log density",xlab = L"\log \mathbb{P}", dolog = true)
	pl[:model_values_density_allyears] = modvsdata(ddens,:ℙ,:citydensity, ylab = "log density",xlab = L"\log \mathbb{P}", dolog = true)

	pl[:model_Lu_θu] = @chain ddens begin
		transform([:θu, :Lu] .=> (x -> log.(x)) .=> [:θu, :Lu] )
		groupby(:year)
	end
	pl[:model_Lu_θu] = modvsdata(pl[:model_Lu_θu], :θu, :Lu,xlab = L"\log \theta_u",ylab = L"\log L_u")

	pl[:model_area_θu] = @chain ddens begin
		transform([:θu, :cityarea] .=> (x -> log.(x)) .=> [:θu, :cityarea] )
		groupby(:year)
	end
	pl[:model_area_θu] = modvsdata(pl[:model_area_θu], :θu, :cityarea,xlab = L"\log \theta_u",ylab = L"$\log$ area", dolog = false)

	pl[:model_area_θr] = modvsdata(groupby(ddens,:year),  :θr, :cityarea, dolog = true,xlab = L"\log \theta_r",ylab = L"$\log$ area")
	# plot_5_het(ddens, :θr, :cityarea, :year, xlab = L"\log \theta_r",ylab = L"$\log$ area", trans = log)

	pl[:model_area_pop] = modvsdata(groupby(ddens,:year),  :pop, :cityarea, dolog = true,xlab = L"$\log$ Pop",ylab = L"$\log$ area")

	pl[:model_area_Lu] = modvsdata(groupby(ddens,:year),  :Lu, :cityarea, dolog = true,xlab = L"$\log$ Lu ",ylab = L"$\log$ area")

	pl[:model_θr_θu] = modvsdata(groupby(ddens,:year),  :θr, :θu, dolog = true,xlab = L"$\log \theta_r$",ylab = L"$\log \theta_u$")

	pl[:model_θr_rents] = modvsdata(groupby(ddens,:year),  :θr, :ρr	, dolog = true,xlab = L"$\log \theta_r$",ylab = L"$\log \rho_r$")

	pl[:model_dens_Lu] = modvsdata(groupby(ddens,:year),  :Lu, :citydensity, dolog = true,xlab = L"\log L_u",ylab = L"$\log$ density", demean = true)
	
	pl[:model_dens_pop] = modvsdata(groupby(ddens,:year),  :pop, :citydensity, dolog = true,xlab = L"$\log$ Population",ylab = L"$\log$ density", demean = true)
	
	pl[:model_Sr_θr] = modvsdata(groupby(ddens,:year),  :Sr, :θr, dolog = true,xlab = L"\log \theta_r",ylab = L"\log S_r")
	
	tmp = @chain ddens begin
		transform([:Lr, :pop] => ((x,y) -> x ./ y) => :Lr_pop,[:θr, :θu] => ((x,y) -> x ./ y) => :θrθu)
		transform([:Lr_pop, :θrθu] .=> (x -> log.(x)) .=> [:Lr_pop, :θrθu] )
	end
	pl[:model_Lr_θrθu] = modvsdata(groupby(tmp, :year),  :θrθu,:Lr_pop, xlab = L"\log \left(\theta_r / \theta_u \right)",ylab = L"\log \left( L_r/L \right)", dolog = false)	

	pl[:model_price_θr] = modvsdata(groupby(ddens,:year),  :ℙ, :θr, dolog = true,xlab = L"\log \theta_r",ylab = L"\log \mathbb{P}")

	# time series
	pl[:model_rprice_ts] = @chain d2020 begin
		transform([:ℙ, :θr] .=> (x -> log.(x)) .=> [:ℙ, :θr] )
		@df plot( :year,:ℙ, group = :region, leg = :outerright,ylab = L"\log \mathbb{P}")	
	end 
	pl[:model_rrents_ts] = @chain d2020 begin
		transform([:ρr, :θr] .=> (x -> log.(x)) .=> [:ρr, :θr] )
		@df plot( :year,:ρr, group = :region, leg = :outerright,ylab = L"\log \rho_r")	
	end 



	tt = t1 = @chain d2100 begin
		transform([:θu, :θr] .=> (x -> log.(x)) .=> [:θu, :θr] )
	end
	t1 = @df tt plot(:year, :θr, group = :region, leg = false, ylab = L"\log \theta_r")
	t2 = @df tt plot(:year, :θu, group = :region, leg = :outerright, ylab = L"\log \theta_u")

	pl[:model_θ_ts] = plot(t1,t2, size = (800,400), link = :y,  left_margin = 0.5Plots.cm, layout = @layout [a{0.45w} b{0.55w}])

	z = change_θ_growth(d2020, Param(),newrg = 1.0)
	cs = reshape(distinguishable_colors(K) , 1,K)
	ls = reshape([:solid for i in 1:K] , 1,K)
	ls[cities .== "Paris"] .= :dot
	lw_ = reshape([1 for i in 1:K] , 1,K)
	lw_[cities .== "Paris"] .= 2

	t1 = @df z plot(:year, :θkt_r, group = :LIBGEO, leg = false, ylab = L"\theta_{rkt}",color = cs, lw = 2,linestyle = ls)
	t2 = @df z plot(:year, :θkt_u, group = :LIBGEO, leg = :outerright, ylab = L"\theta_{ukt}",color = cs, lw = 2,linestyle = ls)

	pl[:model_θkt_ts] = plot(t1,t2, size = (800,300), link = :y,  left_margin = 0.5Plots.cm, layout = @layout [a{0.4w} b{0.6w}])

	# @df d2020 plot(:year, :θu, group = :region, yscale = :log10, ylab = L"\log \theta_u", legend = :outerright)
	# pl[:θu_ts] = @df d2020 plot(:year, :θu, group = :region, yscale = :log10, ylab = L"\log \theta_u", legend = :outerright)
	# pl[:θr_ts] = @df d2020 plot(:year, :θr, group = :region, yscale = :log10, ylab = L"\log \theta_u", legend = :outerright)


	# HPI
	pl[:model_HPI2] = @df d2020 plot(:year, :HPI , group = :CODGEO, title = "deflated HPI")

	# APG
	pl[:model_APG] = @df d2020 plot(:year, :APG , group = :CODGEO, title = "APG", leg = false, label = "", color = cs, xticks = 1840:20:2020)
	@chain d2020 begin
		aggregator(_)
		@df _ plot!(pl[:model_APG],:year, :mean_APG, color = :red, lw = 3, leg = :topleft, label = "France", linestyle = :dash)
	end

	pl[:model_APG_Lu] = modvsdata(groupby(ddens, :year), :Lu, :APG, dolog = true, xlab = L"\log L_u", ylab = L"\log APG")

	pl[:model_HPI_Lu] = modvsdata(groupby(ddens, :year), :Lu, :HPI, dolog = true, xlab = L"\log L_u", ylab = L"\log HPI")
	pl[:model_HPI_center_Lu] = modvsdata(groupby(ddens, :year), :Lu, :HPI_center, dolog = true, xlab = L"\log L_u", ylab = L"\log HPI")

	pl[:model_commuting2] = @chain d2020 begin
		sort([:region,:year])
		groupby(:LIBGEO)
		transform(:imode => firstnorm => :commuting_speed)
		@df plot(:year , :commuting_speed, group = :LIBGEO, color = dcol(20
		), leg = :outerright, linestyles = ls, linewidth = lw_)
	end

	pl[:model_Lrshare2] = @chain begin d
		subset(:year => x -> x .<= 2020)
		@df plot(:year, :Lr ./ :pop ,group = :CODGEO, leg = :outerright , ylab = L"L_{r,k} / L_k")
	end

	pl[:model_hspending2] = @chain begin d
		subset(:year => x -> x .<= 2020)
		@df plot(:year, :Ch ./ :C ,group = :region, leg = :outerright)
	end

	pl[:model_rspending2] = @chain begin d
		subset(:year => x -> x .<= 2020, :region => ByRow(∈(
			@chain d begin
				subset(:year => ByRow(==(1840)))
				sort(:θu,rev = true)
				_.region[[1,2,5,20]]
			end
			)))
		@df plot(:year, :Cr ./ :C ,group = :region, leg = :topright, xticks = 1840:20:2020)
	end

    if save 
		mkpath(joinpath(dbplots(), "revision$(revision())","baseline"))
		for (k,v) in pl
			savefig(v, joinpath(dbplots(), "revision$(revision())","baseline","$(k)." * (tikz ? "tikz" : "pdf")))	
		end
    end

	o[:het] = pl 

	if doreg
		# regressions for heterogeneity
		dr = @chain d begin
			select(:citydensity, :ρr => :rhor, :θu => :thetau,:θr => :thetar, :ℙ => :pricer, :Lu, :Lr, :year => categorical => :year)
			subset(:pricer => x -> isfinite.(x), :year => x -> in.(x, Ref(densyears())))
		end
		dreg(yr) = @chain d begin
			select(:citydensity, :ρr => :rhor, :θu => :thetau,:θr => :thetar, :ℙ => :pricer, :Lu, :Lr, :year, :LIBGEO, :region)
			subset(:pricer => x -> isfinite.(x), :year => x -> x .== yr)
			transform( :year => categorical => :year)
		end
		d1950 = @chain d begin
			select(:citydensity, :ρr => :rhor, :θu => :thetau,:θr => :thetar, :ℙ => :pricer, :Lu, :Lr, :year, :LIBGEO, :region)
			subset(:pricer => x -> isfinite.(x), :year => x -> in.(x, Ref(densyears())) .& (x .>= 1950))
			transform( :year => categorical => :year)
		end
		d1975 = @chain ddens_interpolated begin
			subset(:pricer => x -> isfinite.(x), :year => x -> x .>= 1975)
			transform( :year => categorical => :year)
			leftjoin(CODGEO_REG(popdata()), on = :CODGEO)
		end
		trans_dict = Dict(
			"log(rhor)" => "\$\\log(\\rho_r)\$", 
			"log(pricer)" => "\$\\log(\\overline{\rho}_{r,k,t})\$", 
			"log(thetau)" => "\$\\log(\\theta_u)\$", 
			"log(thetar)" => "\$\\log(\\theta_r)\$", 
			"log(Lu)" => "\$\\log(L_u)\$",
			"log(Lr)" => "\$\\log(L_r)\$")
		r1 = regs(dr)

		if save 
			mkpath(dbtables())
			regtable(r1..., renderSettings = latexOutput(joinpath(dbtables(), "hetr-density-price.tex")), transform_labels = trans_dict, print_estimator_section = false) 
		end
		r1950 = regs(d1950)
		if save 
			regtable(r1950..., renderSettings = latexOutput(joinpath(dbtables(), "hetr-density-price-1950s.tex")), transform_labels = trans_dict, print_estimator_section = false) 
		end
		r1975 = regs(d1975)
		CSV.write(joinpath(dbtables(), "hetr-density-price-1975s.csv"), d1975)

		# lm(@formula(log(citydensity) ~ log(pricer) + year), d1975 )

		if save 
			regtable(r1975..., renderSettings = latexOutput(joinpath(dbtables(), "hetr-density-price-1975s.tex")), transform_labels = trans_dict, print_estimator_section = false) 
		end
		for it in densyears()
			r = regs(dreg(it),year = false)
			if save 
				regtable(r..., renderSettings = latexOutput(joinpath(dbtables(), "hetr-density-price-$it.tex")), transform_labels = trans_dict, print_estimator_section = false) 
			end
		end
	end



	# AGGREGATION TO AVERAGE CITY
	# ===========================

    o[:agg] = plot_het_agg(d, ribbons = ribbons, panelsize = panelsize)

	if save 
		aggpth = joinpath(dbplots(), "revision$(revision())","baseline","aggregation")
		mkpath(aggpth)
		for (k,v) in o[:agg]
			savefig(v, joinpath(aggpth,"$(k)." * (tikz ? "tikz" : "pdf")))
		end
    end

	o

end

"""
average city radius in data in km

This number comes out of the `LandUseR` package:

```R
LandUseR:::exp_decay_linear()
```

"""
mean_city_radius() = 21.447

"exponential decay coeff in data"
exp_coef_data() = 0.15

"""
get exponential curve exponents weighted by urban population

* get urban pop weighted avg of radius in 2020: ϕbar
* rescale model distance units with respect to that target
* estimate decay model on each rescaled region and compute urban pop-weighted (and raw) average of coefficients.
"""
function exp_gradients(d::DataFrame; year = 2020)

	dy = subset(d, :year => x -> x .== year)
	sort!(dy, :region)

	# compute phibar
	ϕbar = mean(dy.ϕ, Weights(dy.Lu))

	# each region has a different x-axis.
	xaxis = dy.ϕmids
	yaxis = dy.iDensities_n
	yaxis2 = dy.iDensities
	nb = length(xaxis[1])

	adjust = mean_city_radius() / ϕbar

	coefs = Float64[]
	coefs2 = Float64[]
	intercepts = Float64[]
	intercepts2 = Float64[]
	mses = Float64[]
	pl = plot(xlab = "km", legend = :outerright, legendfontsize = 10, xticks = :auto)
	pl2 = plot(xlab = "km", legend = :outerright, legendfontsize = 10, xticks = :auto)
	cols = dcol(20)
	for i in 1:length(xaxis)  # num of regions
		xaxis[i] = xaxis[i] .* adjust
		g,e = expmodel(xaxis[i], yaxis[i])
		g2,e2 = expmodel(xaxis[i], yaxis2[i])
		push!(coefs, g[2])
		push!(coefs2, g2[2])
		push!(intercepts, g[1])
		push!(intercepts2, g2[1])
		push!(mses, LsqFit.mse(e))
		scatter!(pl, xaxis[i], yaxis[i],color = cols[i], label = dy.LIBGEO[i], markersize = 3)
		plot!(pl, x -> intercepts[i] .* exp.(coefs[i] * x), xaxis[i], color = cols[i], lw = 2, label = "")
		scatter!(pl2, xaxis[i], yaxis2[i],color = cols[i], label = dy.LIBGEO[i], markersize = 3)
		plot!(pl2, x -> intercepts2[i] .* exp.(coefs2[i] * x), xaxis[i], color = cols[i], lw = 2, label = "")
	end
	
	MSE = round(1000 * mean(mses,Weights(dy.Lu)),digits = 3)
	intercept = mean(intercepts, Weights(dy.Lu))
	intercept2 = mean(intercepts2, Weights(dy.Lu))
	gradient = mean(coefs, Weights(dy.Lu))
	gradient2 = mean(coefs2, Weights(dy.Lu))
	rawgradient = mean(coefs)

	# predict mean
	predicted = map(x -> 1.0 .* exp.(gradient * (x-1)),1:nb)

	m = hcat(dy.iDensities_n...)
	mins = minimum(m, dims = 2)
	maxs = maximum(m, dims = 2)

	# savefig(pl, joinpath(dbplots(),"revision$(revision())","model_density_decay.pdf"))

	Dict(:gradient => gradient,:intercept => intercept, :intercept2 => intercept2, :gradient2 => gradient2, :MSE => MSE, :plot => pl, :plot2 => pl2, :raw => rawgradient)
end

function plot_het_agg(d::DataFrame;ribbons = false, panelsize = panelsizef(),datashape = :star7, datastyle = :dashdot, dataalpha = 0.5, datawidth = 0.5)
	# colors
	brg = ["blue" "red" "green"]

	# aggregation
	sa, gdp = @chain d begin
		subset(:year => x -> x .<= 2020)
		(aggregator(_), GDPdeflator(_))
	end

	
	# long aggregation
	lsa, lgdp = @chain d begin
		subset(:year => x -> x .<= 2100)
		(aggregator(_), GDPdeflator(_))
	end

	# high frequency raw data 
	rawd = highfreq_rawdata()

	dd = Dict()  # output

	# dummy param for moments
	p = Param()

	# theme(:default, linewidth = 3, guidefontsize = 12, legendfontsize = 10)

	def_theme()


	dd[:model_deflator] = @df gdp plot(:year, :p_indexall, leg = false, color = :red )

	dd[:model_Rt] = @chain d begin
		subset(:region => ieq(1)) 
		@df plot(:year, :Rt, color = :red)
	end

	# spending shares
	dd[:model_spending] = @df sa plot(:year, :Ch_share, lab = "Housing",c = brg[1], size = panelsize, leg = :top, xticks = 1840:20:2020)
	@df sa plot!(dd[:model_spending], :year, :Cu_share, lab = "Urban Good",c = brg[2])
	@df sa plot!(dd[:model_spending], :year, :Cr_share, lab = "Rural Good",c = brg[3])
	# add data 
	scatter!(dd[:model_spending], [1900, 2010], [0.237, 0.306], markershape = datashape, c = brg[1], lab = "Data",markersize = 7)

	# rural employment share
	dd[:mod_data_Lrshare] = @df sa plot(:year, :Lrshare_agg, label = "Model", color = :green, leg = :topright,size = panelsize, fillalpha = 0.2, ylims = (0,0.8), xticks = 1840:20:2020)
	# dd[:mod_data_Lrshare] = @df sa plot(:year, :mean_Lrshare, label = "Model", ribbon = ribbons ? (:mean_Lrshare .- :min_Lrshare,:max_Lrshare .- :mean_Lrshare) : false, color = :green, leg = :topright,size = panelsize, fillalpha = 0.2, ylims = (0,0.8), xticks = 1840:20:2020)
	# overplot with high freq raw data
	@df rawd plot!(dd[:mod_data_Lrshare], :year, :Employment_rural, label = "Data", color = "black", linestyle = datastyle, linewidth = 1, linealpha = 1)



	
	# Urban density 
	sa1870 = subset(sa, :year => x -> x .>= 1870)
	dd[:mod_data_avgdensity] = @df sa1870 plot(:year,:mean_citydensity_1870, label = "Model", color = :red, leg = :topright,size = panelsize, yscale = :log10,yticks = [0.15, 0.25, 0.5, 1.0], ylims = (0.1,1.2), yformatter = x -> string.(round(x,digits = 2)), fillalpha = 0.2, xticks = 1880:20:2020)
	
	# get aggregate data on density 
	dagg = agg_pop_density()
	dagg[1, :year] = 1870
	@df dagg plot!(dd[:mod_data_avgdensity], :year, :agg_density, label = "Data", linecolor = "black",m = (datashape, 6, :black), linestyle = datastyle, linewidth = datawidth, linealpha = dataalpha)

	# Urban population and area
	popcol = :red
	dd[:mod_data_urban_pop_area] = @df sa1870 plot(:year,:Lu_agg_1870, label = "Urban Pop (Model)", color = popcol, leg = :topright,size = panelsize, yscale = :log10,yformatter = x -> string.(Int(round(x,digits = 0))), yticks = [1,3,10,30],legend = :topleft, fillalpha = 0.2, xticks = 1880:20:2020)
	# dd[:mod_data_urban_pop_area] = @df sa1870 plot(:year,:mean_Lu_1870, label = "Urban Pop (Model)", ribbon = ribbons ? (:mean_Lu_1870 .- :min_Lu_1870,:max_Lu_1870 .- :mean_Lu_1870) : false, color = popcol, leg = :topright,size = panelsize, yscale = :log10,yformatter = x -> string.(Int(round(x,digits = 0))), yticks = [1,3,10,30],legend = :topleft, fillalpha = 0.2, xticks = 1880:20:2020)

	# area
	areacol = :orange
	@df sa1870 plot!(dd[:mod_data_urban_pop_area],:year,:cityarea_agg_1870, label = "Urban Area (Model)", color = areacol, fillalpha = 0.2 )
	# @df sa1870 plot!(dd[:mod_data_urban_pop_area],:year,:mean_cityarea_1870, label = "Urban Area (Model)", ribbon = ribbons ? (:mean_cityarea_1870 .- :min_cityarea_1870,:max_cityarea_1870 .- :mean_cityarea_1870) : false, color = areacol, fillalpha = 0.2 )

	# overplot with data
	@df dagg plot!(dd[:mod_data_urban_pop_area], :year, :agg_pop, markercolor = popcol, markershape = datashape, markersize = 5, label = "Urban Pop (Data)",linestyle = datastyle, linewidth = datawidth, linecolor = :black, linealpha = dataalpha)
	@df dagg plot!(dd[:mod_data_urban_pop_area], :year, :agg_area, markercolor = areacol, markershape = datashape, markersize = 5, label = "Urban Area  (Data)", linecolor = :black, linewidth = 0.5,linestyle = datastyle, linealpha = 0.5)

	# relative price
	dd[:mod_data_pr] = @df sa plot(:year, :mean_pr_1950, label = "Model",color = :green,size = panelsize, leg = :topright, ylims = (0.1, 1.6), yticks = 0.2:0.2:1.6, xticks = 1840:20:2020)
	# overplot with data
	@df rawd plot!(dd[:mod_data_pr], :year, :P_rural , label = "Data", color = "black",linewidth = 1, linestyle = datastyle)

	# relative price with multiple measures
	dd[:mod_data_pr_all_prices] = @df sa plot(:year, :mean_pr_1950, label = "Model",color = :green,size = panelsize, leg = :topright, ylims = (0.1, 1.6), yticks = 0.2:0.2:1.6, xticks = 1840:20:2020)
	# overplot with data
	@df rawd plot!(dd[:mod_data_pr_all_prices], :year, :P_rural , label = "Data", color = "black",linewidth = 1, linestyle = datastyle)
	@df rawd plot!(dd[:mod_data_pr_all_prices], :year, :P_rural_Sauvy, label = "Sauvy", color = "red",linewidth = 1, linestyle = datastyle)
	@df rawd plot!(dd[:mod_data_pr_all_prices], :year, :P_rural_Toutain, label = "Toutain", color = "blue",linewidth = 1, linestyle = datastyle)


	# HPI
	dd[:model_HPI] = @df sa plot(:year, :mean_HPI_1840, label = "", color = :red, leg = false,size = panelsize, fillalpha = 0.2, xticks = 1840:20:2020,ylims = (90,Inf))

	dd[:model_HPI_center] = @df sa plot(:year, :mean_HPI_center_1840, label = "", color = :red, leg = false,size = panelsize, fillalpha = 0.2, xticks = 1840:20:2020)

	# exponential density decay in 2015
	expgradient = exp_gradients(d)
	def_theme(xticks = :auto)
	dd[:mod_data_expdecay] = plot(x -> 1.0 * exp(expgradient[:gradient] * x), 0, mean_city_radius(), color = :red, label = "Model", xlabel = "km", size = panelsize)
	plot!(dd[:mod_data_expdecay], x -> 1.0 * exp((-1) * exp_coef_data() * x), 0, mean_city_radius(), color = :black, label = "Data", linestyle = :dash)
	dd[:mod_expdecay1] = expgradient[:plot]
	dd[:mod_expdecay2] = expgradient[:plot2]
	# dd[:mod_data_expdecay] = @df exp_gradients(d)[1] scatter(:bin, :mean_exp, label = "Model",markersize = 5,size = panelsize, markercolor = :red, markershape = :circle)
	# plot!(dd[:mod_data_expdecay],1:p.int_bins, x -> 1.0 .* exp.(-0.16 * (x-1)), label = "Fitted Curve (Data)", linestyle = :dash, color = :black, linewidth = 2)

	def_theme()

	# urban density in center, average and Fringe
	
	dd[:model_density_0_r] = @df sa plot(:year, :mean_d0_1840, label = "City Center",
	color = :gold, linestyle = :dash, yscale = :log10,yticks = [0.1, 0.25, 0.5, 1.0], yformatter = x -> string.(round(x,digits = 2)),size = panelsize, xticks = 1840:20:2020)

	@df sa plot!(dd[:model_density_0_r], :year, :mean_citydensity_1840, label = "Average", color = :red)

	@df sa plot!(dd[:model_density_0_r], :year, :mean_dr_1840, label = "Fringe", color = :darkmagenta, linestyle = :dashdot)

	# average commuting time 
	dd[:mod_data_commute_speed] = @df sa plot(:year, :mean_commuting_speed_1840, color = :red, label = "Model", leg = :topleft, yticks = 1:0.5:5,size = panelsize, xticks = 1840:20:2020)
	# overplot with data 
	@chain rawd begin
		dropmissing(:Comm_speed_Paris)
		select(:year, :Comm_speed_Paris => firstnorm => :comm)
		@df plot!(dd[:mod_data_commute_speed], :year, :comm, linestyle = datastyle, linewidth = datawidth, linecolor = :black, linealpha = dataalpha,markershape = datashape, markercolor = :black, markersize = 5 , label = "Estimates for Paris")
	end


	# piketty plot 
	dd[:mod_data_piketty] = @df sa plot(:year, 100 .* :𝕎lushare, color = :red,label = "Urban land value (% of total land value)", leg = :topleft,size = panelsize,xticks = 1840:20:2020,ylims = (0,125))
	@df sa plot!(dd[:mod_data_piketty], :year,100 .* :𝕎lrshare, color = :green , label = "Rural land value (% of total land value)",
	yticks = 0:20:120)

	# get piketty data 
	piketty = get_piketty_shares(rawd)
	# overplot
	@df piketty scatter!(dd[:mod_data_piketty], :year, :ushare, markershape = datashape, label = "Data",markercolor = :black, markersize = 5 )
	@df piketty plot!(dd[:mod_data_piketty], :year, :ushare, linecolor = :red, linewidth = datawidth, linestyle = datastyle,label = "")
	@df piketty plot!(dd[:mod_data_piketty], :year, :rshare, linecolor = :green, markershape = datashape, linewidth = datawidth,markercolor = :black, label = "", linestyle = datastyle,markersize = 5 )
	
	# APG
	dd[:mod_APG_agg] = @df sa plot(:year, :mean_APG, leg = false, color = :red, size = panelsize, xticks = 1840:20:2020)

	# urban rents over income
	# dd[:mod_urbanrents_income] = @df sa plot(:year, :mean_ρy_1840,leg = false, color = :red, size = panelsize, xticks = 1840:20:2020)

	# rural rents over income
	# dd[:mod_ruralrents_income] = @df sa plot(:year, :mean_ρr_y,leg = false, color = :green, size = panelsize, xticks = 1840:20:2020)

	# rural rents 
	# dd[:mod_ruralrents] = @df sa plot(:year, :mean_ρr,leg = false, color = :green, size = panelsize, xticks = 1840:20:2020)

	# aggregate income over DGP
	dd[:mod_agg_income_gdp] = plot(sa.year, sa.y_agg ./ sa.GDP_agg,leg = false, color = :red, size = panelsize, xticks = 1840:20:2020)

	# HPI over aggregate urban productivity
	dd[:mod_HPI1840_thetau] = @df sa plot(:year, :mean_HPI_1840 ./ p.θut, leg = false, xticks = 1840:20:2010)

	# Implied Aggregate Urban Productivity
	# aggregating over each city, urban population weighted θu
	dd[:mod_agg_θu] = @df sa plot(:year, :mean_θu, leg = false,yticks = 1:2:15, color = :red)

	dd

end

"""
Computes aggregate Rural and Urban Wealth shares. We make an assumption on the 
land share in housing wealth, which was 0.32 over the period 1979-2019. Before this date, we don't have data and therefore retropolate. We have the total value of housing wealth 𝕎h in Piketty's data, hence urban land value is 0.32 times 𝕎h.
"""
function get_piketty_shares(d::DataFrame)
	ss = 1.0  # this is done in the input data already now
	@chain d begin
		dropmissing(:HousingLand_inc)
		transform([:HousingLand_inc, :AgriLand_inc] => 
			ByRow((x,y) -> (ushare = 100 * x * ss / (x * ss + y),
			                rshare = 100 * y / (x * ss + y))) => AsTable )
		select(:year, :ushare, :rshare)
	end
end

function regs(dr::DataFrame; year = true)
	r = Any[]
	if year
		push!(r,lm(@formula(log(citydensity) ~ log(pricer) + year), dr ))
		push!(r,lm(@formula(log(citydensity) ~ log(pricer) + log(thetau) + year), dr ))
		push!(r,lm(@formula(log(citydensity) ~ log(pricer) + log(Lu) + year), dr ))
		push!(r,lm(@formula(log(citydensity) ~ log(rhor) + log(thetau) + year), dr ))
		push!(r,lm(@formula(log(citydensity) ~ log(rhor) + log(Lu) + year), dr ))
	else
		push!(r,lm(@formula(log(citydensity) ~ log(pricer) ), dr ))
		push!(r,lm(@formula(log(citydensity) ~ log(pricer) + log(thetau) ), dr ))
		push!(r,lm(@formula(log(citydensity) ~ log(pricer) + log(Lu)), dr ))
		push!(r,lm(@formula(log(citydensity) ~ log(rhor) + log(thetau) ), dr ))
		push!(r,lm(@formula(log(citydensity) ~ log(rhor) + log(Lu) ), dr ))
	end
	r
end



"""
	dashboard(d::DataFrame)

Dashboard for a [`Country`](@ref).
"""
function dashboard(d::DataFrame;aggregated = true, par = Dict())

	pl = Any[]

	ps = plot_het(d)

	if aggregated
		for (k,v) in filter(x -> x.first ∉	[:model_Lrshare2 ,:model_deflator ,:model_Rt,:model_Lrshare2], ps[:agg])
			push!(pl,v)
		end
		plot(pl..., size = (1800,1000))

	else
		push!(pl, ps[:het][:model_θ_ts])

		push!(pl, plot(ps[:agg][:model_Lrshare2], lw = 1, leg = false))
		push!(pl, plot(plot_areapricedata(par = par), lw = 1,leg = false, title = "Land Prices from Data"))

		lr = @df subset(d, :year => leq(2020)) plot(:year, :Lr, group = :region, ylab = "Lr", color = reshape(StatsPlots.distinguishable_colors(20), 1,20), leg = false)
   		sr = @df subset(d, :year => leq(2020)) plot(:year, :Sr, group = :region, ylab = "Sr", color = reshape(StatsPlots.distinguishable_colors(20), 1,20), leg = false)

    	push!(pl,lr,sr)
		la = @layout [a; b c; d e]
		plot(pl..., layout = la, lw = 1, size = (800,800))


	end


end


"""
makes 2 scatter plots, one each for θuk and θrk, i.e. the regional component.
comparing both across baseline and counterfactual (cf) scenarios.

Uses the baseline aggregate θu and θr series. So, not to be used for counterfactuals
which modify that series! like [`sensitivity_θr`](@ref) for instance.
"""
function plot_diagnostic_thetas(b::DataFrame,cf::DataFrame,which::Symbol)
	# get aggregate back
	p0 = Param()  # loads safely from disk.
	θagg = DataFrame(year = collect(p0.T), θuagg = p0.θut, θragg = p0.θrt)

	# merge both 
	d0 = @chain b begin
		subset(:year => leq(2020))
		select(:year, :region, :θu, :θr)
		leftjoin(θagg, on = :year)
		leftjoin(select(cf,:year, :region, :θu => :θu_counterfactual,:θr => :θr_counterfactual),  on = [:year, :region])
		transform(
                  [:θu, :θuagg] => ((x,y) -> x ./ y) => :θu_netofagg,
                  [:θr, :θragg] => ((x,y) -> x ./ y) => :θr_netofagg,
                  [:θu_counterfactual, :θuagg] => ((x,y) -> x ./ y) => :θu_counterfactual_netofagg,
                  [:θr_counterfactual, :θragg] => ((x,y) -> x ./ y) => :θr_counterfactual_netofagg
        )
        sort([:year, :region])
	end

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