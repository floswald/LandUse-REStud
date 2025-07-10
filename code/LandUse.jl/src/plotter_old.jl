function setup_color(l::Int)
    # setup the colors
    grad   = ColorSchemes.inferno
    colors = [grad[k] for k in range(0,stop=1,length=l)]
    # colors = repeat(colors, 1, length(colors))
    return colors
end

"plot densities for all years"
function plot_dens_years(M::Vector{Region},p::Param)
	x = plot(M[1].ϕmids, M[1].iDensities,leg = false)
	for i in 2:length(M)
		plot!(x, M[i].ϕmids, M[i].iDensities)
	end
	x
end

"""
	dashboard(M::Vector{Region},p::Param,i::Int; objvalue = nothing)

Model dashboard for a single [`Region`](@ref).
"""
function dashboard(M::Vector{Region},p::Param,i::Int; objvalue = nothing)
	pl = LandUse.ts_plots(M,p,fixy = false)
	pc = LandUse.cs_plots(M[i], p, i, objvalue = objvalue)
	po = plot(pl[:Lr_data],pl[:spending],pl[:pr_data],pl[:productivity],
			pl[:n_densities], pl[:densities], pl[:mode], pl[:ctime],
			pl[:phi] , plot(pl[:phi_Lu_data], title = "",ylab = ""), pl[:r_y], pl[:r_rho],
			pc[:ϵ] , pc[:D], pc[:mode] , pl[:speedshares],
			layout = (4,4),size = (1200,800))
	po
end




function modvsdata(d::GroupedDataFrame, x::Symbol, y::Symbol; title = nothing,xlab = "", ylab = "",dolog = false,demean = false, size = (800,600))
	# theme(:default, guidefontsize = guidefont, titlefontsize = 10)

	ptmp = Any[]
	k = keys(d)
	for ik in k
		# make one panel for each group
		push!(ptmp, modvsdata(d[ik],x,y, title = "$(values(ik)[1])", antitle = true, xlab = xlab, ylab = ylab, dolog = dolog, demean = demean))
	end
	p = plot(ptmp..., layout = length(k), size = size)	
	# theme(:default)
	p

end

function linearmap(xv::Vector,yv::Vector)
	ey = extrema(yv)
	ex = extrema(xv)
	ey[1] .+ diff([ey...])./diff([ex...]) .* (xv .- ex[1])
end

function modvsdata(d0::Union{DataFrame,SubDataFrame}, x::Symbol, y::Symbol; title = nothing, xan = 11, yan = 10,dolog = false,ansize = 10, antitle = false,xlab = "", ylab = "",colorby = nothing, demean = true, size = (600,400), lims = nothing,limpad = 0.02, add45 = false)

	d = copy(d0)

	if dolog
		d = transform(d, [x,y] .=> (z -> log.(z)) .=> [x,y])
	end
	if demean
		# does NOT demean but map x axis into y axis 
		# d = transform(d, [x,y] .=> (z -> z .- mean(z)) .=> [x,y])
		transform!(d, [x,y] => ((x,y) -> (x .- mean(x) .+ mean(y))) => x)

		# d = transform(d, [x,y] => ((x,y) -> linearmap(x,y)) => x)
	end

	if isnothing(lims)
		xr,yr = extrema(d[!,x]), extrema(d[!,y])
		lims = min(xr[1], yr[1]) - (xr[2]-xr[1])*limpad, max(xr[2], yr[2]) + (yr[2]-yr[1])*limpad
	end

	if antitle
		r = lm( Term(y) ~ Term(x), d)
		mse = deviance(r)/dof_residual(r)
		p = scatter(d[!,x], d[!,y], leg = false,xlab = xlab, 
		ylab = ylab, title = string(title,". slope=$(round(coef(r)[2],digits = 2))"), size = size, lims = lims, widen = true)
		plot!(p, x -> coef(r)[1] + coef(r)[2] * x, lab = "", linewidth = 2, color = "black")
		
	else
		if colorby == :LIBGEO
			which_cities = ["Paris","Lyon","Vienne","Besançon"]
			yoff = [-0.2,-0.2,-0.38,0.6]
			ylineoff = [-0.2,-0.2,-0.05,0.05]
			xoff = [0,0.2,0.2,0.0]

			wc = DataFrame(LIBGEO = which_cities, xoff = xoff, yoff = yoff,ylineoff = ylineoff)
			# color only year 2000

			c = Dict(zip(which_cities,[blues()[1] ,reds()[1] ,greens()[1], golds()[1]]))
			cc = @chain d begin
				subset(:year => ieq(2000))
				select(x,y,:LIBGEO,
					:LIBGEO => (x -> ifelse.(x .∈ Ref(which_cities), :red, :grey)) => :citycolor,
					# :LIBGEO => (x -> ifelse.(x .∈ Ref(which_cities), string(x," (2000)"), "")) => :textlabs
				)
			end

			those_cities = subset(cc, :LIBGEO => (x -> x .∈ Ref(which_cities)))
			those_cities = leftjoin(those_cities, wc, on = :LIBGEO)
			rename!(those_cities, Dict(x => :x, y => :y))

			which_cities_ix = indexin(which_cities, cc.LIBGEO)
			# println(those_cities)
			# transform!(d, :region => :textlabs)
			# transform!(d , [:LIBGEO,:year] => ((x,y) -> string.(x," (",y,")")) => :LIBGEO)
			# textlabs = combine(
			# 	groupby(d, :year), 
			# 		# [:LIBGEO,:year] => ((y,z) -> ifelse.(z .== 2000,y,"")) => :textlabs
			# 		[:LIBGEO,:year] => ((y,z) -> ifelse.((y .∈ Ref(which_cities)) .& (z .== 2000),y,"")) => :textlabs
			# # 		)
			# dy = diff([extrema(d[!,y])...])[1] / 20  
			# dy = zeros(nrow(cc))
			# dx = zeros(nrow(cc))
			# dy[which_cities_ix[1]] = -0.2
			# dy[which_cities_ix[2]] = -0.2
			# dy[which_cities_ix[3]] = 0.32
			# dy[which_cities_ix[4]] = 0.3

			# # dx[which_cities_ix[1]] = 0.25
			# # dx[which_cities_ix[2]] = 0.25
			# dx[which_cities_ix[3]] = 0.1
			# dx[which_cities_ix[4]] = 0.15


			p = @df subset(d , :year => neq(2000)) scatter(cols(x), cols(y), xlab = xlab, 
			ylab = ylab, title = title, color = :grey ,leg = false, size = size, lims = lims)

			# year 2000
			@df subset(d , :year => ieq(2000)) scatter!(p,cols(x), cols(y), color = cc.citycolor)


			# c1 = distinguishable_colors(length(which_cities) + 1)
	 		# c[which_cities] = c1[1:(end-1)]
			# c[Not(which_cities)] .= c1[end]
			# # println(c)
			# c = reshape(c,1,20)
			# p = @df d scatter(cols(x), cols(y), xlab = xlab, 
			# ylab = ylab, title = title, color = c[:region] ,leg = :false, size = size, lims = lims)
			for r in eachrow(those_cities)
				annotate!(p, r.x + r.xoff, r.y + r.yoff, Plots.text(string(r.LIBGEO," (2000)"),:red,7))
			end
			tc = subset(those_cities, :LIBGEO => x -> x .∈ Ref(["Vienne","Besançon"]))
			# println(tc)
			for r in eachrow(tc)
				plot!(p, [r.x,r.x + r.xoff], [r.y , r.y + r.yoff*0.8], color = :red,lw = 1)
			end
			# [annotate!(p,x+dx,y+dy,Plots.text(city,:red,7) ) for (x,dx,y,dy,city) in 
			#       zip(1:nrow(those_cities),those_cities[!,x],those_cities[!,y])] 

			# make lines connecting far away cities to dots
			# per = subset(those_cities, :LIBGEO => ieq("Périgueux"))
			# plot!(p, [per.x,per.y], [per.x + per.xoff,per.y + per.yoff],scale)
			# @df subset(those_cities, :LIBGEO => ieq("Périgueux")) plot!(p, [:x,:y], [:x+:xoff,:y+:yoff], color = :red)
			# @df subset(those_cities, :LIBGEO => ieq("Besançon")) plot!(p, [:x,:y], [:x+:xoff,:y+:yoff], color = :red)

		elseif colorby == :year
			uy = unique(d.year)
			ny = length(uy)
			colors = [:grey for i in 1:ny]
			labels = reshape(["" for i in 1:ny], 1, ny)
			# colors[1:2] = [:pink, :gold3]
			# colors[(ny-3):ny] = [:green, :blue, :yellow, :red]
			colors[1] = :red
			colors[ny] = :blue
			labels[1] = "$(uy[1])"
			labels[ny] = "$(uy[ny])"
			colors = reshape(colors, 1, ny)
			p = @df d scatter(cols(x), cols(y), xlab = xlab, 
			ylab = ylab, title = title, leg = :bottomright, size = size, lims = lims, group = :year, color = colors, label = labels)

		else
			# xr,yr = extrema(d[!,x]), extrema(d[!,y])
			# lims = min(xr[1], yr[1]), max(xr[2], yr[2])

			p = @df d scatter(cols(x), cols(y), xlab = xlab, 
			ylab = ylab, title = title, leg = false, size = size, lims = lims, color = :grey)

		end
		

		r = lm( Term(y) ~ Term(x), d)
		mse = deviance(r)/dof_residual(r)
		pred = GLM.predict(r, d, interval = :confidence, level = 0.95)

		plot!(p, x -> coef(r)[1] + coef(r)[2] * x, lab = "", linewidth = 2, color = "black")
		annotate!(p, [(xan, yan, Plots.text("slope=$(round(coef(r)[2],digits = 3)), R2=$(round(r2(r),digits = 2))", ansize))])

		if add45
			plot!(p, x -> x, color = :black, linestyle = :dash, lw = 1, label = "")
		end
	end
	p
end

"""
	plotk20(;save = false)

Produce model output for 20 city version with homogeneous θr.
"""
function plotk20(;save = false)

	# compare model to data
	# =====================
	# get a single city to compare
	x,M,p = runm()
	d1 = dataframe(M,p)

	# output dict
	di = Dict()
	di[:base] = Dict()
	di[:d0d1] = Dict()

	ddd = k20_dfexport()
	K = maximum(ddd[1].region)

	ddd[1][!,:scenario] .= "baseline"
	ddd[2][!,:scenario] .= "d1-d2"
	dd = vcat(ddd[1],ddd[2])
	d = ddd[1]
	d2 = ddd[2]

	# make x sectional plots data vs model
	# ====================================

	# 1. Compare E[20 cities] with average city: urban denity, relative price and rural employment
	# 2. model vs data: urban population, area, density
	# 3. model vs data: density baseline and density d0d1
	# 4. model vs data in single year 2010: density baseline and density d0d1

	densyears = [1870, 1950,1970,1990,2010]

	dy = subset(d, :year => x -> x .∈ Ref(densyears))
	d2y = subset(d2, :year => x -> x .∈ Ref(densyears))

	meandata = mean(dy.density_data)
	meanmodel = mean(dy.citydensity)
	disallowmissing!(dy)
	disallowmissing!(d2y)

	# transformations:
	# simple log
	# zscores
	# zscores

	transform!(dy, :citydensity => (x -> x .* (meandata / meanmodel)) => :ncitydensity,
				   :citydensity => (x -> log.(x .* (meandata / meanmodel))) => :nlcitydensity,
				   :citydensity => (x -> log.(x)) => :lcitydensity,
				   :citydensity => (x -> zscore(log.(x))) => :lcitydensity_z,
				   :density_data => (x -> zscore(log.(x))) => :ldensity_data_z,
				   :density_data => (x -> log.(x)) => :ldensity_data,
				   :cityarea => zscore => :cityarea_z,
				   :cityarea => (x -> log.(x)) => :lcityarea,
	               :area_data => (x -> log.(x)) => :larea_data,
	               :area_data => zscore => :area_data_z,
				   :Lu => zscore => :Lu_z,
				   :Lu => (x -> log.(x)) => :lLu,
				   :pop_data => zscore => :pop_data_z,
				   :pop_data => (x -> log.(x)) => :lpop_data)

	meandata = mean(d2y.density_data)
	meanmodel = mean(d2y.citydensity)
	transform!(d2y, :citydensity => (x -> x .* (meandata / meanmodel)) => :ncitydensity,
	               :citydensity => (x -> log.(x .* (meandata / meanmodel))) => :nlcitydensity,
				   :citydensity => (x -> log.(x)) => :lcitydensity,
				   :citydensity => (x -> zscore(log.(x))) => :lcitydensity_z,
				   :density_data => (x -> zscore(log.(x))) => :ldensity_data_z,
				   :density_data => (x -> log.(x)) => :ldensity_data,
				   :cityarea => zscore => :cityarea_z,
				   :cityarea => (x -> log.(x)) => :lcityarea,
	               :area_data => (x -> log.(x)) => :larea_data,
	               :area_data => zscore => :area_data_z,
				   :Lu => zscore => :Lu_z,
				   :Lu => (x -> log.(x)) => :lLu,
				   :pop_data => zscore => :pop_data_z,
				   :pop_data => (x -> log.(x)) => :lpop_data)


	# 1. Compare E[20 cities] with average city: urban denity, relative price and rural employment
	# -------------------------------------------------------------------------------------------

	function plotE20(df)
		g3 = combine(groupby(d, :year), 
		# [:avgd_n,:L] => ((x,y) -> mean(x,Weights(y))) => :avgdensity,
		:avgd_n => mean => :avgdensity,
		:pr_1950 => mean => :avgpr,
		:Lr => mean => :avgLr)

		pa = @df g3 plot(:year, :avgdensity, label = "Avg over $K" , title = "Urban Density", lw = 2)
		plot!(pa, d1.year, d1.avgd_n, label = "Single city", lw = 2)

		pp = @df g3 plot(:year, :avgpr, label = "Avg over $K" , title = "Relative Price", lw = 2)
		plot!(pp, d1.year, d1.pr_1950, label = "Single city", lw = 2)

		pl = @df g3 plot(:year, :avgLr, label = "Avg over $K" , title = "Rural Employment", lw = 2)
		plot!(pl, d1.year, d1.Lr, label = "Single city", lw = 2)
		plot(pa,pp,pl, layout = (1,3), size = (900,300))
	end

	di[:base][:p1vsk] = plotE20(d)
	di[:d0d1][:p1vsk] = plotE20(d2)

	if save
		savefig(di[:base][:p1vsk], joinpath(LandUse.dbplots(),"k20-density-vs1.pdf"))
	end

	# 2. model vs data: urban population, area, density
	# -------------------------------------------------
	


	di[:base][:log_dens_mvd] = modvsdata(dy, :lcitydensity, :ldensity_data,xan = 2.5,yan=11,title = "Density")
	di[:base][:log_dens_mvd_yr] = modvsdata(groupby(dy,:year), :lcitydensity, :ldensity_data,title = "",xlab = L"$\log$ density model",ylab = L"$\log$ density data")
	di[:base][:n_dens_mvd] = modvsdata(dy, :nlcitydensity, :ldensity_data)
	di[:base][:z_dens_mvd] = modvsdata(dy, :lcitydensity_z, :ldensity_data_z,xan = 0,yan=1.5)

	di[:d0d1][:log_dens_mvd] = modvsdata(d2y, :lcitydensity, :ldensity_data,xan = 2.5,yan=11)
	di[:d0d1][:n_dens_mvd] = modvsdata(d2y, :nlcitydensity, :ldensity_data)
	di[:d0d1][:z_dens_mvd] = modvsdata(d2y, :lcitydensity_z, :ldensity_data_z,xan = 0,yan=1.5)

	di[:base][:log_Lu_mvd] = modvsdata(dy, :lLu, :lpop_data,xan = 1.5,yan=13)
	di[:base][:z_Lu_mvd] = modvsdata(dy, :Lu_z, :pop_data_z,xan = 3,yan=2.5)

	di[:d0d1][:log_Lu_mvd] = modvsdata(d2y, :lLu, :lpop_data,xan = 1.5,yan=13)
	di[:d0d1][:z_Lu_mvd] = modvsdata(d2y, :Lu_z, :pop_data_z,xan = 3,yan=2.5)

	di[:base][:log_area_mvd] = modvsdata(dy, :lcityarea, :larea_data,xan = -4.5,yan=5)
	di[:base][:log_area_mvd_yr] = modvsdata(groupby(dy, :year), :lcityarea, :larea_data,title = "",xlab = L"$\log$ area model",ylab = L"$\log$ area data")
	di[:base][:z_area_mvd] = modvsdata(dy, :cityarea_z, :area_data_z)

	di[:d0d1][:log_area_mvd] = modvsdata(d2y, :lcityarea, :larea_data,xan = -4.5,yan=5)
	di[:d0d1][:z_area_mvd] = modvsdata(d2y, :cityarea_z, :area_data_z)

	# di[:dens_mod_data] = plot(plot(di[:dens_mod_data_base3],title = "Baseline"), 
	#                           di[:dens_mod_data_d1d2_nocolor], layout = (1,2), size = (800,400))

	# do same for 2010 only
	dy2010 = subset(dy, :year => x->x.==2010 )
	d2y2010 = subset(d2y, :year => x->x.==2010)

	# re-standardize
	transform!(dy2010, 
				   :citydensity => (x -> zscore(log.(x))) => :lcitydensity_z,
				   :density_data => (x -> zscore(log.(x))) => :ldensity_data_z,
				   :cityarea => zscore => :cityarea_z,
	               :area_data => zscore => :area_data_z,
				   :Lu => zscore => :Lu_z,
				   :pop_data => zscore => :pop_data_z)

	transform!(d2y2010, 
				   :citydensity => (x -> zscore(log.(x))) => :lcitydensity_z,
				   :density_data => (x -> zscore(log.(x))) => :ldensity_data_z,
				   :cityarea => zscore => :cityarea_z,
	               :area_data => zscore => :area_data_z,
				   :Lu => zscore => :Lu_z,
				   :pop_data => zscore => :pop_data_z)				   

	di[:base][:log_dens_mvd_2010] = modvsdata(dy2010, :lcitydensity, :ldensity_data)
	di[:base][:n_dens_mvd_2010] = modvsdata(dy2010, :nlcitydensity, :ldensity_data)
	di[:base][:z_dens_mvd_2010] = modvsdata(dy2010, :lcitydensity_z, :ldensity_data_z)
	di[:base][:log_area_mvd_2010] = modvsdata(dy2010, :lcityarea, :larea_data,xan = -1.5,yan = 5)
	di[:base][:z_area_mvd_2010] = modvsdata(dy2010, :cityarea_z, :area_data_z,xan = 2,yan = 1)

	di[:d0d1][:log_dens_mvd_2010] = modvsdata(d2y2010, :lcitydensity, :ldensity_data)
	di[:d0d1][:n_dens_mvd_2010] = modvsdata(d2y2010, :nlcitydensity, :ldensity_data)
	di[:d0d1][:z_dens_mvd_2010] = modvsdata(d2y2010, :lcitydensity_z, :ldensity_data_z)
	di[:d0d1][:log_area_mvd_2010] = modvsdata(d2y2010, :lcityarea, :larea_data,xan = -1.5,yan = 5)
	di[:d0d1][:z_area_mvd_2010] = modvsdata(d2y2010, :cityarea_z, :area_data_z,xan = 2,yan = 1)

	
	# 3. model vs data: density baseline and density d0d1
	# -------------------------------------------------

	di[:areas] = plot(
		plot(di[:base][:log_area_mvd], title = "Baseline"),
		plot(di[:d0d1][:log_area_mvd], title = "d0,d1 Extension"),
		size = (800,400),
		layout = (1,2),
		left_margin = 5Plots.mm,
		bottom_margin = 5Plots.mm
	)
	if save
		savefig(di[:areas], joinpath(LandUse.dbplots(),"k20-area-x-sections.pdf"))
	end

	di[:pop_area_density] = plot(
		plot(di[:base][:log_Lu_mvd], title = "Urban Population"),
		plot(di[:base][:log_area_mvd], title = "Urban Area"),
		plot(di[:d0d1][:log_dens_mvd], title = "Density"),
		size = (1100,400), 
		layout = (1,3),
		left_margin = 5Plots.mm,
		bottom_margin = 5Plots.mm
	)
	if save
		savefig(di[:pop_area_density], joinpath(LandUse.dbplots(),"k20-pop-area-density.pdf"))
	end

	di
end

"""
	dashk20(;save = false)

Produce and optionally save model output for 20 city version.
"""
function dashk20(;save = false)

	# compare model to data
	# =====================
	# get a single city to compare
	x,M,p = runm()
	d1 = dataframe(M,p)


	di = Dict()

	ddd = k20_dfexport()
	K = maximum(ddd[1].region)

	ddd[1][!,:scenario] .= "baseline"
	ddd[2][!,:scenario] .= "d1-d2"
	dd = vcat(ddd[1],ddd[2])
	d = ddd[1]
	d2 = ddd[2]


	# baseline vs policy: exponential coefs
	# ==================

	# exponential coefficient
	for yr in p.T[[1,5,10,15,19]]
		x = select(subset(dd, :year => x -> x.== yr), :exp_coef => (x -> abs.(x)) => :exp_coef, :region, :scenario)
		di[Symbol("d1d2_coefs_$yr")] = @df x plot(:region, :exp_coef, group = :scenario, marker = (:auto, 3), leg = :bottomright,
									ylab = "exponential exponent (abs)", xlab = "city rank", title = "Exponential coefs in $yr")

		if save
			savefig(di[Symbol("d1d2_coefs_$yr")], joinpath(LandUse.dbplots(),"k20-exp-coefs-$yr.pdf"))
		end
	end

	# radius of cities
	radii = DataFrame(year = [2010, 2020], 
	                  singleϕ = [M[end].ϕ, M[end-1].ϕ])

	r2 = combine(groupby(subset(ddd[1], :year => x -> x .∈ Ref([2010, 2020])
       ), :year), 
	   :ϕ => mean => :meanϕ,
	   [:ϕ, :Lu] => ((x,y) -> mean(x,fweights(y))) => :wgtmeanϕ
	   )
	radii = leftjoin(radii, r2, on = :year)

	CSV.write(joinpath(LandUse.dbtables(),"radii.csv"),radii)

	# make x sectional plots data vs model
	# ====================================

	densyears = [1870, 1950,1970,1990,2010]

	dy = subset(d, :year => x -> x .∈ Ref(densyears))
	d2y = subset(d2, :year => x -> x .∈ Ref(densyears))

	meandata = mean(dy.density_data)
	meanmodel = mean(dy.citydensity)
	disallowmissing!(dy)
	disallowmissing!(d2y)

	# transformations
	# simple log
	# zscores
	# zscores

	transform!(dy, :citydensity => (x -> x .* (meandata / meanmodel)) => :ncitydensity,
				   :citydensity => (x -> log.(x)) => :lcitydensity,
				   :citydensity => (x -> zscore(log.(x))) => :lcitydensity_dm,
				   :density_data => (x -> zscore(log.(x))) => :ldensity_data_dm,
				   :density_data => (x -> log.(x)) => :ldensity_data,
				   :cityarea => zscore => :cityarea_dm,
				   :cityarea => (x -> log.(x)) => :lcityarea,
	               :area_data => (x -> log.(x)) => :larea_data,
	               :area_data => zscore => :area_data_dm,
				   :Lu => zscore => :Lu_dm,
				   :pop_data => zscore => :pop_data_dm)

	

	meandata = mean(d2y.density_data)
	meanmodel = mean(d2y.citydensity)
	transform!(d2y, :citydensity => (x -> x .* (meandata / meanmodel)) => :ncitydensity,
				   :citydensity => (x -> zscore(log.(x))) => :lcitydensity_dm,
				   :density_data => (x -> zscore(log.(x))) => :ldensity_data_dm,
	               :cityarea => zscore => :cityarea_dm,
	               :area_data => zscore => :area_data_dm,
				   :Lu => zscore => :Lu_dm,
				   :pop_data => zscore => :pop_data_dm)

	dly = transform(dy, :density_data => (x -> log.(x)) => :ldensity_data, 
	                    :citydensity => (x -> log.(x)) => :lcitydensity,
						:ncitydensity => (x -> log.(x)) => :nlcitydensity)
	transform!(dly, :ldensity_data => zscore => :ldensity_data_z,
					:nlcitydensity => zscore => :nlcitydensity_z
	)

	d2ly = transform(d2y, :density_data => (x -> log.(x)) => :ldensity_data, 
	                    :citydensity => (x -> log.(x)) => :lcitydensity,
						:ncitydensity => (x -> log.(x)) => :nlcitydensity)

	r0 = lm(@formula( ldensity_data ~ nlcitydensity), dly)
	r1 = lm(@formula( ldensity_data ~ nlcitydensity), d2ly)


	di[:dens_mod_data_base] = @df dly plot(:nlcitydensity, :ldensity_data, group = :LIBGEO, leg = :outerright, 
		xlab = "log normalized model density", ylab = "log data density", title = "Urban Density X-section over time",
		color = reshape(palette(:tab20)[1:20], 1,20),
		arrow = 2, 
		markershape = :circle,
		markersize = 4)
	plot!(di[:dens_mod_data_base], x -> coef(r0)[1] + coef(r0)[2] * x, lab = "", linewidth = 2, color = "black")
	annotate!(di[:dens_mod_data_base], [(11.5, 11.8, Plots.text("slope=$(round(coef(r0)[2],digits = 3))", 12))])

	di[:dens_mod_data_base2] = @df dly scatter(:nlcitydensity, :ldensity_data, leg = false, 
		xlab = "Model", 
		ylab = "Data")
	plot!(di[:dens_mod_data_base2], x -> coef(r0)[1] + coef(r0)[2] * x, lab = "", linewidth = 2, color = "black")
	annotate!(di[:dens_mod_data_base2], [(11, 10.0, Plots.text("slope=$(round(coef(r0)[2],digits = 3))", 12))])


	r3 = lm(@formula( ldensity_data_dm ~ lcitydensity_dm), dy)
	di[:dens_mod_data_base3] = @df dy scatter(:lcitydensity_dm, :ldensity_data_dm, leg = false, 
	xlab = "z(log(Model))", 
	ylab = "z(log(Data))",title = "Density")
	plot!(di[:dens_mod_data_base3], x -> coef(r3)[1] + coef(r3)[2] * x, lab = "", linewidth = 2, color = "black")
	annotate!(di[:dens_mod_data_base3], [(0, 2, Plots.text("slope=$(round(coef(r3)[2],digits = 3))", 12))])

	r4 = lm(@formula( ldensity_data ~ lcitydensity), dy)
	di[:dens_mod_data_base4] = @df dy scatter(:lcitydensity, :ldensity_data, leg = false, 
	xlab = "log(Model)", 
	ylab = "log(Data)",title = "Density")
	plot!(di[:dens_mod_data_base4], x -> coef(r4)[1] + coef(r4)[2] * x, lab = "", linewidth = 2, color = "black")
	annotate!(di[:dens_mod_data_base4], [(2.5, 11, Plots.text("slope=$(round(coef(r4)[2],digits = 3))", 12))])


	di[:dens_mod_data_d1d2] = @df d2ly plot(:nlcitydensity, :ldensity_data, group = :LIBGEO, leg = :outerright, 
		xlab = "log normalized model density", ylab = "log data density", title = "X-section with d0, d1",
		color = reshape(palette(:tab20)[1:20], 1,20),
		arrow = 2, 
		markershape = :circle,
		markersize = 4)
	plot!(di[:dens_mod_data_d1d2], x -> coef(r1)[1] + coef(r1)[2] * x, lab = "", linewidth = 2, color = "black")
	annotate!(di[:dens_mod_data_d1d2], [(11.5, 11.8, Plots.text("slope=$(round(coef(r1)[2],digits = 3))", 12))])

	di[:dens_mod_data_d1d2_nocolor] = @df d2ly scatter(:nlcitydensity, :ldensity_data, leg = false, 
		xlab = "log normalized model density", ylab = "log data density", title = "X-section with d0, d1")
	plot!(di[:dens_mod_data_d1d2_nocolor], x -> coef(r1)[1] + coef(r1)[2] * x, lab = "", linewidth = 2, color = "black")
	annotate!(di[:dens_mod_data_d1d2_nocolor], [(11.5, 11.8, Plots.text("slope=$(round(coef(r1)[2],digits = 3))", 12))])

	r4 = lm(@formula( ldensity_data_dm ~ lcitydensity_dm), d2y)
	di[:dens_mod_data_d1d2_nocolor] = @df d2y scatter(:lcitydensity_dm, :ldensity_data_dm, leg = false, 
	xlab = "Model", 
	ylab = "Data", title = "d0, d1 Extension")
	plot!(di[:dens_mod_data_d1d2_nocolor], x -> coef(r4)[1] + coef(r4)[2] * x, lab = "", linewidth = 2, color = "black")
	annotate!(di[:dens_mod_data_d1d2_nocolor], [(0, 2, Plots.text("slope=$(round(coef(r4)[2],digits = 3))", 12))])

	di[:dens_mod_data] = plot(plot(di[:dens_mod_data_base3],title = "Baseline"), 
	                          di[:dens_mod_data_d1d2_nocolor], layout = (1,2), size = (800,400))


	# pick a year and do there
	dly2010 = subset(dly, :year => x->x.==2010 )
	d2ly2010 = subset(d2ly, :year => x->x.==2010)
	
	r0 = lm(@formula( ldensity_data ~ nlcitydensity), dly2010)
	r1 = lm(@formula( ldensity_data ~ nlcitydensity), d2ly2010)

	di[:dens_mod_data_base2010] = @df dly2010 scatter(:nlcitydensity, :ldensity_data, leg = false, 
	xlab = "Model", 
	ylab = "Data",
	title = "Baseline")
	plot!(di[:dens_mod_data_base2010], x -> coef(r0)[1] + coef(r0)[2] * x, lab = "", linewidth = 2, color = "black")
	annotate!(di[:dens_mod_data_base2010], [(9, 8.2, Plots.text("slope=$(round(coef(r0)[2],digits = 3))", 12))])

	di[:dens_mod_data_d1d2_2010] = @df d2ly2010 scatter(:nlcitydensity, :ldensity_data, leg = false, 
	xlab = "Model", 
	ylab = "Data",
	title = "d0, d1 Extension")
	plot!(di[:dens_mod_data_d1d2_2010], x -> coef(r1)[1] + coef(r1)[2] * x, lab = "", linewidth = 2, color = "black")
	annotate!(di[:dens_mod_data_d1d2_2010], [(9, 8.2, Plots.text("slope=$(round(coef(r1)[2],digits = 3))", 12))])

	di[:dens_mod_2010] = plot(di[:dens_mod_data_base2010],di[:dens_mod_data_d1d2_2010], 
	       leg = (1,2), size = (800,400),
		   link = :both,
		   xlims = (7.5,10.5),
		   left_margin = 5Plots.mm,
		   bottom_margin = 5Plots.mm)
	
	if save
		savefig(di[:dens_mod_data_base], joinpath(LandUse.dbplots(),"k20-xsect-time.pdf"))
		savefig(di[:dens_mod_2010], joinpath(LandUse.dbplots(),"k20-xsect-2010.pdf"))
		savefig(di[:dens_mod_data_base2], joinpath(LandUse.dbplots(),"k20-xsect-time-nocolor.pdf"))
		savefig(di[:dens_mod_data_d1d2], joinpath(LandUse.dbplots(),"k20-xsect-time-d0d1.pdf"))
		savefig(di[:dens_mod_data_d1d2_nocolor], joinpath(LandUse.dbplots(),"k20-xsect-time-d0d1-nocolor.pdf"))
	end



	# add rel pop, rel urban area and rel dens
	d = transform(groupby(d,:year), [:Lu, :region]          => ((a,b) -> a ./ a[b .== 1]) => :rel_Lu,
	                                [:citydensity, :region] => ((a,b) -> a ./ a[b .== 1]) => :rel_density,
	                                [:rel_cityarea, :region] => ((a,b) -> a ./ a[b .== 1]) => :nrel_cityarea)

	# add rel pop, rel urban area and rel dens
	d2 = transform(groupby(d2,:year), [:Lu, :region]          => ((a,b) -> a ./ a[b .== 1]) => :rel_Lu,
	                                [:citydensity, :region] => ((a,b) -> a ./ a[b .== 1]) => :rel_density,
	                                [:rel_cityarea, :region] => ((a,b) -> a ./ a[b .== 1]) => :nrel_cityarea)


	# no paris
	nop = subset(d, :region => x -> x .> 1, :year => x -> x .∈ Ref(densyears))


	# relative population
	c20 = palette(:tab20)
	pl = @df subset(nop, :region => x -> x.==2) plot(:year, :rel_Lu, leg = :outertopright, linecolor = c20[2], lab = "Lyon",
	ylab = "Population rel to Paris" )
	@df subset(nop, :region => x -> x.==2) scatter!(:datayears, :relpop_data, markercolor = c20[2], lab = "" )
	for ik in 3:K
		dk = subset(nop, :region => x -> x.==ik)
		plot!(pl,dk.year, dk.rel_Lu,  linecolor = c20[ik], lab = dk[1,:LIBGEO] )
		scatter!(pl,dk.datayears, dk.relpop_data,  markercolor = c20[ik], lab = "" )
	end
	title!(pl, "Model (Lines) vs Data (Points)")

	di[:relpop_data] = pl

	r2 = lm(@formula( pop_data_dm ~ Lu_dm ), dy)
	di[:relpop_data2] = @df dy scatter(:Lu_dm, :pop_data_dm, leg = false, 
	           ylab = "Data", xlab = "Model",
			   title = "Urban Population")
	plot!(di[:relpop_data2], x -> coef(r2)[1] + coef(r2)[2] * x, lab = "", linewidth = 2, color = "black")
	annotate!(di[:relpop_data2], [(2, 4, Plots.text("slope=$(round(coef(r2)[2],digits = 3))", 12))])
		   

	r2 = lm(@formula( area_data_dm ~ cityarea_dm ), dy)
	di[:relarea_data] = @df dy scatter(:cityarea_dm, :area_data_dm, 
	          leg = false, ylab = "Data", xlab = "Model",
			  title = "Urban Area",aspect_ratio = 1)
	plot!(di[:relarea_data], x -> coef(r2)[1] + coef(r2)[2] * x, lab = "", linewidth = 2, color = "black")
	annotate!(di[:relarea_data], [(3, 0, Plots.text("slope=$(round(coef(r2)[2],digits = 3))", 12))])

	di[:relarea_data_color] = @df nop plot(:nrel_cityarea, :relarea_data, group = :LIBGEO, 
	          leg = :outerright, ylab = "Data", xlab = "Model",
			  color = reshape(palette(:tab20)[1:20], 1,20),
			  arrow = 2, 
			  markershape = :circle,
			  markersize = 4,
			  title = "Rel. Area by city")
	plot!(di[:relarea_data_color], x -> coef(r2)[1] + coef(r2)[2] * x, lab = "", linewidth = 2, color = "black")
	annotate!(di[:relarea_data_color], [(0.25, 0.11, Plots.text("slope=$(round(coef(r2)[2],digits = 3))", 12))])

	di[:mod_vs_data_density] = plot(di[:relpop_data2], di[:relarea_data], plot(di[:dens_mod_data_base3], title = "Density"),
	     size = (1100,400), 
		 layout = (1,3),
		 left_margin = 5Plots.mm,
		 bottom_margin = 5Plots.mm)


	# single year (2010) analysis
	# ===========================

	y2010 = subset(d, :year => x -> x .== 2010)
	y22010 = subset(d2, :year => x -> x .== 2010)

	# demean area
	transform!(y2010, :cityarea => (x -> (x .- mean(x))./std(x)) => :cityarea_dm,
	                  :area_data => (x -> (x .- mean(x))./std(x)) => :area_data_dm)

	transform!(y22010, :cityarea => (x -> (x .- mean(x))./std(x)) => :cityarea_dm,
		               :area_data => (x -> (x .- mean(x))./std(x)) => :area_data_dm)					  

	r2 = lm(@formula( area_data_dm ~ cityarea_dm ), y2010)
	di[:area_data_2010] = @df y2010 scatter(:cityarea_dm, :area_data_dm, 
			leg = false, ylab = "Data", xlab = "Model",
			title = "Baseline")
	plot!(di[:area_data_2010], x -> coef(r2)[1] + coef(r2)[2] * x, lab = "", linewidth = 2, color = "black")
	annotate!(di[:area_data_2010], [(1, 2, Plots.text("slope=$(round(coef(r2)[2],digits = 3))", 12))])
	 
	r2 = lm(@formula( area_data_dm ~ cityarea_dm ), y22010)
	di[:area_data_2010_d1d2] = @df y22010 scatter(:cityarea_dm, :area_data_dm, 
			leg = false, ylab = "Data", xlab = "Model",
			title = "d0,d1 Extension")
	plot!(di[:area_data_2010_d1d2], x -> coef(r2)[1] + coef(r2)[2] * x, lab = "", linewidth = 2, color = "black")
	annotate!(di[:area_data_2010_d1d2], [(1, 2, Plots.text("slope=$(round(coef(r2)[2],digits = 3))", 12))])
	
	di[:area_data_2010] = plot(di[:area_data_2010],di[:area_data_2010_d1d2],
								 link = :y,
								 size = (800,400) ,
								 left_margin = 5Plots.mm,
								 bottom_margin = 5Plots.mm,aspect_ratio = 1)

	di[:relpop_nop] = @df nop plot(:year, :rel_Lu, group = :LIBGEO, leg = :outertopright, col = palette(:tab20))
	@df nop scatter!(di[:relpop_nop],:datayears, :relpop_data, group = :LIBGEO, lab = false, col = palette(:tab20))

	di[:rpop] = @df d plot(:year,:Lr, group = :region, ylab = "Share of Rural Population")
	di[:pop] = @df d plot(:year,:Lu, group = :region, ylab = "Urban Population")
	di[:popn] = @df d plot(:year,:Lu_n, group = :region, ylab = "Share of Urban Population")
	di[:relpop] = @df nop plot(:year,:rel_Lu, group = :region,
	                   ylab = "Population relative to Biggest City")
	di[:dens_pop] = @df d plot(:Lu, :citydensity, group = :region, 
	                    xaxis = ("log urban pop", :log10), yaxis = ("log density", :log10), leg = false)

	dparis = subset(d, :region => x-> x.==1)
	di[:dens_paris] = @df dparis plot(:year, :citydensity, label = "Average", lw = 2, title = "Paris Density", ylab = "Density")
	plot!(di[:dens_paris], dparis.year, dparis.d0, label = "Central", lw = 2)

	di[:densn_paris] = @df dparis plot(:year, :avgd_n, label = "Average", lw = 2, title = "Paris Normalized Density", ylab = "Density")
	plot!(di[:densn_paris], dparis.year, dparis.d0_n, label = "Central", lw = 2)
	plot!(di[:densn_paris], dparis.year, dparis.dr_n, label = "Fringe", lw = 2)

	dparis = subset(d, :region => x-> x.==2)
	di[:dens_lyon] = @df dparis plot(:year, :citydensity, label = "Average", lw = 2, title = "Lyon Density", ylab = "Density")
	plot!(di[:dens_lyon], dparis.year, dparis.d0, label = "Central", lw = 2)

	di[:densn_lyon] = @df dparis plot(:year, :avgd_n, label = "Average", lw = 2, title = "Lyon Normalized Density", ylab = "Density")
	plot!(di[:densn_lyon], dparis.year, dparis.d0_n, label = "Central", lw = 2)
	plot!(di[:densn_lyon], dparis.year, dparis.dr_n, label = "Fringe", lw = 2)

	sort!(d, [:year, :region])
	x20 = subset(d,:year => x -> x.==2020)
	w2020 = select(x20,:region,:Lu => :weights)

	g3 = combine(groupby(d, :year), 
	      :avgd_n => mean => :avgdensity,
	      :pr_1950 => mean => :avgpr,
	      :Lr => mean => :avgLr)

	gg4 = leftjoin(d,w2020,on = :region)
	disallowmissing!(gg4,:weights)
	g4 = combine(groupby(gg4, :year), 
	     [:avgd_n, :weights] => ((x,y) -> mean(x,fweights(y))) => :wtd_avgdensity,
	     [:pr_1950, :weights] => ((x,y) -> mean(x,fweights(y))) => :wtd_price,
	     [:Lr, :weights] => ((x,y) -> mean(x,fweights(y))) => :wtd_Lr)

	
	di[:avg_density] = @df g3 plot(:year, :avgdensity, label = "Avg over $K" , title = "Urban Density", lw = 2)
	plot!(di[:avg_density], d1.year, d1.avgd_n, label = "Single city", lw = 2)

	di[:avg_pr] = @df g3 plot(:year, :avgpr, label = "Avg over $K" , title = "Relative Price", lw = 2)
	plot!(di[:avg_pr], d1.year, d1.pr_1950, label = "Single city", lw = 2)

	di[:avg_Lr] = @df g3 plot(:year, :avgLr, label = "Avg over $K" , title = "Rural Employment", lw = 2)
	plot!(di[:avg_Lr], d1.year, d1.Lr, label = "Single city", lw = 2)



	di[:wtd_avg_density] = @df g4 plot(:year, :wtd_avgdensity, label = "Avg over $K" , title = "Average Densities (wtd)", lw = 2)
	plot!(di[:wtd_avg_density], d1.year, d1.citydensity, label = "Single city", lw = 2)

	di[:wtd_avg_p] = @df g4 plot(:year, :wtd_price, label = "Avg over $K" , title = "Average Price (wtd)", lw = 2)
	plot!(di[:wtd_avg_p], d1.year, d1.pr, label = "Single city", lw = 2)

	di[:wtd_avg_Lr] = @df g4 plot(:year, :wtd_Lr, label = "Avg over $K" , title = "Average Rural Employment (wtd)", lw = 2)
	plot!(di[:wtd_avg_Lr], d1.year, d1.Lr, label = "Single city", lw = 2)


	di[:density_kvs1] = @df d plot(:year, :citydensity, group = :LIBGEO, label = "", color = :lightgray, leg = false)
	plot!(di[:density_kvs1],d1.year, d1.citydensity, color = :red ,lw = 3, title = "Urban Density")

	di[:relarea_kvs1] = @df d plot(:year, :rel_cityarea, group = :LIBGEO, label = "", color = :lightgray, leg = false)
	plot!(di[:relarea_kvs1],d1.year, d1.rel_cityarea, color = :red ,lw = 3, title = "Urban Area")
	
	di[:Lu_kvs1] = @df d plot(:year, :Lu, group = :LIBGEO, label = "", color = :lightgray)
	plot!(di[:Lu_kvs1],d1.year, d1.Lu, label = "Single city", color = :red ,lw = 3, title = "Urban Population", leg = :topleft)

	# cross section of densities
	sort!(d, [:region, :year])
	di[:density_3d] = wireframe(unique(d.region), unique(d.year), 
	                            reshape(d.citydensity,19,20), 
								camera = (70,40),xlab = "City", 
								ylab = "year",zlab = "Average Density", 
								title = "Density Cross Section over Time")


	

	# within density in year 2020
	de2020 = hcat( Matrix(select(subset(d , :year => x -> x .== 2020 ),  :iDensities))... )
	di[:dens_2020] = wireframe(1:K,1:p.int_bins,de2020, 
	                          xlab = "city rank", ylab = "distance bin",
							  zlab = "density",camera = (70,40),
							  title = "year 2020 within city gradients")

	# 1 by 3 plot of 1 vs k
	# di[:p1vsk] = plot(di[:Lu_kvs1],di[:relarea_kvs1],di[:density_kvs1], layout = (1,3), size = (900,300))
	
	di[:p1vsk] = plot(di[:avg_density],di[:avg_pr],di[:avg_Lr], layout = (1,3), size = (900,300))

	if save
		savefig(di[:area_data_2010], joinpath(LandUse.dbplots(),"k20-relarea-data-model-2010.pdf"))
		savefig(di[:mod_vs_data_density], joinpath(LandUse.dbplots(),"k20-density-data-model.pdf"))
		savefig(di[:p1vsk], joinpath(LandUse.dbplots(),"k20-density-vs1.pdf"))
		savefig(di[:dens_2020], joinpath(LandUse.dbplots(),"k20-density3D-2020.pdf"))
		savefig(di[:relpop_data], joinpath(LandUse.dbplots(),"k20-relpop-model-data.pdf"))
		savefig(di[:relpop_data2], joinpath(LandUse.dbplots(),"k20-relpop-model-data-nocolor.pdf"))
		savefig(di[:relarea_data], joinpath(LandUse.dbplots(),"k20-relarea-model-data.pdf"))
		savefig(di[:relarea_data_color], joinpath(LandUse.dbplots(),"k20-relarea-model-data-color.pdf"))
		savefig(di[:density_3d], joinpath(LandUse.dbplots(),"k20-density3D.pdf"))
	end


	# compare model to data
	# =====================

	di
end

"single spatial cross sections region"
function cs_plots(m::Region,p::Param,it::Int; fixy = false, objvalue = nothing)

	lvec = collect(range(0.,m.ϕ,length=100))  # space for region ik
	lvec0 = collect(range(0.01,m.ϕ,length=100))  # space for region ik
	setperiod!(p,it)  # set time on parameter!
	ti = p.T[it]  # acutal year for printing
	d = Dict()

	# get data
	ϵd = [ϵ(i,m.ϕ,p) for i in lvec]
	Dd = [D(i,p,m) for i in lvec]
	Hd = [H(i,p,m) for i in lvec]
	ρd = [ρ(i,p,m) for i in lvec]
	qd = [q(i,p,m) for i in lvec]	
	md = [mode(i,p,m.Lu) for i in lvec0]

	# get ratio first over last point
	ϵg = round(ϵd[1]/ϵd[end],digits =1)
	Dg = round(Dd[1]/Dd[end],digits =1)
	Hg = round(Hd[1]/Hd[end],digits =1)
	ρg = round(ρd[1]/ρd[end],digits =1)
	qg = round(qd[1]/qd[end],digits =1)

	# get 90/10 ratio of density
	d1090 = round(m.iDensity_q10 / m.iDensity_q90, digits = 1)

	# run exponential decay model
	ndensities = m.iDensities ./ m.iDensities[1]
	gradient,emod = expmodel(1:p.int_bins, ndensities)
	MSE = round(1000 * mse(emod),digits = 3)

	# get elasticity of speed wrt distance
	# it's a straight line
	spela = diff(log.(md[[1,end]])) ./ diff(log.(lvec0[[1,end]]))

	if isnothing(objvalue)
		manno = (log(m.ϕ*0.2) , log(0.5*maximum(md)) , "Elasticity: $(round(spela[1],digits=2))")
	else
		manno = (log.([m.ϕ*0.2,m.ϕ*0.2 ]) , log.([0.5*maximum(md),0.3*maximum(md)]) , ["Elasticity: $(round(spela[1],digits=2))", "Objective: $(round(objvalue,digits = 2))"])
	end


	if fixy
		d[:ϵ] = plot(lvec , ϵd , title = latexstring("\\epsilon(l,$(ti))") , ylims = (2    , 4.1) , linewidth = 2 , leg = false , xlab = "distance" , annotations = (m.ϕ*0.8 , 0.9*maximum(ϵd) , "$(ϵg)x"))                   

		d[:D] = Plots.scatter(1:p.int_bins, ndensities, m = (:circle, :red, 4), leg = false,title = latexstring("D(l,$(ti))")  )
		plot!(d[:D],1:p.int_bins, x -> gradient[1] .* exp.(gradient[2] * x), linewidth = 2, xlab = "distance", 
		            annotations = ([p.int_bins*0.7 ] , [0.9], ["exp.coef=$(round(gradient[2],digits=2))\n10/90=$(d1090)\nMSE=$MSE"]))
		
		d[:Dpaper] = Plots.scatter(1:p.int_bins, ndensities, m = (:circle, :green, 4), label = "Urban Density (Model)")
		plot!(d[:Dpaper],1:p.int_bins, x -> 1.0 .* exp.(-0.16 * (x-1)), linewidth = 2, xlab = "distance bin", label = "Fitted Curve (Data)")

		# d[:D] = plot(lvec , Dd , title = latexstring("D(l,$(ti))")         , ylims = (-3   , 60)  , linewidth = 2 , leg = false , xlab = "distance" , annotations = ([m.ϕ*0.7 ] , [0.9*maximum(Dd)], ["10/90=$(round(m.iDensity_q10,digits=1))/$(round(m.iDensity_q90,digits=1))\n=$(d1090)"]))
		# vline!(d[:D],[m.ϕ10, m.ϕ90], color = :red,leg = false)
		d[:H] = plot(lvec , Hd , title = latexstring("H(l,$(ti))")         , ylims = (-0.1 , 15)  , linewidth = 2 , leg = false , xlab = "distance" , annotations = (m.ϕ*0.8 , 0.9*maximum(Hd) , "$(Hg)x"))
		d[:ρ] = plot(lvec , ρd , title = latexstring("\\rho(l,$(ti))")     , ylims = (-0.1 , 10)  , linewidth = 2 , leg = false , xlab = "distance" , annotations = (m.ϕ*0.8 , 0.9*maximum(ρd) , "$(ρg)x"))
		d[:q] = plot(lvec , qd , title = latexstring("q(l,$(ti))")         , ylims = (0.5  , 5.5) , linewidth = 2 , leg = false , xlab = "distance" , annotations = (m.ϕ*0.8 , 0.9*maximum(qd) , "$(qg)x"))
		d[:mode] = plot(log.(lvec0) , log.(md) , title = latexstring("mode(l,$(ti))")       , linewidth = 2 , leg = false , xlab = "distance (log scale)" , annotations = manno)
	else
		d[:ϵ] = plot(lvec , ϵd , title = latexstring("\\epsilon(l,$(ti))")     , linewidth = 2 , leg = false , xlab = "distance" , annotations = (m.ϕ*0.8 , 0.9*maximum(ϵd) , "$(ϵg)x"))
		d[:D] = Plots.scatter(1:p.int_bins, ndensities, m = (:circle, :red, 4), leg = false,title = latexstring("D(l,$(ti))")  )
		plot!(d[:D],1:p.int_bins, x -> gradient[1] .* exp.(gradient[2] * x), linewidth = 2, xlab = "distance", 
		            annotations =  ([p.int_bins*0.7 ] , [0.9], ["exp.coef=$(round(gradient[2],digits=2))\n10/90=$(d1090)\nMSE=$MSE"]))

		d[:Dpaper] = Plots.scatter(1:p.int_bins, ndensities, m = (:circle, :green, 4), label = "Urban Density (Model)")
		plot!(d[:Dpaper],1:p.int_bins, x -> 1.0 .* exp.(-0.16 * (x-1)), linewidth = 2, xlab = "distance bin", label = "Fitted Curve (Data)")
		d[:H] = plot(lvec , Hd , title = latexstring("H(l,$(ti))")             , linewidth = 2 , leg = false , xlab = "distance" , annotations = (m.ϕ*0.8 , 0.9*maximum(Hd) , "$(Hg)x"))
		d[:ρ] = plot(lvec , ρd , title = latexstring("\\rho(l,$(ti))")         , linewidth = 2 , leg = false , xlab = "distance" , annotations = (m.ϕ*0.8 , 0.9*maximum(ρd) , "$(ρg)x"))
		d[:q] = plot(lvec , qd , title = latexstring("q(l,$(ti))")             , linewidth = 2 , leg = false , xlab = "distance" , annotations = (m.ϕ*0.8 , 0.9*maximum(qd) , "$(qg)x"))
		d[:mode] = plot(log.(lvec0) , log.(md), title = latexstring("mode(l,$(ti))")       , linewidth = 2 , leg = false , xlab = "distance (log scale)" , annotations = manno)
	end
	d
end



"""
	ts_plots(M,p::Param;fixy = false)

Time series plots for a single [`Region`](@ref) `M`.
"""
function ts_plots(M,p::Param;fixy = false)
	d = dataframe(M,p)
	dd = Dict()
	df = select(d, :year, :hshare => :h, :ushare => :u, :rshare => :r)
	ds = stack(df, Not(:year))
	
	# marker
	mmark = (:circle, 4)

	# colors
	rg = ["red" "green"]
	brg = ["blue" "red" "green"]

	# subsets
	d1880 = subset(d, :year => x -> x .>= 1880)

	# normalizations
	transform!(d1880, :cityarea => firstnorm => :cityarea_n,
	                  :rel_cityarea => firstnorm => :rel_cityarea_n,
	                  :Lu => firstnorm => :Lu_n)

	i1900 = argmin( abs.(p.moments.year .- 1900) )
	i2015 = argmin( abs.(p.moments.year .- 2015) )
	i2010 = argmin( abs.(p.moments.year .- 2015) )
	i1870 = argmin( abs.(p.moments.year .- 1870) )

	h1900 = df[i1900,:h]
	hend = df[i2010,:h]
	dd[:spending] = @df ds plot(:year,:value, group = :variable,
			   linewidth = 2, title = "Spending Shares",
			   ylims = (0.0,0.83), marker = mmark, legend = :right, color = brg,
			   annotations = ([p.T[i1900],p.T.stop],[h1900-0.1, hend-0.1],["$(round(h1900,digits = 3))","$(round(hend,digits = 3))"]))

    # dd[:spending_data] = plot!(dd[:spending], p.moments.year, p.moments[!,[:SpendingShare_Housing, :SpendingShare_Urban,:SpendingShare_Rural]], color = brg)

	ds2 = stack(select(d,:year,:Lu, :Lr), Not(:year))
	dd[:pop] = @df ds2 plot(:year, :value, group = :variable,
					 linewidth = 2, title = "Population", marker = mmark,
					 legend = :right, color = rg)

	dd[:Lr_data] = @df d plot(:year, :Lr ./ p.Lt, label = "model",marker = mmark, color = "blue",linewidth = 2, title = "Rural Employment")
	plot!(dd[:Lr_data], p.moments.year, p.moments.Employment_rural, label = "data", color = "red",linewidth = 2)

	dd[:pr_data] = @df d plot(:year, :pr, label = "model",marker = mmark, color = "blue",linewidth = 2, title = "Rural Price")
	plot!(dd[:pr_data], p.moments.year, p.moments.P_rural, label = "data", color = "red",linewidth = 2)


	dd[:LrSr] = @df d plot(:year, :Lr ./ :Sr, linewidth = 2, color = "black", leg =false, title = L"\frac{L_r}{S_r}", marker = mmark )
	dd[:LrSr_Srh] = @df d plot(:year, :Lr ./ (:Sr .+ :Srh), linewidth = 2, color = "black", leg =false, title = "Lr over Sr", marker = mmark )
	dd[:ρ0_real] = @df d plot(:year, :ρ0_real, linewidth = 2, color = "black", leg =false, title = "Central Land Values", marker = mmark, ylims = fixy ? (0.0,29.0) : false )
	dd[:ρ0] = @df d plot(:year, :ρ0, linewidth = 2, color = "black", leg =false, title = "Central Land Values", marker = mmark, ylims = fixy ? (0.0,29.0) : false )
	dd[:qbar] = @df d plot(:year, :qbar, linewidth = 2, color = "black", leg =false, title = "Average House Prices", marker = mmark, ylims = fixy ? (0.0,29.0) : false )
	dd[:qbar_real] = @df d plot(:year, :qbar_real, linewidth = 2, color = "black", leg =false, title = "Average House Prices", marker = mmark, ylims = fixy ? (0.0,29.0) : false )
	dd[:ρ0_y] = @df d plot(:year, :ρ0_y, linewidth = 2, color = "black", leg =false, title = "Central Land Rents over Income", marker = mmark, ylims = fixy ? (0.0,200.0) : false )
	dd[:ρr_y] = @df d plot(:year, :ρr_y, linewidth = 2, color = "black", leg =false, title = "Fringe Land Rents over Income", marker = mmark, ylims = fixy ? (0.0,200.0) : false )
 	dd[:ρr] = @df d plot(:year, :ρr, linewidth = 2, color = "black", leg =false, title = "Fringe Land Values", marker = mmark, ylims = fixy ? (0.19,0.36) : false )
 	dd[:q0] = @df d plot(:year, :q0, linewidth = 2, color = "black", leg =false, title = "Central House Prices", marker = mmark, ylims = fixy ? (0.0,5.0) : false )
 	dd[:qr] = @df d plot(:year, :qr, linewidth = 2, color = "black", leg =false, title = "Fringe House Prices", marker = mmark, ylims = fixy ? (0.5,1.5) : false )
 	dd[:qr_real] = @df d plot(:year, :qr_real, linewidth = 2, color = "black", leg =false, title = "Fringe House Prices", marker = mmark, ylims = fixy ? (0.5,1.5) : false )
	dd[:hr] = @df d plot(:year, :hr, linewidth = 2, color = "black", leg =false, title = "Fringe Housing Demand", marker = mmark, ylims = fixy ? (0.0,5.0) : false )

	df = @chain d begin
		 select(:year,:rr,:r, :y ,:iq, :rr_real, :ru_real, :pop)
		 @transform(:r_y = 100*(:r .* :pop ./ :y), :rr_y = 100*(:rr ./ :y), :ru_y = 100*(:iq ./ :y))
	end
	dd[:r_y] = @df df plot(:year, [:r_y, :ru_y,:rr_y], labels = ["Total" "Urban" "Rural"],
	            linewidth = 2, title = "Rents as % of Income", marker = mmark,
				ylims = fixy ? (0,50) : false, color = brg,legend = :bottomright)


	df = @chain d begin
		 select(:year,:rr_real, :ru_real)
		 @transform(:rr_real_1 = :rr_real ./ :rr_real[1], :ru_real_1 = :ru_real ./ :ru_real[1])
	end
	dd[:rents_real] = @df df plot(:year, [:ru_real, :rr_real ], labels = ["Rural Rents" "Urban Rents"],
	            linewidth = 2, title = "Real Rents", marker = mmark,
				ylims = fixy ? (0,50) : false, legend = :left, color = rg)
	dd[:rents_real_1] = @df df plot(:year, [:ru_real_1,:rr_real_1], labels = ["Rural Rents" "Urban Rents"],
				linewidth = 2, title = "Real Rents", marker = mmark, ylims = fixy ? (0,50) : false,
				legend = :left, color = rg)
				en


	y1950 = findmin(abs.(d.year .- 1950))[2]
	dtemp = transform(select(d, :year, :r, :r_real, :ρ0, :ρr, :q0, :qr, :hr, :Hr, :qbar, :qq1),
	                  :r => ((x)  -> 100*(x ./ x[y1950])) => :r,
	                  :r_real => ((x)  -> 100*(x ./ x[y1950])) => :r_real,
	                  :ρ0 => ((x) -> 100*(x ./ x[y1950])) => :ρ0,
	                  :ρr => ((x) -> 100*(x ./ x[y1950])) => :ρr,
	                  :q0 => ((x) -> 100*(x ./ x[y1950])) => :q0,
	                  :qq1 => ((x) -> 100*(x ./ x[y1950])) => :qq1,
	                  :hr => ((x) -> 100*(x ./ x[y1950])) => :hr,
	                  :Hr => ((x) -> 100*(x ./ x[y1950])) => :Hr,
	                  :qbar => ((x) -> 100*(x ./ x[y1950])) => :qbar,
	                  :qr => ((x) -> 100*(x ./ x[y1950])) => :qr
					  )
	dd[:r] = @df dtemp plot(:year, :r, linewidth = 2, color = "black", leg =false, title = "Land Rents",ylab = "1950=100", marker = mmark, ylims = fixy ? (95,500) : false)
	dd[:r_real] = @df dtemp plot(:year, :r_real, linewidth = 2, color = "black", leg =false, title = "Real Land Rents",ylab = "1950=100", marker = mmark, ylims = fixy ? (95,500) : false)
	dd[:r_rho] = @df dtemp plot(:year, [:r, :ρ0],linewidth = 2, lab = [L"r" L"\rho_0" ],
	                           ylab = "1950=100",
							   title = "Rents and Land Value",
							   leg = :topleft,
							   marker = mmark, ylims = fixy ? (50,500) : false)

    dd[:q0_qr] = @df dtemp plot(:year, [:q0, :qr],linewidth = 2, lab = [L"q_0" L"q_r"],
	                           title = "Central and Fringe House Prices (1945=100)",
							   leg = :topleft,
							   marker = mmark, ylims = fixy ? (85,150) : false)
   dd[:q0_qr_qq1] = @df dtemp plot(:year, [:q0, :qr , :qq1],linewidth = 2, lab = [L"q_0" L"q_r" L"q_q1"],
	                           title = "Central and Fringe House Prices (1945=100)",
							   leg = :topleft,
							   marker = mmark, ylims = fixy ? (85,150) : false)
   dd[:qbar_100] = @df dtemp plot(:year, :qbar, linewidth = 2, color = "black", leg =false, title = "Average House Prices", marker = mmark, ylims = fixy ? (0.0,29.0) : false )

	# is missing density!
    # dd[:hr100] = @df dtemp plot(:year, :hr, linewidth = 2, color = "black", leg =false, title = "Fringe Housing Demand (1945=100)", marker = mmark)
    # dd[:Hr100] = @df dtemp plot(:year, :Hr, linewidth = 2, color = "black", leg =false, title = "Fringe Housing Supply (1945=100)", marker = mmark)



	ds2 = stack(select(d,:year,:qr, :ρr), Not(:year))

	d3  = @chain d begin
			select(:year,:θu, :θr)
			@transform(:theta_u = :θu, :theta_r = :θr) 
			select(:year,:theta_u, :theta_r)
	end

	# ds3 = stack(d3, Not(:year))
	# pl3 = @df ds3 plot(:year, :value, group = :variable,
	# 			      linewidth = 2, title = "Consumption",
	# 				  ylims = (0.0,3.5))

	ds3 = stack(d3, Not(:year))
	dd[:productivity] = @df ds3 plot(:year, :value, group = :variable,
					  linewidth = 2, title = "Log Productivity", ylims = fixy ? (0,20) : false, marker = mmark,
					  legend = :left, yscale = :log10)
	# ds4 = stack(select(d,:year,:ϕ), Not(:year))
	ds4 = select(d,:year,:cityarea)
	incphi = round(d1880.rel_cityarea[end] / d1880.rel_cityarea[1],digits = 1)

	dd[:phi] = @df d1880 plot(:year, :rel_cityarea,
					 linewidth = 2, title = "Rel City Area. 2010=$(round(d.rel_cityarea[i2010],digits=2))", color = "black",
					 leg = fixy ? :topleft : false, marker = mmark, annotate = (p.T[end],0.2*maximum(d.rel_cityarea),"$(round(incphi,digits=1))x"),
					 ylims = fixy ? (0.0,0.15) : false)


	dd[:phi_Lu] = @df stack(select(d1880, :year, :Lu_n => :population, :rel_cityarea_n => :rel_area), Not(:year)) plot(:year, :value, 
	                       group = :variable, leg = :left, linewidth = 2,
						   linecolor = ["orange" "red" ], marker = (:circle, ["orange" "red" ]),
						   yscale = :identity, formatter = y->string(round(Int,y)),
						   title = "Urban Population and Area (rel to rural)",
						   ylab = "data (1876) = model (1880) = 1")

	pad = copy(p.poparea_data) # will error - removed object.
	transform!(pad, :population => firstnorm => :population, 
	                :area => firstnorm => :area)
	dd[:phi_Lu_data] = scatter!(dd[:phi_Lu], pad.year, [pad.population, pad.area],
	                            marker = (:star5, ["orange" "red" ]), label = ["pop data" "area data"])					   
    
	dd[:Sr] = @df d plot(:year, :Sr,
				  linewidth = 2, color = "black",title = "Agricultural Land",
				  leg = false, marker = mmark, ylims = fixy ? (0.6,1.0) : false)
	
    dens = select(d,:year,:d0, :dr,:citydensity)
	df4 = dens
	ds4 = stack(dens, Not(:year))
	# ds4 = stack(select(df4,:year, :avgd), Not(:year))
	dd[:n_densities] = @df stack(select(d,:year,:d0_n, :dr_n, :avgd_n), Not(:year)) plot(:year, :value, group = :variable,
 					 linewidth = 2, title = "Normalized Densities", color = brg,
					 leg = :topright, ylims = fixy ? (0,300) : false)


    incdens = d.citydensity[i1870] / d.citydensity[i2010]
    diffdens = d.citydensity[i1870] - d.citydensity[i2010]
	ancdens = d.citydensity[end] + 0.2 * diffdens
	dd[:avdensity] = @df d plot(:year, :citydensity,
					 linewidth = 2, title = "Avg Density", marker = mmark,
					 legend = false,color = "black", ylims = fixy ? (0,200) : false, annotations = (p.T.stop,ancdens,"$(round(incdens,digits = 1))x"))

	dd[:densities] = @df stack(select(d,:year,:d0, :dr, :citydensity), Not(:year)) plot(:year, :value, group = :variable,
					 linewidth = 2, title = "Densities", leg = :topright, ylims = fixy ? (0,300) : false,
					 annotations = (p.T[2],0.2*maximum(d.d0),"$(round(incdens,digits = 2))x"))


    dss = stack(select(d,:year,:mode0,:modeϕ,:imode), Not(:year))
	facs = combine(groupby(dss,:variable),:value => (x -> x[end]/x[1]) => :factor)
	labs = String.(facs.variable) .* " : " .* string.(round.(facs.factor,digits=1))  .* "x"

	dss = [d[!,:year] mapcols(x -> x ./ x[1],select(d,:mode0,:modeϕ,:imode,:ctime0,:ctimeϕ,:ictime))]
	rename!(dss, :x1 => :year)
	dmode = stack(select(dss,:year,:mode0,:modeϕ,:imode) , Not(:year))

	dd[:mode] = @df dmode plot(:year, :value, group = :variable,
					 linewidth = 2, title = "mode increase", leg = :left, ylims = fixy ? (0,1.4) : false , marker = mmark)
    # dss = stack(select(d,:year,:ctime0,:ctimeϕ,:ictime), Not(:year))
	# facs = combine(groupby(dss,:variable),:value => (x -> x[end]/x[1]) => :factor)
	# labs = String.(facs.variable) .* " : " .* string.(round.(facs.factor,digits=1))  .* "x"

	dtime = stack(select(dss,:year,:ctime0,:ctimeϕ,:ictime) , Not(:year))
	dd[:ctime] = @df dtime plot(:year, :value, group = :variable,
					 # linewidth = 2, title = "commute time", leg = :topleft, labels = reshape(labs,1,3) )
					 linewidth = 2, title = "ctime increase", leg = :topleft, marker = mmark)

	# distribution of modes chosen in each year to get a view of how many people choose which mode in which year
	z = select(d, :iDensity, :iDensitySpeeds , :year, [:iDensity, :iDensitySpeeds] => ((a,b) -> b ./ a) => :SpeedShares)
	z = hcat(select(d,:year), DataFrame(hcat(Array(select(z, :SpeedShares))...)',[:walk, :bus, :car]))
	sy = stack(z, Not(:year))
	dd[:speedshares] = @df sy groupedbar(:year, :value, group = :variable)

	# plot(pl,pl2,pl3,pl4,pl5, layout = (1,5),size = (700,250))
	# plot(pl2,pl6,pl4,pl7 ,layout = (1,4),size = (900,250))
	dd
end


function plot_i0k(di::Dict;it = 19)

	p0 = Param(par = di, use_estimatedθ = false)

	x,M,p = run(p0, estimateθ = false)
	pl = ts_plots(M,p0,fixy = false)
	pc = cs_plots(M[it], p0, it)

						# multi country	
	x,C,p = runk(par = merge(Dict(:K => 2, :kshare => [0.5,0.5], :factors => [1.0,1.05]),di))
	x = impl_plot_slopes(C)
	pl1 = plot(x[3])

	pl2 = plot(pl[:Lr_data],pl[:spending],pl[:pr_data],pl[:productivity],
						     pl[:n_densities], pl[:densities], pl[:mode], pl[:ctime],
							 pl[:phi] , pl[:qbar_real], pl[:r_y], pl[:r_rho],
							 pc[:ϵ] , pc[:D], pc[:q] , pc[:H],
							 layout = (4,4))
	plot(pl2, pl1, size = (1600,700), layout = @layout [a{0.7w} b{0.3w}])	
end
