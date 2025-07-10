


"""
	i0()

interact with the baseline single city version of the model.
This basically calls [`dashboard(M::Vector{Region},p::Param,i::Int)`](@ref LandUse.dashboard) after creating `p` from a set of user inputs.
"""
function i0()
	xis = 0.0:0.01:1.5
	cbars = 0.0:0.01:1.5
	as = 0.0:0.01:5.0
	psis = 0.1:0.01:1.0
	p1 = Param()

	# mp = @manipulate for est in OrderedDict("from data θ" => 1, "estimate θ" => 2, "from estimation θ" => 3),
	mp = @manipulate for it in slider(1:length(p1.T), value = length(p1.T), label = "period") |> onchange,
					ϕx in slider(0.05:0.05:1.0, value = p1.ϕ1x, label = "ϕ1x") |> onchange,
					cbar in slider(cbars, value = p1.cbar, label = "cbar") |> onchange,
					sbar in slider(cbars, value = p1.sbar, label = "sbar") |> onchange,
					gamma in slider(psis, value = p1.γ, label = "gamma") |> onchange,
					a in slider(as, value = p1.a, label = "a") |> onchange,
					es in slider(1.0:0.1:3.0, value = p1.ϵs	, label = "ϵs") |> onchange,
					xil in slider(xis, value = p1.ξl, label = "ξl") |> onchange,
					xiw in slider(xis, value = p1.ξw, label = "ξw") |> onchange,
					eta in slider(0.0:0.01:0.1, value = 0.0, label = "η-agglo") |> onchange,
					etaa in slider(0.0:0.01:0.1, value = 0.0, label = "η-congest") |> onchange,
					d1 in slider(0.0:0.0001:0.1, value = 0.0, label = "d1") |> onchange,
					d2 in slider(0.0:0.01:3.0, value = 0.0, label = "d2") |> onchange,
					flat in checkbox(label = "flat ϵ?")

					p0 = LandUse.Param(par = Dict(:ξl => xil, :ξw => xiw,
											  :cbar => cbar, :sbar => sbar, :a => a, :γ => gamma, :ϵs => es,
											  :ϕ1x => ϕx, :ϵflat => flat, :η => eta, :ηa => etaa, :d1 => d1, :d2 => d2),
									   use_estimatedθ = false)

					# try
						x,M,p = LandUse.run(p0, estimateθ = false)
						# pl = LandUse.ts_plots(M,p0,fixy = false)
						# pc = LandUse.cs_plots(M[it], p0, it)
						dashboard(M,p0,it)
						# plot(pl[:Lr_data],pl[:spending],pl[:pr_data],pl[:productivity],
						#      pl[:n_densities], pl[:densities], pl[:mode], pl[:ctime],
						# 	 pl[:phi] , pl[:qbar_real], pl[:r_y], pl[:r_rho],
						# 	 pc[:ϵ] , pc[:D], pc[:q] , pc[:H],
						# 	 layout = (4,4),size = (1200,800))
					# catch e
					# 	wdg = alert("Error!")
					# 	print(wdg())
					# end

					# plot(pl[:phi], pl[:avdensity],pl[:mode],pl[:ctime],pl[:dist_vs_time],plot(), l = (2,3))
					# plot(pl[:Lr_data],pl[:spending],pl[:qbar_real],pl[:productivity],pl[:n_densities], pl[:avdensity], layout = (2,3),link = :x)

	end
	@layout! mp vbox(hbox(:it, :ϕx , :flat, :eta, :etaa),
	                 hbox(:sbar, :cbar,:gamma,:es),
	                 hbox(:a, :xil, :xiw, :d1, :d2),
					 observe(_))
	w = Window()
	body!(w,mp)
end

"""
	iobj()

Similar to [`i0`](@ref) but prints the value of the [`objective`](@ref) function into one of the panels.
"""
function iobj()
	eta = 0.5:0.01:2.0
	tau = 0.1:0.1:1.0
	eta2 = 0.0:0.01:1.0
	cbars = 0.0:0.01:1.5
	ctaus = 0.0:0.01:5.0
	psis = 0.1:0.01:1.0
	p1 = Param()

	# mp = @manipulate for est in OrderedDict("from data θ" => 1, "estimate θ" => 2, "from estimation θ" => 3),
	mp = @manipulate for it in slider(1:length(p1.T), value = length(p1.T), label = "period") |> onchange,
					cbar in slider(cbars, value = p1.cbar, label = "cbar") |> onchange,
					sbar in slider(cbars, value = p1.sbar, label = "sbar") |> onchange,
					gamma in slider(psis, value = p1.γ, label = "gamma") |> onchange,
					cτ in slider(ctaus, value = p1.a, label = "cτ") |> onchange,
					es in slider(1.0:0.1:3.0, value = p1.ϵs	, label = "ϵs") |> onchange,
					etam in slider(eta, value = p1.ηm, label = "ηm") |> onchange,
					etal in slider(eta2, value = p1.ηl, label = "ηl") |> onchange,
					etaw in slider(eta2, value = p1.ηw, label = "ηw") |> onchange

					p0 = LandUse.Param(par = Dict(:ηm => etam, :ηl => etal, :ηw => etaw,
											  :cbar => cbar, :sbar => sbar, :cτ => cτ, :γ => gamma, :ϵsmax => es, :ϵs => es,
											  :ϕ1x => 0.15),
									   use_estimatedθ = false)

					o = objective(p2x(p0), moments = true, plot = true)

					o[3]

					# plot(pl[:phi], pl[:avdensity],pl[:mode],pl[:ctime],pl[:dist_vs_time],plot(), l = (2,3))
					# plot(pl[:Lr_data],pl[:spending],pl[:qbar_real],pl[:productivity],pl[:n_densities], pl[:avdensity], layout = (2,3),link = :x)

	end
	@layout! mp vbox(hbox(:it, :ϕx),
	                 hbox(:sbar, :cbar,:gamma,:es),
	                 hbox(:cτ, :etam, :etal, :etaw),
					 observe(_))
	w = Window()
	body!(w,mp)
end

function ik3()
	p1 = Param()
	gfac = 1.00:0.01:2.0
	xis = 0.0:0.01:1.0
	cbars = 0.0:0.01:1.5
	as = 0.0:0.01:5.0
	psis = 0.1:0.01:1.0
	mp = @manipulate for it in slider(1:length(p1.T), label = "it"), 
		cbar in slider(cbars, value = p1.cbar, label = "cbar") |> onchange,
		sbar in slider(cbars, value = p1.sbar, label = "sbar") |> onchange,
		gamma in slider(psis, value = p1.γ, label = "gamma") |> onchange,
		a in slider(as, value = p1.a, label = "a") |> onchange,
		xil in slider(xis, value = p1.ξl, label = "ξl") |> onchange,
		xiw in slider(xis, value = p1.ξw, label = "ξw") |> onchange,
		eta in slider(0.0:0.01:0.1, value = 0.0, label = "η-agglo") |> onchange,
		d1 in slider(0.0:0.001:0.1, value = 0.01, label = "d1") |> onchange,
		d2 in slider(0.0:0.01:2.0, value = 1.5, label = "d2") |> onchange,
		etaa in slider(0.0:0.01:0.1, value = 0.0, label = "η-congest") |> onchange

		# try
			x,C,p = runk(par = Dict(:K => 20, :kshare => [1/20 for i in 1:20], :factors => ones(20), :gs => zeros(20),
									:ξl => xil, :ξw => xiw,
									:cbar => cbar, :sbar => sbar, 
									:a => a, :γ => gamma, :η => eta, :ηa => etaa, :d1 => d1, :d2 => d2), estimateθ = true)
			d = dataframe(C)
			p0 = @df select(subset(d, :year => x->x.== 2020), :year, :Lu, :citydensity => LandUse.firstnorm => :fn, :region) bar(:fn,xticks = ([1,2,3],["Paris","Lyon","Marseille"]), ylab = "rel density", title = "year 2020")
			p01 = @df select(subset(d, :year => x->x.== 2020,  :region => x->x.> 1 ), :year, :Lu, :citydensity => LandUse.firstnorm => :fn, :region) bar(:fn,xticks = ([1,2,3],["Paris","Lyon","Marseille"]), ylab = "rel density", title = "year 2020")
			pl = dashk20(d)
			dd = select(subset(d, :year => x->x.== 2020, :region => x->x.>1), :year, :Lu, :citydensity, :region)
			xx = lm(@formula( log(citydensity) ~ log(Lu)), dd)
			plot(pl[:relpop],pl[:avg_density], bar([coef(xx)[2]],ylims = (0,1)), p0, layout = (2,2), size = (800,500))
		# catch e
		# 	wdg = alert("Error!")
		# 	print(wdg())
		# end
	end
	@layout! mp vbox(hbox(:eta, :etaa),
	                 hbox(:sbar, :cbar,:gamma,:es),
	                 hbox(:a, :xil, :xiw, :it, :d1, :d2),
					 observe(_))
	w = Window()
	body!(w,mp)
end

function ik2()
	p1 = Param()
	gfac = 1.00:0.01:2.0
	xis = 0.0:0.01:1.0
	cbars = 0.0:0.01:1.5
	as = 0.0:0.01:5.0
	psis = 0.1:0.01:1.0
	mp = @manipulate for it in slider(1:length(p1.T), label = "it"), 
		cbar in slider(cbars, value = p1.cbar, label = "cbar") |> onchange,
		sbar in slider(cbars, value = p1.sbar, label = "sbar") |> onchange,
		gamma in slider(psis, value = p1.γ, label = "gamma") |> onchange,
		a in slider(as, value = p1.a, label = "a") |> onchange,
		es in slider(1.0:0.1:3.0, value = p1.ϵs	, label = "ϵs") |> onchange,
		xil in slider(xis, value = p1.ξl, label = "ξl") |> onchange,
		xiw in slider(xis, value = p1.ξw, label = "ξw") |> onchange,
		eta in slider(0.0:0.01:0.1, value = 0.0, label = "η-agglo") |> onchange,
		etaa in slider(0.0:0.01:0.1, value = 0.0, label = "η-congest") |> onchange,
		g1 in slider(0.9:0.01:1.0, value = 1.0, label = "g1") |> onchange,
		f1 in slider(0.9:0.01:1.1, value = 1.0, label = "f1") |> onchange,
		g2 in slider(gfac, value = 1.2, label = "g2") |> onchange

		try
			x,C,p = runk(par = Dict(:K => 2, :kshare => [1/2,1/2], :factors => [g1,g2],:factor1 => f1,
									:ξl => xil, :ξw => xiw,
									:cbar => cbar, :sbar => sbar, 
									:a => a, :γ => gamma, :ϵs => es, :η => eta, :ηa => etaa))
			dashboard(C, it)
		catch 
			wdg = alert("Error!")
			print(wdg())
		end
	end
	@layout! mp vbox(hbox(:g1,:g2, :f1, :eta, :etaa),
	                 hbox(:sbar, :cbar,:gamma,:es),
	                 hbox(:a, :xil, :xiw, :it),
					 observe(_))
	w = Window()
	body!(w,mp)
end

"""
	i0k()

interact with the 2 city model.
"""
function i0k()
	gfac = 1.00:0.05:2.0
	eta = 0.5:0.01:2.0
	tau = 0.1:0.1:1.0
	eta2 = 0.0:0.01:1.0
	cbars = 0.0:0.01:1.5
	ctaus = 0.0:0.01:6.0
	psis = 0.1:0.01:1.0

	p1 = Param()

	# mp = @manipulate for est in OrderedDict("from data θ" => 1, "estimate θ" => 2, "from estimation θ" => 3),
	mp = @manipulate for it in slider(1:length(p1.T), value = length(p1.T), label = "period"),
					ϕx in slider(0.05:0.05:1.0, value = 1.0, label = "ϕ1x"),
					cbar in slider(cbars, value = p1.cbar, label = "cbar"),
					sbar in slider(cbars, value = p1.sbar, label = "sbar"),
					gamma in slider(psis, value = p1.γ, label = "gamma"),
					cτ in slider(ctaus, value = p1.a, label = "cτ"),
					es in slider(0.0:0.1:10.0, value = p1.ϵs	, label = "ϵs"),
					etam in slider(eta, value = p1.ηm, label = "ηm"),
					etal in slider(eta2, value = p1.ηl, label = "ηl"),
					g2 in slider(1.0:0.01:1.1, value = 1.05, label = "g2"),
					etaw in slider(eta2, value = p1.ηw, label = "ηw")

					p0 = LandUse.Param(par = Dict(:ηm => etam, :ηl => etal, :ηw => etaw,
											  :cbar => cbar, :sbar => sbar, :cτ => cτ, :γ => gamma, :ϵsmax => es, :ϵs => es,
											  :ϕ1x => ϕx),
									   use_estimatedθ = false)

					try
						x,M,p = LandUse.run(p0, estimateθ = false)
						pl = LandUse.ts_plots(M,p0,fixy = false)
						pc = LandUse.cs_plots(M[it], p0, it)

						# multi country	
						x,C,p = runk(par = Dict(:K => 2, :kshare => [0.5,0.5], :factors => [1.0,g2],
												:ηm => etam, :ηl => etal, :ηw => etaw,
													:cbar => cbar, :sbar => sbar, :cτ => cτ, :γ => gamma, :ϵsmax => es))
						x = impl_plot_slopes(C)
						pl1 = plot(x[1])

						pl2 = plot(pl[:Lr_data],pl[:spending],pl[:pr_data],pl[:productivity],
						     pl[:n_densities], pl[:densities], pl[:mode], pl[:ctime],
							 pl[:phi] , pl[:qbar_real], pl[:r_y], pl[:r_rho],
							 pc[:ϵ] , pc[:D], pc[:q] , pc[:H],
							 layout = (4,4))
						plot(pl2, pl1, size = (1600,700), layout = @layout [a{0.7w} b{0.3w}])
					catch e
						wdg = alert("Error!")
						print(wdg())
					end

					# plot(pl[:phi], pl[:avdensity],pl[:mode],pl[:ctime],pl[:dist_vs_time],plot(), l = (2,3))
					# plot(pl[:Lr_data],pl[:spending],pl[:qbar_real],pl[:productivity],pl[:n_densities], pl[:avdensity], layout = (2,3),link = :x)

	end
	@layout! mp vbox(hbox(:it, :ϕx, :g2),
	                 hbox(:sbar, :cbar,:gamma,:es),
	                 hbox(:cτ, :etam, :etal, :etaw),
					 observe(_))
	w = Window()
	body!(w,mp)
end


function interact1()
	K = 2
	es = 1.0:0.5:10.0
	g2 = 1.0:0.01:1.06

    # mp = @manipulate for θr in slider(θrs, label = "θr"), θu in slider(θus, label = "θu")
    mp = @manipulate for e in slider(es, label = "ϵr", value = 4.0 ),
		                 g in slider(g2, label = "growth2", value = 1.0)
		x = LandUse.issue12_1(e; gf = [1.0,g])
		x[5]
    end
end

function interact2()
	K = 2
	es = 1.0:0.5:10.0
	g2 = 1.0:0.01:1.06

    # mp = @manipulate for θr in slider(θrs, label = "θr"), θu in slider(θus, label = "θu")
    mp = @manipulate for e in slider(es, label = "ϵr", value = 4.0 ),
		                 g in slider(g2, label = "growth2", value = 1.01)
		x = LandUse.issue12_1(e; gf = [1.0,g])
		x[5]
    end
end


function imulti3()
	g1 = 1.18:0.01:1.22
	esl = 0.0:20:400.0
	es = 1.0:0.5:10.0
	esm = 1.0:0.5:10.0
	zetas = 0.0:0.1:0.6
	t1s = 0.0:0.1:1.0
	t2s = 0.5:0.1:2.0

	K = 3
	cpar = Dict(:S => 1.0, :L => 1.0,
				:K => K,
				:kshare => [1/K for i in 1:K])


	mp = @manipulate for
		                 # esl in slider(esl,label = "eslope",value = 0.0),
		                 # gr in slider(g1,label = "gr", value = 4.0),
		                 gs1 in slider(g1,label = "u-growth 1",value = 1.19),
		                 gs2 in slider(g1,label = "u-growth 2",value = 1.2),
		                 gs3 in slider(g1,label = "u-growth 3",value = 1.21),
						 etam in slider(t2s, label = "ηm", value = 1.0),
						 etal in slider(t1s, label = "ηl", value = 0.0)
						 # z in slider(zetas,label = "ζ", value = 0.0)

						 d0 = Dict(:ηm => etam, :ηl => etal)

						 dd = Dict(1 => merge(Dict(:θut=> 1.0, :θrt=> 1.0,:θu_g => gs1,  :θr_g => 1.2), d0),
						 		   2 => merge(Dict(:θut=> 1.0, :θrt=> 1.0,:θu_g => gs2, :θr_g => 1.2),d0),
								   3 => merge(Dict(:θut=> 1.0, :θrt=> 1.0,:θu_g => gs3,  :θr_g => 1.2),d0)
								   )

   					    sols,C,cp,pp = LandUse.runk(cpar = cpar,par = dd)
   					    p1 = plot(impl_plot_ts_all(C)..., layout = (2,2))
   					    p2 = impl_plot_slopes(C)

						p1
	# 					p1
	end
end

function imulti2()
	g1 = 1.0:0.01:1.1
	esl = 0.0:20:400.0
	es = 1.0:0.5:10.0
	esm = 1.0:0.5:10.0
	zetas = 0.0:0.1:0.6
	t1s = 0.5:0.1:1.0

	K = 2
	cpar = Dict(:S => 1.0, :L => 1.0,
				:K => K,
				:kshare => [1/K for i in 1:K])


	mp = @manipulate for e in slider(es, label = "ϵr", value = 4.0 ),
		                 esl in slider(esl,label = "eslope",value = 0.0),
		                 # gr in slider(g1,label = "gr", value = 4.0),
		                 gs1 in slider(g1,label = "u-shift 1",value = 1.0),
		                 gs2 in slider(g1,label = "u-shift 2",value = 1.0),
		                 # gs3 in slider(g1,label = "u-shift 3",value = 1.0),
						 t1 in slider(t1s, label = "τ1", value = 0.9),
						 z in slider(zetas,label = "ζ", value = 0.0)

						 d0 = Dict(:ζ => z, :τ1 => t1, :ϵr => e, :ϵsmax => esl)

						 dd = Dict(1 => merge(Dict(:θut=> gs1, :θrt=> 1.0,:θu_g => 1.2,  :θr_g => 1.2), d0),
						 		   2 => merge(Dict(:θut=> gs2, :θrt=> 1.0,:θu_g => 1.2, :θr_g => 1.2),d0)
								   # 3 => merge(Dict(:θut=> gs3, :θrt=> 1.0,:θu_g => 1.2,  :θr_g => 1.2),d0)
								   )

   					    sols,C,cp,pp = LandUse.runk(cpar = cpar,par = dd)
   					    p1 = plot(impl_plot_ts_all(C)..., layout = (2,2))
   					    p2 = impl_plot_slopes(C)

						plot(p1,p2,layout = (1,2))
	# 					p1
	end
end
