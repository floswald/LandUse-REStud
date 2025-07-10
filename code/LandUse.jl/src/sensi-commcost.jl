# functions related to experiments on commuting costs


"""
# Commuting Cost Wage elasticity

We set the elasticity of commuting costs to income or distance, ``\\xi_w``, ``\\xi_l`` or both to unity.
This means that the fraction of income devoted to commuting does not fall
over time as income increases.
"""
function sensitivity_ξ(; save = false, readdisk = true, returnplots = true)
    pth = joinpath(dbplots(), "revision$(revision())","sensitivity-commutecost")
    mkpath(pth)

	if !readdisk
        d = sensitivity_ξw_()
        JLD2.jldsave(joinpath(pth,"data.jld2"); d)
    else
        d = JLD2.load_object(joinpath(pth,"data.jld2"))
    end

	# checking
	# za = Dict( k => subset(v, :year => x -> x .< 2030) for (k,v) in d)
	# ch = Dict()
	# ch[:θr] = scatter( za[Symbol("ξw=1")].θr, za[Symbol("ξw=1 (new θu,θr)")].θr, xlab = "ξw=1", ylab = "ξw=1 (new θu,θr)", title = "θr")
	# ch[:θu] = scatter( za[Symbol("ξw=1")].θu, za[Symbol("ξw=1 (new θu,θr)")].θu, xlab = "ξw=1", ylab = "ξw=1 (new θu,θr)", title = "θu")
	# ch[:area] = scatter( za[Symbol("ξw=1")].cityarea, za[Symbol("ξw=1 (new θu,θr)")].cityarea, xlab = "ξw=1", ylab = "ξw=1 (new θu,θr)", title = "cityarea")
	# ch[:ρr] = scatter( za[Symbol("ξw=1")].ρr, za[Symbol("ξw=1 (new θu,θr)")].ρr, xlab = "ξw=1", ylab = "ξw=1 (new θu,θr)", title = "ρr")

    if !returnplots

	   return d
    else
    
        p,papp,agg,changes = sensitivity_ξw_plot(d)

        dout = select(@subset(d[:Baseline] , :year .< 2030), :region, :year, :citydensity, :Lu, :density_data)
        b1870 = select(@subset(dout, :year .== 1870), :region, :Lu => :Lu_base_1870)
        b2020 = select(@subset(dout, :year .== 2020), :region, :Lu => :Lu_base_2020)
        cfout = select(@subset(d[Symbol("ξw=1")] , :year .< 2030), :region, :year, :citydensity => :citydensity_cf, :Lu => :Lu_cf)
        cf1870 = select(@subset(cfout, :year .== 1870), :region, :Lu_cf => :Lu_cf_1870)
        cf2020 = select(@subset(cfout, :year .== 2020), :region, :Lu_cf => :Lu_cf_2020)
        dout = @chain dout begin
                innerjoin(_, cfout, on = [:region, :year])
                innerjoin(_, b1870, on = :region)
                innerjoin(_, cf1870, on = :region)
                innerjoin(_, b2020, on = :region)
                innerjoin(_, cf2020, on = :region)
            end
        CSV.write(joinpath(pth, "data.csv"), dout)

        aout = @chain agg[:Baseline] begin
            select(:year, "BAD_mean_avgd_n_1840",["mean_citydensity$(i)" for i in ["","_1840","_1870","_Lu_1870","_Lu_2020","_Lu_1870_1840","_Lu_2020_1840"]])
            innerjoin(select(agg[Symbol("ξw=1")], :year, "BAD_mean_avgd_n_1840" => "BAD_mean_avgd_n_1840_cf",["mean_citydensity$(i)" => "mean_citydensity$(i)_cf" for i in ["","_1840","_1870","_Lu_1870","_Lu_2020","_Lu_1870_1840","_Lu_2020_1840"]]), on = :year)
            @aside CSV.write(joinpath(pth, "data-aggregated.csv"), _)
        end

        if save

            for (k,v) in p
                savefig(v, joinpath(pth,"$k.pdf"))
            end
            for (k,v) in papp
                savefig(v, joinpath(pth,"appendix-$k.pdf"))
            end
            # for (k,v) in ch
            # 	savefig(v, joinpath(pth,"checking-$k.pdf"))
            # end
        end
        # p,papp, dout, aout
        p,papp,changes
    end
end



function sensitivity_ξw_()
    d = khet_run(readdisk = true)
    OrderedDict(:Baseline => d, 
	     Symbol("ξw=1") => ξ_stepper(ξw = true, ξl = false),
         Symbol("ξw=ξl=1") => ξ_stepper(ξw = true, ξl = true))
        #  Symbol("ξl=1") => ξ_stepper(ξw = false, ξl = true,maxT = 2020))
	    #  Symbol("ξw=1 (new θu,θr)") => ξ_stepper(estimateθu = true,estimateθr = true))
		#  Symbol("ξw=1 prime") => khet_estimate_impl(20,par = Dict(:ξw => 1.0),iterateP = 2, estimateθr = true))
end



# goes until 2020
function ξ_stepper(; K = 20, newξ = 1.0, maxT = 2350, estimateθu = false, estimateθr = false, ξw = true, ξl = false, nsteps = 5)
	@info "ξ stepper. (estimateθu,estimateθr) = ($estimateθu,$estimateθr)"

	θrdf = khet_run(readdisk = true)  # get baseline results 
	# p = Param(par = Dict(σ => 0.5))
	p = Param(par = Dict(:T => 1840:10:maxT,:gsr => zeros(K),:gs => zeros(K),:kshare => [1/K for i in 1:K],:factors => ones(K),:K => K,:𝕊 => zeros(60,K), :a => 1.1))

	C = HetCountry[]
	edatas = DataFrame[]  # collect estimation input data along the way even if we dont' estiamte the thetas
	# try to get first period solution from baseline

	
	d0 = @chain θrdf begin
		subset(:year => x -> x .== minimum(x))  # period == 1
		sort(:region)  # just to make sure
		select(:region,:r,:pr,:Sr,:Lu,:Lr,:θu,:θr, :ϕ, :Srh)
	end
 
	sols = NamedTuple[]
	
	x0 = (Sr = d0.Sr,
		  Lu = d0.Lu,
		  Lr = d0.Lr,
		  r = unique(d0.r)[1],
		  pr = unique(d0.pr)[1])
	push!(sols, x0)
	
    if ξw && (!ξl) # default
        ξws = range(0.75,newξ, length = nsteps)
    elseif ξw && ξl
        ξws = range(0.75,newξ, length = nsteps)
        ξls = range(0.5,newξ, length = nsteps)
    elseif (!ξw) && ξl  
        ξls = range(0.5,newξ, length = nsteps)
    else
        error("invalid")
    end

	# evaluate system in period 1
	for i in 1:nsteps

        if ξw && (!ξl) # default
            p.ξw = ξws[i]
        elseif ξw && ξl
            p.ξw = ξws[i]
            p.ξl = ξls[i]
        elseif (!ξw) && ξl  
            p.ξl = ξls[i]
        end

		@debug "ξw, ξl = " p.ξw p.ξl

		xx = nlsolve((x,y) -> system!(x,y,p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr),nt2x(sols[i]), autodiff = :forward)
		push!(sols, x2nt(xx.zero,K))
		converged(xx) || error("not converged ξw=$(p.ξw)")
	end

	years = sort(unique(θrdf.year))

	ϕs = zeros(p.K)

	prog = Progress(length(p.T), desc = "ξw Country Model", barglyphs=BarGlyphs("[=> ]"), barlen = 50, color = :green) 
	for it in 1:length(p.T)
		@debug it
		setperiod!(p,it)
		c = HetCountry(p) 
		push!(edatas, areaprice_estim(p))

        for ik in 1:p.K
            idx = (θrdf.it .== it) .&  (θrdf.region .== ik)
            # println("k = $ik, t = $it, idx = $idx")
            c.pp[ik].θr = θrdf.θr[idx][1]
            c.pp[ik].θu = θrdf.θu[idx][1]
        end

		
		if (it < 11 || it >= 20) #|| (estimateθu && estimateθr)
			d0 = @chain θrdf begin
				subset(:year => ieq(years[it]))  # period == it
				sort(:region)  # just to make sure
				select(:region,:r,:pr,:Sr,:Lu,:Lr,:θu,:θr, :ϕ, :Srh)
			end
            if !(estimateθu && estimateθr)
                xx = nlsolve((x,y) -> system!(x,y,p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr),nt2x(sols[end]), autodiff = :forward)
                 # returns named tuple
                aux = system!(zeros(5K),xx.zero,p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr)
                if maximum(abs,aux.Lu - aux.iDensity) > 1e-5
                    @warn "incorrect solution"
                end
                # @infiltrate
			    push!(sols, aux)
                ϕs[:] = aux.ϕ  # store phis
            else
                xx = jc(c,sols[end], estimateθr = estimateθr && it < 20,estimateθu = estimateθu && it < 20)
			    push!(sols, xx[1])
                # println(xx[1].U)
                ϕs[:] = xx[2]  # store phis

            end
		else
			@info "turning on 4x4 drive 🚚" maxlog=1
			# take convex combinations of thetau and thetar between both time periods 
            # d0.θu[1] = d0.θu[1] * 0.95
			θu0 = d0.θu  # last period
			θr0 = d0.θr  # last period
			d0 = @chain θrdf begin
				subset(:year => ieq(years[it]))  # period == it
				sort(:region)  # just to make sure
				select(:region,:r,:pr,:Sr,:Lu,:Lr,:θu,:θr, :ϕ, :Srh)
			end
            # d0.θu[1] = d0.θu[1] * 0.95

            tvals = if it > 12
                # [range(0.01, 0.95, 20)..., range(0.95, 1, 100)...]
                range(0.0, 1, 10)
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
                xx = jc(c,sols[end], estimateθr = estimateθr,estimateθu = estimateθu )
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
		update!(c,sols[end],estimateθ = false)
		push!(C,c)
        next!(prog)

	end
	post_proc(C,edatas)
end



function sensitivity_ξw_plot(d::OrderedDict)
    a = OrderedDict( k => @chain aggregator(v) begin
            subset(:year => leq(2020))
            end 
        for (k,v) in d)

    # report mean city in first and last year
    for (k,v) in a
        ia = @chain v begin
            subset(:year => ieq(2010))
        end
        @info "avg city size $(k) in 2010 = $(ia.rel_cityarea)"
        ia = @chain v begin
            subset(:year => ieq(1840))
        end
        @info "avg city size $(k) in 1840 = $(ia.rel_cityarea)"
    end
    println()

    def_theme()
    
    sys = setdiff(collect(keys(a)), [:Baseline])
    
    pvars = [:commuting_speed_1840,:citydensity_1840,:HPI_1840, :ρr, :cityarea_agg_1840]
    ticks = Dict(:commuting_speed_1840 => 1:0.5:5,
                 :citydensity_1840 => [1, 0.5, 0.25,0.1],
				 :cityarea_agg_1840 => [1,10,25,50,90],
                 :HPI_1840 => :auto,
                 :ρr => [0.1,0.15,0.2,0.3]
                 )
    scales = Dict(:commuting_speed_1840 => :identity,
	:cityarea_agg_1840 => :log10,
                  :citydensity_1840  => :log10,
                  :ρr      => :log10,
                  :HPI_1840 => :identity
                  )
    legs = Dict(:commuting_speed_1840 => :topleft,
	:cityarea_agg_1840 => :topleft,
                  :citydensity_1840          => :bottomleft,
                  :ρr          => :topleft,
                  :HPI_1840        => :topleft
                  )                  
    lims = Dict(:commuting_speed_1840 => (0.8,5),
	:cityarea_agg_1840 => (0,53),
                  :citydensity_1840          => (0.1,1.1),
                  :ρr          => (0.1,0.35),
                  :HPI_1840        => (0,Inf)
                    )
    cold = Dict(:commuting_speed_1840 => reds(),
	:cityarea_agg_1840 => golds(),
                :citydensity_1840          => reds(),
                :ρr          => greens(),
                :HPI_1840        => reds()
                )
    namepref = Dict(:commuting_speed_1840 => "mean_",
	:cityarea_agg_1840 => "",
                :citydensity_1840          => "mean_",
                :ρr          => "mean_",
                :HPI_1840        => "mean_"
                )
    p = Dict()
    papp= Dict()  # appendix plots

    mshapes = [:none, :diamond]
    for ip in pvars
        
        ss = Symbol("$(namepref[ip])$(ip)")

        p[ip] = plot(a[:Baseline].year, a[:Baseline][!,ss], label = "Baseline", color = cold[ip][1],yscale = scales[ip], yticks = ticks[ip],
        yformatter = ((ip == :HPI_1840)) ? x -> string.(floor(Int,x)) : x -> string.(round(x,digits = 2)),
        ylims = lims[ip],linestyle = :dot, size = panelsizef(),leg = legs[ip], xticks = 1840:20:2020)
        # yscale!(p[ip], scales[ip])

        @df a[sys[1]] plot!(p[ip],:year, cols(ss), label = "$(sys[1])", color = cold[ip][2],ylims = lims[ip], linestyle = :solid, markershape = :circle)

        papp[ip] = deepcopy(p[ip])

        if length(sys) > 1
            for isys in 2:length(sys)
                @df a[sys[isys]] plot!(papp[ip],:year, cols(ss), label = "$(sys[isys])", color = cold[ip][1+isys],ylims = lims[ip], linestyle = :solid, markershape = mshapes[isys-1])
            end
        end

    end

    # retro-fit :cityarea_agg_1840 with population
    ddd = a[:Baseline]
    bbb = a[sys[1]]
    blab = "Urban Pop Baseline"
    plab = string("Urban Pop ",sys[1])

    p[:area_pop] = deepcopy(p[:cityarea_agg_1840])

    plot!(p[:area_pop], ddd.year, ddd.Lu_agg_1840, color = reds()[1], ls = :dot, label = blab)
    plot!(p[:area_pop], bbb.year, bbb.Lu_agg_1840, color = reds()[2], ls = :solid,label = plab)

    # print avg_density
    bs2020 = subset(a[:Baseline], :year => ieq(2020))
    aggdensity = agg_pop_density()


    @info "Reporting Ratios: (dens(1870,cf) / dens(2020,cf)) / (dens(1870,base) / dens(2020,base))"

    changes = Dict()

    for isys in 1:length(sys)
        println()
        @info "Scenario: $(sys[isys])"
        println()
        p2020 = subset(a[sys[isys]], :year => ieq(2020))

        changes[sys[isys]] = 
        Dict(
            :baseline => change_reporter( 1 , bs2020.mean_citydensity_1870[1] ),
            :cf => change_reporter( 1 , p2020.mean_citydensity_1870[1] ),
            :data => change_reporter( 1 , aggdensity[aggdensity.year .== 2015,:agg_density][1] ))
        
        @info "log (cf1870/cf2020) / log (base1870/base2020): $(round(changes[sys[isys]][:cf][:log] / changes[sys[isys]][:baseline][:log], digits = 3))"
        @info "(cf1870/cf2020) / (base1870/base2020): $(round(changes[sys[isys]][:cf][:ratio] / changes[sys[isys]][:baseline][:ratio], digits = 3))"
        @info "log (cf1870/cf2020) / log (data1870/data2020): $(round(changes[sys[isys]][:cf][:log] / changes[sys[isys]][:data][:log], digits = 3))"
        @info "(cf1870/cf2020) / (data1870/data2020): $(round(changes[sys[isys]][:cf][:ratio] / changes[sys[isys]][:data][:ratio], digits = 3))"

    end
    p,papp,a, changes


end