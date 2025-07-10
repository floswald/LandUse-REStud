


function hetgrowth(;save = false,readdisk = false)
    pth = joinpath(dbplots(), "revision$(revision())","sensitivity-hetgrowth")
    mkpath(pth)
    if !readdisk
        d = arti_run()
        JLD2.jldsave(joinpath(pth,"data.jld2"); d)
    else
        d = JLD2.load_object(joinpath(pth,"data.jld2"))
    end
    pl,plgrey = arti_plot(d)

    if save
        for (k,v) in pl
			savefig(v, joinpath(pth,"$k.pdf"))
        end
        for (k,v) in plgrey
			savefig(v, joinpath(pth,"$k-grey.pdf"))
        end
    end
    pl,plgrey

end

function artificialθ(p::Param; T = 19,K=20)
    rng = MersenneTwister(1234)
    θagg = DataFrame(year = p.T, it = 1:length(p.T), θrt = p.θrt, θut = p.θut)
    θreg = DataFrame(region = 1:K, θkt_r = ones(K) .+ randn(rng,K) ./ 100, θkt_u = ones(K) .+ randn(rng,K) ./ 100)
    θreg[!, :θkt_r] = ifelse.(θreg.region .== 17, 1.15, θreg.θkt_r)
    θreg[!, :θkt_r] = ifelse.(θreg.region .== 18, 0.9, θreg.θkt_r)
    θreg[!, :θkt_u] = ifelse.(θreg.region .== 19, 1.1, θreg.θkt_u)
    θreg[!, :θkt_u] = ifelse.(θreg.region .== 20, 0.95, θreg.θkt_u)

    x = DataFrame(region = repeat(collect(1:K),inner = 1, outer = T ), it = repeat(repeat(collect(1:T),inner = K, outer = 1 )))
    x = @chain x begin
        leftjoin(θagg, on = :it)
        leftjoin(θreg, on = :region)
        transform([:θrt, :θkt_r] => ((x,y) -> x .* y) => :θr)
        transform([:θut, :θkt_u] => ((x,y) -> x .* y) => :θu)
    end
    x
end


function artificialθ(p::Param, offsets::OrderedDict; T = 19, K = 4)
    # aggregate growth
    θagg = DataFrame(year = p.T, it = 1:length(p.T), θrt = p.θrt, θut = p.θut)

    θreg = DataFrame(region = 1:K, θkt_r = ones(K),θkt_u = ones(K) )
    θreg[!, :θkt_r] = ifelse.(θreg.region .== 1, offsets["low θr"], θreg.θkt_r)
    θreg[!, :θkt_r] = ifelse.(θreg.region .== 2, offsets["high θr"], θreg.θkt_r)
    θreg[!, :θkt_u] = ifelse.(θreg.region .== 3, offsets["low θu"], θreg.θkt_u)
    θreg[!, :θkt_u] = ifelse.(θreg.region .== 4, offsets["high θr"], θreg.θkt_u)

    x = DataFrame(region = repeat(collect(1:K),inner = 1, outer = T ), it = repeat(repeat(collect(1:T),inner = K, outer = 1 )))
    x = @chain x begin
        leftjoin(θagg, on = :it)
        leftjoin(θreg, on = :region)
        transform([:θrt, :θkt_r] => ((x,y) -> x .* y) => :θr)
        transform([:θut, :θkt_u] => ((x,y) -> x .* y) => :θu)
    end
    x
end

function arti_run()
    b = khet_run(readdisk = true)
    K = 20
    p = Param(par = Dict(:T => 1840:10:2020, :gsr => zeros(K),:gs => zeros(K),:kshare => [1/K for i in 1:K],:factors => ones(K),:K => K,:𝕊 => zeros(60,K)))
    arti = artificialθ(p)
    
    artificialθ_impl_(p,b,arti)
end

function arti_plot(x::DataFrame)
    # return arti
    x[!,:LIBGEO] .= ""
    x[x.region .== 17, :LIBGEO] .= "high θr"
    x[x.region .== 18, :LIBGEO] .= "low θr"
    x[x.region .== 19, :LIBGEO] .= "high θu"
    x[x.region .== 20, :LIBGEO] .= "low θu"
    idx = subset(x, :it => ieq(1))
    labs = reshape(idx.LIBGEO, 1,20)
    cols = reshape([[:grey for i in 1:16]...,:darkgreen,:darkgreen,:dodgerblue,:dodgerblue], 1,20)
    styles = reshape([[:solid for i in 1:16]...,:solid,:dot,:solid,:dot], 1,20)
    widths = reshape([[0.8 for i in 1:16]...,3,3,3,3], 1,20)

    def_theme()

    pl = Dict()
    pl_grey = Dict()
    # avgd
    pl[:density] = @df x plot(:year, :citydensity, group = :region, color = cols, label = labs, linestyle = styles, linewidth = widths, size = panelsizef(npanels = 2), yscale = :log10, yticks = [5, 10,20,40,100], yformatter = x -> string(round(Int,x)))
    pl_grey[:density] = @df x plot(:year, :citydensity, group = :region, color = :grey, leg = false, linestyle = :solid, linewidth = 0.8, size = panelsizef(npanels = 2), yscale = :log10, yticks = [5, 10,20,40,100], yformatter = x -> string(round(Int,x)))

    # rhor
    pl[:ρr] = @df x plot(:year, :ρr, group = :region, color = cols, label = labs, linestyle = styles, linewidth = widths, size = panelsizef(npanels = 2))
    pl_grey[:ρr] = @df x plot(:year, :ρr, group = :region, color = :grey, leg = false, linestyle = :solid, linewidth = 0.8, size = panelsizef(npanels = 2))

    pl[:cityarea] = @df x plot(:year, :cityarea, group = :region, color = cols, label = labs, linestyle = styles, linewidth = widths,size = panelsizef(npanels = 2))
    pl_grey[:cityarea] = @df x plot(:year, :cityarea, group = :region, color = :grey, leg = false, linestyle = :solid, linewidth = 0.8,size = panelsizef(npanels = 2))

    pl[:LrLu] = @df x plot(:year, :Lr ./ (:Lr .+ :Lu), group = :region, color = cols, label = labs, linestyle = styles, linewidth = widths, size = panelsizef(npanels = 2))
    pl_grey[:LrLu] = @df x plot(:year, :Lr ./ (:Lr .+ :Lu), group = :region, color = :grey, leg = false, linestyle = :solid, linewidth = 0.8, size = panelsizef(npanels = 2))

    pl,pl_grey

end

function artificialθ_impl_(p::Param,θrdf::DataFrame, arti::DataFrame; overdrive = 20)
	sols = NamedTuple[]
	objs = Float64[]
	edatas = DataFrame[]
	# ϕvs =Vector{Float64}[]
	# dϕvs =Vector{Float64}[]

	# get starting point from baseline
	d0 = @chain θrdf begin
		subset(:year => x -> x .== minimum(x))  # period == it
		sort(:region)  # just to make sure
		select(:region,:r,:pr,:Sr,:Lu,:Lr,:θu,:θr, :ϕ, :Srh)
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
	for it in 1:length(p.T)
		@debug it
		setperiod!(p,it)
		c = HetCountry(p) 
		push!(edatas, areaprice_estim(p))

        for ik in 1:p.K
            idx = (arti.it .== it) .&  (arti.region .== ik)
            # println("k = $ik, t = $it, idx = $idx")
            c.pp[ik].θr = arti.θr[idx][1]
            c.pp[ik].θu = arti.θu[idx][1]
        end

		
		if it < overdrive
			# d0 = @chain θrdf begin
			# 	subset(:year => ieq(years[it]))  # period == it
			# 	sort(:region)  # just to make sure
			# 	select(:region,:r,:pr,:Sr,:Lu,:Lr,:θukt_new => :θu,:θrkt_new => :θr, :ϕ, :Srh)
			# end
			# xx = nlsolve((x,y) -> system!(x,y,p,sum(d0.Lu .+ d0.Lr),d0.θu, d0.θr),nt2x(sols[end]), autodiff = :forward)
			# push!(sols, x2nt(xx.zero,K))
            xx = jc(c,sols[end], estimateθr = false, estimateθu = false)
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

            tvals = if it > 13
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
                xx = jc(c,sols[end], estimateθr = false, estimateθu = false )
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