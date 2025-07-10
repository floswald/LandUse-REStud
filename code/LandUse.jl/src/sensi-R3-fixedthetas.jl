
"""
# R3's experiment: fixed producitivity differences

This experiment is similar to [`sensitivity_θr`](@ref), but we only estimate the regional
producitivity shifters in the first period. That is, we can import them directly from the
[`khet_run`](@ref) function, period 1. This gives us the ``\\theta_{sk1}``, i.e. for each sector, the period 1 specific shifter. Then, we use modified version of [`change_θ_growth`](@ref), which will apply those period 1 shifters on top of the aggregate trends.

Finally, we solve the model in each period, *without* estimating the theta's.


"""
function sensitivity_R3_θfixed(; save = false, readdisk = false, Tmax = 2020, K = 20, returnplots = true)

    pth = joinpath(dbplots(), "revision$(revision())","sensitivity-R3-fixedtheta")
    mkpath(joinpath(pth,"aggregation"))


    if !readdisk
        d = khet_run(readdisk = true,K = K)  # baseline 
        # get a baseline param
        p0 = Param(par = Dict(:T => 1840:10:Tmax, :K => K))
    
        dd = sensitivity_R3_(d,p0,Tmax,K)
        JLD2.jldsave(joinpath(pth,"data.jld2"); dd)
    else
        dd = JLD2.load_object(joinpath(pth,"data.jld2"))
    end
    
    if !returnplots
        return dd
    else

        p = plot_het(dd)
        
        if save
            export_any_csv(dd, joinpath(pth, "R3-θfixed.csv"))


            for (k,v) in p[:het]
                savefig(v, joinpath(pth, "$k.pdf"))	
            end
            for (k,v) in p[:agg]
                savefig(v, joinpath(pth,"aggregation","$k.pdf"))	
            end

        end
        p
    end

end


"""
    θsk_fixer

function takes baseline, extracts period 1 regional shifters, and applies them going forward 
"""
function θsk_fixer(d::DataFrame,p::Param)
    # get aggregate theta and it's growth rate first
    θagg = DataFrame(year = p.T, it = 1:length(p.T), θrt = p.θrt, θut = p.θut)
    # baseline data has θrkt = θrt * θkt, and we observe θrt.
    
    # first year data
    d1 = @chain d begin
        subset(:year => ieq(1840))
        select(:year, :θr => :θrkt, :θu => :θukt, :region, :LIBGEO,:r,:pr,:Sr,:Lu,:Lr,:ϕ, :Srh)  
        leftjoin(θagg, on = :year)
        # hence can extract regional component: θrkt / θrt = θkt
        transform([:θrkt, :θrt] => ((x,y) -> x ./ y ) => :θkt_r1)   
        transform([:θukt, :θut] => ((x,y) -> x ./ y ) => :θkt_u1)  
        transform(:θrkt => :θrkt_new )
        transform(:θukt => :θukt_new )
    end

    d2 = @chain d begin
        subset(:year => (x -> (x .> 1840) .& (x .<= p.T[end])))
        select(:year, :θr => :θrkt, :θu => :θukt, :region, :LIBGEO,:r,:pr,:Sr,:Lu,:Lr,:ϕ, :Srh) 
        leftjoin(θagg, on = :year)
        leftjoin(select(d1, :θkt_r1, :θkt_u1, :LIBGEO), on = :LIBGEO)
        transform([:θrt, :θkt_r1] => ((x,y) -> x .* y) => :θrkt_new )
        transform([:θut, :θkt_u1] => ((x,y) -> x .* y) => :θukt_new )
    end
    [d1; d2]

end


function sensitivity_R3_(d::DataFrame, p::Param, Tmax,K)

    @info "running R3's counterfactual"

    # fix city shifters to first period
    dd = θsk_fixer(d,p)
   
 
    # return subset(dd, :region => x -> x .< 6)
    # make plot old vs new setting 
    pl1 = @df dd plot(:year,:θkt_r1, group = :LIBGEO, yscale = :log10, ylab = L"\log \theta_{k,r}",xticks = 1840:40:2020, leg = :outerright, yformatter = x -> string(round(x,digits = 2)), color = dcol(20), lw = 2)
    # @df dd plot!(pl1,:year, :θrt_new, label = "", color = :red, lw = 4)
    savefig(pl1, joinpath(dbplots(),"revision$(revision())","sensitivity-R3-fixedtheta","setup-theta-k-r.pdf"))

    pl1u = @df dd plot(:year,:θkt_u1, group = :LIBGEO, yscale = :log10,ylab = L"\log \theta_{k,u}",xticks = 1840:40:2020, leg = :outerright, yformatter = x -> string(round(x,digits = 2)), color = dcol(20), lw = 2)
    # @df dd plot!(pl1u,:year, :θut_new, label = "", color = :red, lw = 4)
    savefig(pl1u, joinpath(dbplots(),"revision$(revision())","sensitivity-R3-fixedtheta","setup-theta-k-u.pdf"))


    pl0 = @df dd plot(:year,:θrkt, group = :LIBGEO, yscale = :log10, title = "Baseline", ylab = L"\log \theta_r", label = "", color = dcol(20))
    @df dd plot!(pl0,:year, :θrt,  color = :red, lw = 4,leg = :topleft,label = L"\theta_{rt}" , legendfontsize = 12)

    pl0u = @df dd plot(:year,:θukt, group = :LIBGEO, yscale = :log10, title = "Baseline", ylab = L"\log \theta_u", label = "", color = dcol(20))
    @df dd plot!(pl0u,:year, :θut,  color = :red, lw = 4,leg = :topleft,label = L"\theta_{ut}" , legendfontsize = 12)

    pl = plot(pl0, pl1, size = (800,400),left_margin = 0.5Plots.cm)
    plu = plot(pl0u, pl1u, size = (800,400),left_margin = 0.5Plots.cm, layout = @layout [a{0.4w} b{0.6w}])
    plu_ = plot(pl0u, pl1u, size = (800,400),left_margin = 0.5Plots.cm, link = :y, layout = @layout [a{0.4w} b{0.6w}])
    pl_ = plot(pl0, pl1, size = (800,400),left_margin = 0.5Plots.cm, link = :y)
    savefig(pl, joinpath(dbplots(),"revision$(revision())","sensitivity-R3-fixedtheta","setup-growth.pdf"))
    savefig(plu, joinpath(dbplots(),"revision$(revision())","sensitivity-R3-fixedtheta","setup-ugrowth.pdf"))
    # savefig(pl_, joinpath(dbplots(),"revision$(revision())","sensitivity-R3-fixedtheta","setup-growth-link.pdf"))
    # savefig(plu_, joinpath(dbplots(),"revision$(revision())","sensitivity-R3-fixedtheta","setup-ugrowth-link.pdf"))

    # run model
    # =========
    # return dd
    # supply a matrix of rural growth rates for each region and year to the HetCountry constructor

    # khet_estimate_impl(K;tol = 1e-5,par = Dict(:T => 1840:10:Tmax),verbose = true, iterateP = 2,estimateθu = true,estimateθr = false,θdf = dd,lbL = 0.0)
    runk_impl_lowθr(Param(par = Dict(:T => 1840:10:Tmax, :gsr => zeros(K),:gs => zeros(K),:kshare => [1/K for i in 1:K],:factors => ones(K),:K => K,:𝕊 => zeros(60,K))), dd, overdrive = 4 , estimateθr = false, estimateθu = false)
end