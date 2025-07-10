
# functionality to fix the land value in the rural sector
# A "Partial Equilibrium Model"

function sensitivity_fixedρ(; readdisk = true)
    x = fixed_ρr_results(readdisk = readdisk)
    q = fixed_ρr_summary(x)
end

"interpolate all values in v and return next index' value"
function itp_var(v::Vector)
    ix = length(v)
    it = linear_interpolation(1:length(v),v , extrapolation_bc = Linear())
    it(ix + 1)
end


function fixedρ_4_search(target; delta0 = 1.0)

    sols = NamedTuple[]

    x0 = fixedρ_4(delta = delta0)
    println("delta0 solved")

    push!(sols, x0[2])

    tried_counter = 0
    vals = collect(0.01:0.01:target)
    for id in vals
        tried_counter += 1
        if tried_counter > 1
            # make a prediction of next start value based on accumulated data


        else
            new_start = sols[end]
        end
        println("trying $(delta0 + id) increase")
        x1 = fixedρ_4( delta = delta0 + id, x00 = new_start)
        push!(sols,x1[2])
    end
    sols, vals
end


function fixedρ_4(; delta = 1.0, x00 = nothing, plots = false)
    # load 4 city baseline
    pth = joinpath(dbplots(), "revision$(revision())","appendix-numillust")
    pthout = joinpath(dbplots(), "revision$(revision())","sensitivity-fixedrho")

    b = JLD2.load_object(joinpath(pth,"data.jld2"))

    # 4 city param
    p4 = param_4_cities()
    p0 = Param()  # dummy baseline param for starting value
    p0.K = 4

    # get artificial θ series 
    offs = OrderedDict("low θr" => 0.95,"high θr" => 1.05,"low θu" => 0.95,"high θu" => 1.05)

    arti = artificialθ(p4,offs)

    # add baseline 'rho  to arti and change it by delta
    arti = innerjoin(arti,select(b,:region,:it,:ρr => (x -> x .* delta) => :ρr_given ), on = [:region, :it])
    # add constant lambda as well
    arti = innerjoin(arti, 
        combine(
            groupby(b, :it), 
            :region, :Sr => (x -> x ./ sum(x)) => :λSr ), on = [:region, :it])

    
    sort!(arti, [:region, :it])

    # get a starting values
    x0 = start_4cities(p0)

    if !isnothing(x00)
        x0 = x00
    end

    # p4.σ = 1.0 

    # run model 
    # with p4, not p0!
    rm = app_numillustration_impl_(p4,x0,arti)
    if rm isa JuMP.Model
        error()
    end

    if !plots
        return rm[2] # sols
    else
        d = @chain rm[1] begin
            rename(_, string.(names(_), "_exo"))
        end

        bd = innerjoin(b,d, on = [:region => :region_exo, :it => :it_exo])

        bd.LrSr = bd.Lr ./ bd.Sr
        bd.LrSr_exo = bd.Lr_exo ./ bd.Sr_exo

        pl = Dict()
        labs = reshape(collect(keys(offs)),1,4)
        cols = [:green :blue :red :black]
        for v in [:Sr, :citydensity, :ρr, :Lu, :Lr, :ϕ, :pr, :wr, :LrSr]
            pl[v] = plot(bd.year, bd[!,v], group = bd.region, title = v, color = cols, label = labs)
            plot!(pl[v] , bd.year, bd[!,string(v,"_exo")], group = bd.region, color = cols, linestyle = :dash, label = "")
        end
        
        # get agg population dist
        pops = @chain bd begin
            groupby(:year)
            combine(
                [:Lu_exo, :Lr_exo] => ((x,y) -> sum(x + y)) => :pop_exo,
                [:Lu, :Lr] => ((x,y) -> sum(x + y)) => :pop,
                [:Sr, :ϕ, :Srh] => ((x,y,z) -> sum(x + (y .^ 2 .* π) + z)) => :S,
                [:Sr_exo, :ϕ_exo, :Srh_exo] => ((x,y,z) -> sum(x + (y .^ 2 .* π) + z)) => :S_exo,
            )
        end

        pl[:pop] = plot(bd.year, bd.Lu .+ bd.Lr, group = bd.region, title = "population", color = cols, leg = :right)
        plot!(pl[:pop], pops.year, pops.pop, linewidth = 3, color = :magenta, label = "total", linestyle = :dash)

        pl[:pop_exo] = plot(bd.year, bd.Lu_exo .+ bd.Lr_exo, group = bd.region, title = "population exo", color = cols, leg = :right)
        plot!(pl[:pop_exo], pops.year, pops.pop_exo, linewidth = 3, color = :magenta, label = "total")

        pl[:Srall] = plot(bd.year, bd.Sr .+ (bd.ϕ .^ 2 .* π) .+ bd.Srh, group = bd.region, title = "S", color = cols, leg = :right)
        plot!(pl[:Srall], pops.year, pops.S, linewidth = 3, color = :magenta, label = "total")

        pl[:Srall_exo] = plot(bd.year, bd.Sr_exo .+ (bd.ϕ_exo .^ 2 .* π) .+ bd.Srh_exo, group = bd.region, title = "S exo", color = cols, leg = :right)
        plot!(pl[:Srall_exo], pops.year, pops.S_exo, linewidth = 3, color = :magenta, label = "total")

        pl[:SrLr2] = plot(pl[:Sr], pl[:Lr], layout = @layout [a ; b])


        # for (k,v) in pl 
        #     savefig(v, joinpath(pthout,"$k.png"))
        # end

        pl

    end
end

function raise20(delta,it)
    b = khet_run(readdisk = true)

    # param vector
    K = 20
    p = Param(par = Dict(:T => 1840:10:2020,:gsr => zeros(K),:gs => zeros(K),:kshare => [1/K for i in 1:K],:factors => ones(K),:K => K,:𝕊 => zeros(60,K)))
    
    # get a starting value
    d0 = @chain b begin
        subset(:it => x -> x .== it )  # period == 1
        sort(:region)  # just to make sure
        select(:region,:r,:pr,:Sr,:Lu,:Lr,:θu,:θr, :ϕ, :Srh)
    end

    sols = NamedTuple[]
    ds = DataFrame()


    x0 = (Sr = d0.Sr,
        Lu = d0.Lu,
        Lr = d0.Lr,
        r = unique(d0.r)[1],
        pr = unique(d0.pr)[1])
    push!(sols, x0)

    increasing = delta > 0
    nosteps = abs(delta) < 0.01
    dsteps = nosteps ? delta : (increasing ? (0.01:0.01:delta) : (-0.01):(-0.01):delta)
    str = increasing ? "Increasing" : "Decreasing"
    if nosteps
        println()
        println("$str ρr in $(p.T[it]) by $(round(100 * delta, digits = 1))%")
        println()
    end
    prog = Progress(length(dsteps); dt=1, barglyphs=BarGlyphs("[=> ]"), barlen=40, desc = "$str ρr in $(p.T[it]) by $(round(100 * delta, digits = 1))%")
    for id in dsteps
        dd = (1 + id)
        # increase ρr exogenously in all periods and regions
        transform!(b, :ρr => (x -> x .* dd) => :ρr_given)

        # create dataframe with thetas and exogenous price
        exog = combine(
            groupby(b, :it),
            :region,:LIBGEO, :citydensity,:cityarea, :ρr_given, :θu, :θr, :Lu, :Lr, :Sr => (x -> x ./ sum(x)) => :λSr )

        startval = if abs(id) > 0.05
            isol = eachindex(sols)
            # take sols[end] and predict next period's starting values
            (
            Sr = itp_var([sols[i].Sr for i in isol ]),
            Lu = itp_var([sols[i].Lu for i in isol ]),
            Lr = itp_var([sols[i].Lr for i in isol ]),
            r = itp_var([sols[i].r   for i in isol ]),
            pr = itp_var([sols[i].pr for i in isol ]),
            )
        else
            sols[end]
        end
        xx = fixed_single(p,it,startval,exog)
        if xx isa JuMP.Model
            println("failed for delta = $(dd)")
            return Dict(:model => xx,:sols => sols,:p => p,:exog => exog)
        else
            push!(sols,xx[2])
            xx[1].delta .= id
            append!(ds,xx[1])
        end
        next!(prog)
    end
    @info "max error Lu - iDensity" maximum(ds.iDensity .- ds.Lu)
    ds
end



function fixed_ρr_summary(di::Dict; target = "ρr_GDP_pc_t-1")

    pth = joinpath(dbplots(), "revision$(revision())","sensitivity-fixedrho",target)
    mkpath(pth)

    # di has the max elasticity and the 10% elasticity
    # want to return avg elasticites for the 10% case as well.



    # 0. get full baseline data for 1870 outcomes
    b1870 = @chain khet_run(readdisk = true) begin 
        subset(:year => ieq(1870))
        select(:year, :region, :LIBGEO, :citydensity,:cityarea, :cityarea => :cityarea_baseline, :citydensity => :citydensity_baseline, :Lu, :Lu => :Lu_baseline, :region => (x -> 0.0) => :rho_delta)
    end
    disallowmissing!(b1870)

    x = di[:deltamax]
    # 1. output data to csv
    d = select(x, :year, :region, :LIBGEO, :citydensity, :cityarea, :cityarea_baseline, :citydensity_baseline, :Lu, :Lu_baseline, :delta => :rho_delta)

    # make a plot of percent increases for each city
    pl = @chain d begin
        transform([:citydensity, :citydensity_baseline] => ((x,y) -> 100 .* (x .- y) ./ y) => :percent)
        plot(_.year,_.percent,group = _.region, color = dcol(20), markershape = :circle, xticks = _.year, ylab = "% increase wrt baseline", title = "city density",leg = :outerright)
    end
    

    # put 1870 on top
    append!(d, b1870)
    sort!(d,:year)
    

    # add 1870 Lu to each year so we can use that as weights
    d = leftjoin(d, select(b1870, :Lu => :Lu_1870, :region), on = :region)
    disallowmissing!(d)

    sort!(d, [:LIBGEO,:year])

    CSV.write(joinpath(pth,"full-results.csv"),d)

    by_city = @chain d begin
        groupby(:LIBGEO)
        combine(
        :year                                => (x -> string.( x[1:(end-1)] ,"-", x[2:end]))     => :years,
        :region => first => :region,
        :citydensity_baseline                => (x -> 100 .* log.(x[2:end] ./ x[1:(end-1)]))     => "%Δ baseline(log)",
        :citydensity                         => (x -> 100 .* log.(x[2:end] ./ x[1:(end-1)]))     => "%Δ counterfactual(log)",
        [:citydensity,:citydensity_baseline] => ((x,y) -> 100 .* log.(x[2:end] ./ y[1:(end-1)])) => "%Δ cf vs base(log)",
        :Lu_baseline                         => (x -> 100 .* log.(x[2:end] ./ x[1:(end-1)]))     => "%Δ Lu baseline(log)",
        :Lu                                  => (x -> 100 .* log.(x[2:end] ./ x[1:(end-1)]))     => "%Δ Lu counterfactual(log)",
        :Lu_baseline                         => (x -> x[2:end] .- x[1:(end-1)])     => "Δ Lu baseline",
        :Lu                                  => (x -> x[2:end] .- x[1:(end-1)])     => "Δ Lu"
        )
        transform(["%Δ baseline(log)", "%Δ cf vs base(log)"] => ((x,y) -> 1 .- (y ./ x)) => :share_explained_by_falling_ρr_y)
    end
    CSV.write(joinpath(pth,"bycity.csv"),d)

    
    # 2. compute table with different measures of "average density"

    # create previous year weights
    sort!(d, [:region,:year])
    transform!(groupby(d,:region),
        :Lu => (x -> lag(x)) => :Lu_lag,
        :Lu_baseline => (x -> lag(x)) => :Lu_baseline_lag
        )
    mi = @view d[ismissing.(d.Lu_lag),:]
    mi.Lu_lag = mi.Lu_1870
    mi.Lu_baseline_lag = mi.Lu_1870

    disallowmissing!(d)
    # return d
    
    ad = @chain d begin
        groupby(:year)
        combine(
            [:citydensity_baseline, :Lu_baseline,:Lu_baseline_lag, :cityarea_baseline] => ((x,y,z,a) -> (
                baseline_mean_Lu_base = mean(x,Weights(y)),
                baseline_mean_Lu_lag = mean(x,Weights(Array(z))), 
                baseline_agg_Lu_area = sum(y) / sum(a) 
                )) => AsTable,
            [:citydensity, :Lu ,:Lu_baseline, :Lu_lag, :cityarea] => ((x,y,w,z,a) -> (
                cf_mean = mean(x), 
                cf_mean_Lu_cf = mean(x,Weights(y)),
                cf_mean_Lu_lag = mean(x,Weights(Array(z))), 
                cf_agg_Lu_area = sum(y) / sum(a), 
                cf_mean_Lu_base = mean(x,Weights(w))
                )) => AsTable,
            :rho_delta => first => :delta
            )
    end

    adsmall = @chain d begin
        transform(:region => (x -> x .> 2) => :small)
        groupby([:year,:small])
        combine(
            [:citydensity_baseline, :Lu_baseline, :Lu_baseline_lag, :cityarea_baseline] => ((x,y,z,a) -> (
                baseline_mean = mean(x), 
                baseline_mean_Lu_base = mean(x,Weights(y)),
                baseline_mean_Lu_lag = mean(x,Weights(z)), 
                baseline_agg_Lu_area = sum(y) / sum(a) 
                )) => AsTable,
            [:citydensity, :Lu ,:Lu_baseline, :Lu_lag, :cityarea] => ((x,y,w,z,a) -> (
                cf_mean = mean(x), 
                cf_mean_Lu_cf = mean(x,Weights(y)),
                cf_mean_Lu_lag = mean(x,Weights(z)), 
                cf_agg_Lu_area = sum(y) / sum(a), 
                cf_mean_Lu_base = mean(x,Weights(w))
                )) => AsTable,
            :rho_delta => first => :delta
            )
    end

    ad10 = @chain di[:delta10] begin
        groupby(:year)
        combine(
            :citydensity_baseline => (x -> mean(x)) => :baseline_mean,
            :citydensity          => (x -> mean(x)) => :cf_mean,
            :delta => first => :delta,
            [:citydensity,:citydensity_baseline,:delta] => ((cf,b,d) -> log.(mean(cf) ./ mean(b)) ./ log.(1 .+ mean(d))  ) => :elasticity_of_means,
            # [:citydensity,:citydensity_baseline,:delta] => ((cf,b,d) -> mean(  log.(cf ./ b) ) / log(1 .+ first(d)  )) => :mean_of_elasticities2,
            [:citydensity,:citydensity_baseline,:delta] => ((cf,b,d) -> mean(  log.(cf ./ b)  ./ log.(1 .+ first(d) ))) => :mean_of_elasticities
            )
    end

    # compute 10% elasticity


    select!(ad,:year, :baseline_mean_Lu_base, :cf_mean_Lu_cf, :baseline_mean_Lu_lag, :cf_mean_Lu_lag)
    
    CSV.write(joinpath(pth,"averages.csv"),ad)
    savefig(pl, joinpath(pth,"density-increases.pdf"))

    j_changes = @chain ad begin
        combine(
            :year => (x -> string.( x[1:(end-1)] ,"-", x[2:end])) => :years,
            :baseline_mean_Lu_base => (x -> 100 .* log.(x[2:end] ./ x[1:(end-1)])) => "%Δ baseline_mean_Lu_base(log)",
            :baseline_mean_Lu_lag => (x -> 100 .* log.(x[2:end] ./ x[1:(end-1)])) => "%Δ baseline_mean_Lu_base(log,lag)",
            [ :baseline_mean_Lu_base,:cf_mean_Lu_cf] => ((x,y) -> 100 .* log.((y[2:end] ./ x[1:(end-1)]))) => "%Δ vs baseline (log)",
            [ :baseline_mean_Lu_lag,:cf_mean_Lu_lag] => ((x,y) -> 100 .* log.((y[2:end] ./ x[1:(end-1)]))) => "%Δ vs baseline (log,lag)",
            )
    end

    j_changes_small = @chain adsmall begin
        groupby(:small)
        combine(
            :year => (x -> string.( x[1:(end-1)] ,"-", x[2:end])) => :years,
            :baseline_mean_Lu_base => (x -> 100 .* log.(x[2:end] ./ x[1:(end-1)])) => "%Δ baseline_mean_Lu_base(log)",
            :baseline_mean_Lu_lag => (x -> 100 .* log.(x[2:end] ./ x[1:(end-1)])) => "%Δ baseline_mean_Lu_base(log,lag)",
            [ :baseline_mean_Lu_base,:cf_mean_Lu_cf] => ((x,y) -> 100 .* log.((y[2:end] ./ x[1:(end-1)]))) => "%Δ vs baseline (log)",
            [ :baseline_mean_Lu_lag,:cf_mean_Lu_lag] => ((x,y) -> 100 .* log.((y[2:end] ./ x[1:(end-1)]))) => "%Δ vs baseline (log,lag)",
            )
    end

    headline = transform(j_changes, 
    ["%Δ baseline_mean_Lu_base(log)", "%Δ vs baseline (log)"] => ((x,y) -> 1 .- y ./ x) => :share_explained_by_falling_ρr_y,
    ["%Δ baseline_mean_Lu_base(log,lag)", "%Δ vs baseline (log,lag)"] => ((x,y) -> 1 .- y ./ x) => :share_explained_by_falling_ρr_y_lag
    )

    headlinesmall = combine(groupby(j_changes_small,:small), 
    :years => first => :years,
    ["%Δ baseline_mean_Lu_base(log)", "%Δ vs baseline (log)"] => ((x,y) -> 1 .- y ./ x) => :share_explained_by_falling_ρr_y,
    ["%Δ baseline_mean_Lu_base(log,lag)", "%Δ vs baseline (log,lag)"] => ((x,y) -> 1 .- y ./ x) => :share_explained_by_falling_ρr_y_lag)

    select!(headline, :years, :share_explained_by_falling_ρr_y, :share_explained_by_falling_ρr_y_lag)

    return Dict(:avg_densities => ad,:avg_densities_small => adsmall,:elast_10 => ad10,:plot => pl,:density_changes => j_changes, :density_changes_small => j_changes_small, :headline => headline, :headlinesmall => headlinesmall, :bycity => by_city,:full => d)
    
end

function fixed_ρr_setup(;fifty_years = true)
    # get baseline to find required price increases
    at_years = fifty_years ? [1870,1920,1970,2020] : collect(1870:10:2020)
    a = subset!(khet_run(readdisk = true) |> aggregator , :year => ByRow(<(2030)))

    a_years = @chain a begin
        subset(:year => ByRow(<(2030)))
        select(:year, :mean_ρr, :GDP_agg_pc, :mean_ρr_GDP_pc, :mean_ρr_y_pc, :citydensity_agg,:mean_citydensity,:y_disposable_pc,:mean_ρr_y_disposable, :GDP_agg,:mean_ρr_GDP)
        subset(:year => ByRow(∈(at_years)))
    end

    # this does ρ1 / y1 = x1 => ρ2 = x1 * y2
    a_changes = @chain a_years begin
        combine(
            :year => (x -> string.( x[1:(end-1)] ,"-", x[2:end])) => :years,
            [i => (x -> 100 .* (x[2:end] .- x[1:(end-1)]) ./ x[1:(end-1)]) => "%Δ $i" for i in [:mean_ρr, :GDP_agg_pc,:mean_ρr_GDP_pc,:mean_ρr_y_disposable,:mean_ρr_y_pc , :citydensity_agg,:mean_citydensity,:y_disposable_pc ,:GDP_agg,:mean_ρr_GDP ]],
            [:GDP_agg_pc,:mean_ρr_GDP_pc, :mean_ρr] => ((x,y,z) -> 100 * ((x[2:end] .* y[1:(end-1)]) .- z[2:end]) ./ z[2:end] ) => "ρr_GDP_pc_t-1",
            [:GDP_agg   ,:mean_ρr_GDP   , :mean_ρr] => ((x,y,z) -> 100 * ((x[2:end] .* y[1:(end-1)]) .- z[2:end]) ./ z[2:end] ) => "ρr_GDP_t-1",
            [:GDP_agg_pc,:mean_ρr_GDP_pc, :mean_ρr] => ((x,y,z) -> 100 * ((x[2:end] .* y[1]) .- z[2:end]) ./ z[2:end] ) => "ρr_GDP_pc_1870",
            [:y_disposable_pc,:mean_ρr_y_pc, :mean_ρr] => ((x,y,z) -> 100 * ((x[2:end] .* y[1:(end-1)]) .- z[2:end]) ./ z[2:end] ) => "ρr_y_t-1",
        )
    end

    pth = joinpath(dbplots(), "revision$(revision())","sensitivity-fixedrho")
    CSV.write(joinpath(pth,"keep_rho_constant.csv"), a_years)



    pl = Dict()
    a1870 = subset(a, :year => geq(1870))


    CSV.write(joinpath(pth,"baseline_GDP_growth.csv"), select(a1870, :year, :GDP_agg_1870,:GDP_agg_pc_1870,:Yu_pc,:Yu_Lu,:Yu_pc_1870,:Yu_Lu_1870 ))

    pl[:agg] = @df a1870 plot(:year, log.(:GDP_agg_1870), title = "Aggregate",label = "sum(GDP)", xticks = :year, ylab = "log(GDP(1870))")
    plot!(pl[:agg], a1870.year, log.(a1870.Yu_agg_1870), label = "sum(Yu)")
    plot!(pl[:agg], a1870.year, log.(a1870.θu_agg_1870), label = "sum(θu)")
    pl[:pc] = @df a1870 plot(:year, log.(:GDP_agg_pc_1870), title = "Per Capita", label = "sum(GDP)/sum(pop)", xticks = :year, ylab = "log(GDP_pc(1870))")
    plot!(pl[:pc],a1870.year, log.(a1870.Yu_pc_1870), label = "sum(Yu)/sum(pop)")
    plot!(pl[:pc],a1870.year, log.(a1870.Yu_Lu_1870), label = "sum(Yu)/sum(Lu)")
    plot!(pl[:pc],a1870.year, log.(a1870.θu_Lu_1870), label = "sum(θu)/sum(Lu)")
    return a_years, a_changes, pl
end

function fixed_ρr_results(;target = "ρr_GDP_pc_t-1", fifty_years = true, readdisk = false)
    pth = joinpath(dbplots(), "revision$(revision())","sensitivity-fixedrho",target)
    mkpath(pth)

    # plots
    pl = Dict()

    a_years, a_changes, pl = fixed_ρr_setup(fifty_years = fifty_years)
    # return a_changes, a_years

    if readdisk
        r = JLD2.load_object(joinpath(pth,fifty_years ? "$(target)-data-50years.jld2" : "$(target)-data.jld2"))
    else
        year_ranges = a_changes.years
        # year_indices = [9,14,19]  # 1920,1970,2020
        year_indices = fifty_years ? [9,14,19] : collect(5:19)  
        r = OrderedDict(
            year_ranges[k] => raise20(a_changes[a_changes.years .== year_ranges[k],target][1] / 100,year_indices[k]) for k in eachindex(year_indices)
        )
        JLD2.jldsave(joinpath(pth,fifty_years ? "$(target)-data-50years.jld2" : "$(target)-data.jld2"); r)
    end

    # keep only highest elasticity estimate and outcomes for each year.
    de = DataFrame()
    d10 = DataFrame()
    for (k,v) in r
        v.years .= k
        append!(d10,subset(v, :delta => (x -> (x .≈ 0.1))))
        append!(de,subset(v, :delta => (x -> (x .== maximum(x)))))
    end

    return Dict(:delta10 => d10, :deltamax => de)
end

function raise4(delta,it)
    pth = joinpath(dbplots(), "revision$(revision())","appendix-numillust")
    pthout = joinpath(dbplots(), "revision$(revision())","sensitivity-fixedrho")

    b = JLD2.load_object(joinpath(pth,"data.jld2"))

    # 4 city param
    p4 = param_4_cities()
    p0 = Param()  # dummy baseline param for starting value
    p0.K = 4

    # get artificial θ series 
    offs = OrderedDict("low θr" => 0.95,"high θr" => 1.05,"low θu" => 0.95,"high θu" => 1.05)

    arti = artificialθ(p4,offs)

    # add baseline 'rho  to arti and change it by delta
    arti = innerjoin(arti,select(b,:region,:it,:ρr => (x -> x .* delta) => :ρr_given ), on = [:region, :it])
    # add lambda as well
    arti = innerjoin(arti, 
        combine(
            groupby(b, :it), 
            :region, :Sr => (x -> x ./ sum(x)) => :λSr ), on = [:region, :it])

    
    sort!(arti, [:region, :it])
    # get a starting value
    d0 = @chain b begin
        subset(:it => x -> x .== it )  # period == 1
        sort(:region)  # just to make sure
        select(:region,:r,:pr,:Sr,:Lu,:Lr,:θu,:θr, :ϕ, :Srh)
    end

    x0 = (Sr = d0.Sr,
        Lu = d0.Lu,
        Lr = d0.Lr,
        r = unique(d0.r)[1],
        pr = unique(d0.pr)[1])
    fixed_single(p4,it,x0,arti)

end

"increase rho from basline in a given period"
function fixed_single(p::Param,it::Int, x0::NamedTuple, arti::DataFrame)
	
    setperiod!(p,it)
    c = HetCountry(p) 
    	
    # overwrite region specific productivities
    for ik in 1:p.K
        idx = (arti.it .== it) .&  (arti.region .== ik)
        c.pp[ik].θr = arti.θr[idx][1]
        c.pp[ik].θu = arti.θu[idx][1]
    end
    ρr_given = arti[arti.it .== it, "ρr_given"]
    λSr = arti[arti.it .== it, "λSr"]
        
    xx = jc(c,x0, estimateθr = false, estimateθu = false, ρr_given = ρr_given,λSr = λSr, constr_viol_tol = 1e-1 )

    if xx isa JuMP.Model
        xx
    else
        update!(c,xx[1],estimateθ = false)
        d0 = subset(arti, :it => ieq(it))
        d1 = DataFrame(region = 1:p.K, 
                        LIBGEO = d0.LIBGEO, 
                        year = p.T[it], 
                        it = it,
                        rhorexog = d0.ρr_given, 
                        citydensity = [i.citydensity for i in c.R], 
                        cityarea = [i.cityarea for i in c.R], 
                        iDensity = [i.iDensity for i in c.R], 
                        GDP = [i.GDP for i in c.R], 
                        ρr_y = [i.ρr_y for i in c.R], 
                        Lu = [i.Lu for i in c.R], 
                        Lr = [i.Lr for i in c.R], 
                        Sr = [i.Sr for i in c.R], 
                        Srh = [i.Srh for i in c.R])
        d2 = leftjoin(d1, select(d0, :it, :region, :citydensity => :citydensity_baseline,:Lu => :Lu_baseline,:cityarea => :cityarea_baseline), on = [:region, :it])
        return d2,xx[1]
    end	
end