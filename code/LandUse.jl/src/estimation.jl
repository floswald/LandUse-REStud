function post_slack(job)
	if haskey(ENV,"SLACK_HOOK")
        cmd = `curl -X POST -H "Content-type: application/json" --data "{'text':'$(job)'}" $(ENV["SLACK_HOOK"])`
        # @show cmd
        Base.run(cmd)
		return nothing
	else
		error("you need a webhook into slack as environment variable SLACK_HOOK to post a message")
	end
end

# curl -X POST -H 'Content-type: application/json' --data '{"text":"Hello, World!"}' https://hooks.slack.com/services/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
function post_slack()
	haskey(ENV,"SLACK_HOOK") || error("you need a webhook into slack as environment variable SLACK_HOOK to post a message")
end

function post_file_slack(fname::String)
    # if LandUse.isflo
    #     Base.run(`curl -F file=@$(fname) -F "initial_comment=Dashboard at best candidate" -F channels=CCW1NHS1K -H "Authorization: Bearer $(ENV["SLACK_FILES"])" https://slack.com/api/files.upload`)
    # else
        Base.run(`curl --insecure -F file=@$(fname) -F "initial_comment=Dashboard at best candidate" -F channels=CCW1NHS1K -H "Authorization: Bearer $(ENV["SLACK_FILES"])" https://slack.com/api/files.upload`)
    # end
end
function post_file_slack()
    haskey(ENV,"SLACK_FILES") || error("you need a webhook into slack as environment variable SLACK_FILES to post files")
end



"""
returns a dict of empirical targets for the model
"""
function targets(p::Param)

    md = @chain copy(p.moments) begin
        subset(:year => leq(2020))
    end

    # vector-value moments: time varying stuff
    m = Dict()
    m[:rural_empl] = DataFrame(moment = ["rural_emp_" .* string(i) for i in md.year], 
               data = copy(md.Employment_rural),
               model = zeros(length(md.year)))

    # single value stuff: ratios of change etc
    # from average city over time
    m[:avg_density_change] = DataFrame(moment = "avg_density_change", data = -0.883, model = 0.0)
    m[:rel_city_area] = DataFrame(moment = "rel_city_area_2010",data = 0.173, model = 0.0)
    maxmode = @chain highfreq_rawdata() begin
        dropmissing(:Comm_speed_Paris)
        select(:year, :Comm_speed_Paris => LandUse.firstnorm => :comm)
    end
    m[:max_mode_increase] =  DataFrame(moment = "max_mode_increase", data = maximum(maxmode.comm), model = 0.0)

    # average city spatial moments in 2020
    # m[:density_9010_2020] =  DataFrame(moment = "density_gradient_2020", data = 6.0 , model = 0.0)   # 1st tenth is 6 times denser than last 10-th
    # exponential decay model. exponential coefficient:
    m[:density_decay_coef] =  DataFrame(moment = "density_decay_coef", data = 0.15 , model = 0.0)   # on 21 points
    m[:density_decay_MSE] =  DataFrame(moment = "density_decay_MSE", data = 0.0 , model = 0.0) 

    # housing spending Shares
    m[:housing_share_1900] = DataFrame(moment = "housing_share_1900", data = 0.237 , model = 0.0)
    # m[:housing_share_2015] = DataFrame(moment = "housing_share_2015", data = 0.314 , model = 0.0)
    m[:housing_share_2010] = DataFrame(moment = "housing_share_2010", data = 0.306 , model = 0.0)

    return m
    
end

function dicts2df(d::Dict)
    df = copy(d[:rural_empl])
    for (k,v) in d
        if k != :rural_empl
            append!(df,v)
        end
    end
    df
end



function search_over(; scenario = :default)
    if scenario == :d1d2
        OrderedDict(zip([:cbar,:sbar, :a, :gamma, :nu, :sigmac], [(0.603, 0.71),(0.16, 0.2), (2.0, 2.34), (0.21, 0.31), (0.02,0.029), (0.7,2.0)]))
    elseif scenario == :aggloλ1
        OrderedDict(zip([:cbar,:sbar, :a, :gamma, :nu, :sigmac], [(0.603, 0.71),(0.16, 0.2), (1.69, 1.71), (0.295, 0.31), (0.02,0.029), (0.7,2.0)]))
    elseif scenario == :aggloλ2
        OrderedDict(zip([:cbar,:sbar, :a, :gamma, :nu, :sigmac], [(0.603, 0.71),(0.16, 0.2), (1.69, 1.721), (0.295, 0.31), (0.02,0.029), (0.7,2.0)]))
    elseif scenario == :aggloμ
        OrderedDict(zip([:cbar,:sbar, :a, :gamma, :nu, :sigmac], [(0.603, 0.71),(0.16, 0.2), (1.5, 1.721), (0.295, 0.31), (0.01,0.029), (0.7,2.0)]))
    else  # default
        OrderedDict(zip([:cbar,:sbar, :a, :gamma, :nu, :sigmac], [(0.65, 0.71),(0.17, 0.2), (1.6, 1.73), (0.295, 0.31), (0.02,0.029), (0.7,2.0)]))
    end
end
# search_over() = OrderedDict(zip([:cbar], [(0.7, 0.88)]))
symrange(x,pad,n) = range(x-pad, stop = x+pad, length = n)

"""
## Moment Function

Returns a dataframe with 3 colums *data*, *model* and *weight* for each required data moment.
"""
function moments(p::Param, d::DataFrame)

    # aggregated data to avg city
    da = @chain d begin
        subset(:year => leq(2020))
        aggregator(_)
    end

    # find year indices
    i1870 = argmin( abs.(p.moments.year .- 1870) )
    i1900 = argmin( abs.(p.moments.year .- 1900) )
    i2020 = argmin( abs.(p.moments.year .- 2020) )
    i2015 = argmin( abs.(p.moments.year .- 2015) )
    i2010 = argmin( abs.(p.moments.year .- 2010) )

    # get data moments
    ta = targets(p)
    # println(ta)
    # println(da.Lrshare_agg)
    ta[:rural_empl].model = copy(da.Lrshare_agg)
    ta[:rural_empl].weights .= 0.01
    ta[:rural_empl].weights[1:6] .= 1.0  # increase weight of first 6 years.

    # m = 0.0
    # m += sum(ta[:rural_empl].weights .* (ta[:rural_empl].data .- ta[:rural_empl].model).^2)

    # ta[:avg_density_fall][!,:model] .= da.citydensity_agg[i1870] / da.citydensity_agg[i2015]
    ta[:avg_density_change][!,:model] .= (da.mean_citydensity_1870[i2020] - da.mean_citydensity_1870[i1870]) / da.mean_citydensity_1870[i1870]
    ta[:avg_density_change][!,:weights] .= 0.0

    ta[:rel_city_area][!,:model] .= da.rel_cityarea[i2010]
    ta[:rel_city_area][!,:weights] .= 15.0

    ta[:max_mode_increase][!,:model] .= maximum(da.mean_commuting_speed_1840)
    ta[:max_mode_increase][!,:weights] .= 0.0

    # ta[:density_gradient_2020][!,:model] .= M1[i2020].iDensity_q10 / M1[i2020].iDensity_q90
    # ta[:density_gradient_2020][!,:weights] .= 1.0

    # exponential model
    de = exp_gradients(d, year = 2020)

    ta[:density_decay_coef][!,:model] .= abs(de[:gradient])
    ta[:density_decay_coef][!,:weights] .= 0.0
    ta[:density_decay_MSE][!,:model] .= de[:MSE]
    ta[:density_decay_MSE][!,:weights] .= 0.0

    # housing spending Shares
    ta[:housing_share_1900][!,:model] .= da[i1900,:Ch_share]
    ta[:housing_share_1900][!,:weights] .= 10.0
    ta[:housing_share_2010][!,:model] .= da[i2010,:Ch_share]
    ta[:housing_share_2010][!,:weights] .= 10.0
    
    # ta[:pop_vs_density_1876][!,:model]   .= (C[1].R[2].cityarea - C[1].R[1].cityarea) / (C[1].R[2].Lu - C[1].R[1].Lu)
    # ta[:pop_vs_density_1876][!,:weights] .= 10.0
    # ta[:pop_vs_density_2015][!,:model]   .= (C[end].R[2].cityarea - C[end].R[1].cityarea) / (C[end].R[2].Lu - C[end].R[1].Lu)
    # ta[:pop_vs_density_2015][!,:weights] .= 10.0

    dout = DataFrame()
    for (k,v) in ta
        append!(dout, v)
    end
    dout

end

"""
    p2x(p::Param)

map param to x for objective function quick eval
this is the inverse of [`x2dict`](@ref).
"""
function p2x(p::Param)
    [ p.cbar,  p.sbar, p.a, p.γ , p.ν, p.σc]
end

function d2x(p::Dict)
    [ p[:cbar],  p[:sbar], p[:a], p[:γ] , p[:ν], p[:σc]]
end

"""
    x2dict(x)

map vector x to dict for [`Param`](@ref)
"""
function x2dict(x)
    di = Dict(:cbar => x[1],
    :sbar => x[2],
    :a => x[3],
    :γ => x[4],
    :ν => x[5],
    :σc => x[6],
    :ϕ1x => 0.15)
    di
end

"""
    objective1(;save = false)

run objective function at default parameter
"""
objective1(;save = false) = objective(p2x(Param()), showmoments = true, plot = true, save = save)

"""
    objective(x; moments = false, plot = false, save = false, fname = "moments")

GMM objective function for an optimizer
"""
function objective(x; showmoments = false, plot = false, save = false, scenario = :default)
    # unpack X
    di = x2dict(x)
    # println(di)
    p = Param(par = di, use_estimatedθ = false)


        
    # run k city model with price convergence
    # d = khet_run(writedisk = false, readdisk = false, par = di, tol = 0.2, verbose = true)
    if (scenario == :aggloλ1) || (scenario == :aggloλ2)
        di = merge(di, Dict(:λ => 0.05,:T => 1840:10:2150))
        p = Param(par = di, use_estimatedθ = false)
    elseif scenario == :aggloμ
        di = merge(di, Dict(:μ => 0.05))
        p = Param(par = di, use_estimatedθ = false)
    elseif scenario == :d1d2 
        di = merge(di, Dict(:d1 => 0.05, :d2 => 2.0, :T => 1840:10:2020))
        p = Param(par = di, use_estimatedθ = false)
    elseif scenario == :sample2 
        di = merge(di, Dict(:data_input => "sample2"))
        p = Param(par = di, use_estimatedθ = false)
    elseif scenario != :default
        error("invalid scenario: $scenario")
    end
    
    d = try
        khet_estimate_impl(20, iterateP = ((scenario == :d1d2) || (scenario == :aggloλ2)) ? 0 : 2, par = di, verbose = false, irrelevantλ = scenario != :aggloλ2 )
    catch 
        @info "error at $(di)"
        return Inf
    end

    mo = moments(p, d)

    if showmoments
        vv = sum(mo.weights .* (mo.data .- mo.model).^2)
        if plot
            po = dashboard(d,aggregated = true)
            if save
                savefig(po, joinpath(dbplots(),"$(scenario)_estimation.pdf"))
            end
            return (vv , mo, po)
        else
            return (vv , mo)
        end
    else
        return sum(mo.weights .* (mo.data .- mo.model).^2)
    end
    
end

function print_latex_moments()
    b = khet_run(readdisk = true)
    m = moments(Param(),b)
    latex_moments(m)
end

function latex_moments(d::DataFrame; fname = "moments")

    sanitize(x) = replace(x, "_" => "\\_")
    getline(x;digits = 4) = [sanitize(x[:moment]), round(x[:data],digits = digits), round(x[:model],digits = digits), x[:weights]]
    getline2(x;digits = 4) = if x[:moment] == "density_decay_MSE"
        [sanitize(x[:moment]), "  -", round(x[:model],digits = digits)]
    else
        [sanitize(x[:moment]), round(x[:data],digits = digits), round(x[:model],digits = digits)]
    end

    d1 = subset(d, :weights => x -> x .> 0)
    d2 = subset(d, :weights => x -> x .== 0)

	latex_tabular(joinpath(dbtables(),"$fname.tex"), Tabular("l D{.}{.}{1.3}@{}  D{.}{.}{3.3}@{}  D{.}{.}{8.2}@{}"), [
	   Rule(:top),
       ["Moment", MultiColumn(1,:c,"Data"), MultiColumn(1,:r,"Model") , MultiColumn(1,:r,"Weight")],
       Rule(:mid),
       [getline(i) for i in eachrow(d1)]...,
       Rule(:bottom)
	   ]
	)

    latex_tabular(joinpath(dbtables(),"$fname-nontarget.tex"), Tabular("l D{.}{.}{1.3}@{}  D{.}{.}{3.3}@{}"), [
        Rule(:top),
        ["Moment", MultiColumn(1,:c,"Data"), MultiColumn(1,:r,"Model")],
        Rule(:mid),
        [getline2(i,digits = 3) for i in eachrow(d2)]...,
        Rule(:bottom)
        ]
     )
end

# estimation results from estimation on cluster
# parsμ() = [0.7064284368132546, 0.195526699823356, 1.65, 0.30046219285914055, 0.02750675492447498]
parsμ() = x2dict([0.67763, 0.160045, 1.50008, 0.300928, 0.01, 1.98448])
# parsμ() = Dict(:a => 1.504160404904378, :γ => 0.2969975089309196, :ν => 0.021367028683042626, :sbar => 0.16313949316671814, :cbar => 0.674933404227715, :σc => 1.4046417815014929, :ϕ1x => 0.15)
parsλ1() = x2dict([0.674259, 0.169109, 1.70999, 0.304741, 0.0256059, 0.886771])
parsλ2() = x2dict([0.671618, 0.167536, 1.71136, 0.302281, 0.0263421, 1.15116])
# parsd1d2() = [0.7037819447182618, 0.1950413168212306, 2.3329995762094993, 0.30129049243630923, 0.027069140123780547]  # version with d2=2.25
# parsd1d2() = [0.7035782722558146, 0.1959896376001289, 2.230196518444022, 0.29340052169828595, 0.0240332167373242]  # version with d2=2
# parsd1d2() = Dict(:a => 2.2795049996363455, :γ => 0.2946196209344721, :ν => 0.024104025924067515, :sbar => 0.1950401882762317, :cbar => 0.7029656620016426, :σc => 0.7810695546610217, :ϕ1x => 0.15)
parsd1d2() = x2dict([0.659191, 0.17053, 2.08522, 0.305336, 0.0273266, 1.91266])
# parsd1d2() = [0.7035782722558146, 0.1959896376001289, 2.230196518444022, 0.29340052169828595, 0.027 ] # version with d2=2 but nu higher
# parsd1d2() = [0.7035782722558146, 0.1959896376001289, 2.230196518444022, 0.29340052169828595, 0.029 ] # version with d2=2 but nu even higher
parssample2() = [0.675743, 0.17, 1.71032, 0.299935, 0.0229991, 0.98211]

# starting value 2023-01-28
estim_start() = Dict(
  :a    => 1.6653,
  :γ    => 0.3001,
  :ν    => 0.027501,
  :sbar => 0.195598,
  :cbar => 0.70635,
  :ϕ1x  => 0.15
)

# malbec_ces() = Dict(:a => 1.6532336913168784, :γ => 0.29865460716630293, 
# :ν => 0.020168178623773233, :sbar => 0.19512137871337717, 
# :cbar => 0.703514466165511, :σc => 1.1461934545943437, :ϕ1x => 0.15)

malbec_ces() = Dict(:a => 1.6883422541695259, :γ => 0.30122142499462845, :ν => 0.022007011125890943, :sbar => 0.1708174340649497, :cbar => 0.6777258996849997, :σc => 1.0086678018068012, :ϕ1x => 0.15)





"""
    runestim(;steps = 1000,fname = "moments")

Run the default differential evolution optimizer from [BlackBoxOptim.jl](https://github.com/robertfeldt/BlackBoxOptim.jl)

"""
function runestim(;steps = 200,savefreq = 50,method = :dxnes, fname = "moments", scenario = :default)
    # check slack
    post_slack("[LandUse.jl] job started")
    post_file_slack()

    # set seed
    Random.seed!(20230101)

    # mm = :dxnes

    # get a reasonable starting value for each scenario
    # amounts to chosing base cose param a in a way that solution is feasible
    x0 = if scenario == :d1d2
        p2x(Param(par = merge(estim_start(),Dict(:a => 2.335,:cbar => 0.704))))
    elseif scenario == :aggloλ1
        p2x(Param(par = merge(estim_start(),Dict(:a => 1.7))))
    elseif scenario == :aggloλ2
        p2x(Param(par = merge(estim_start(),Dict(:a => 1.72))))
    elseif scenario == :aggloμ
        p2x(Param(par = merge(estim_start(), Dict(:a => 1.65))))
    else  # default (i.e. also sample2 goes here)
        p2x(Param(par = estim_start()))
    end

    # do savefreq steps at a time
    if steps < savefreq
        println("number of workers: $(workers())")
        optctrl = bbsetup(x -> objective(x, scenario = scenario) ; SearchRange = collect(values(search_over( scenario = scenario))),MaxSteps = steps, Workers = workers(), Method = method,TraceMode = :verbose, lambda = length(workers()))
        res100 = bboptimize(optctrl,x0)
        best  = best_candidate(res100)

        # Now serialize to a temp file:
        fp = joinpath(@__DIR__,"..","out","bboptim_$(scenario)_$(Dates.today()).dat")
        fh = open(fp, "w")
        serialize(fh, (optctrl, res100))
        close(fh)
        post_slack("[LandUse] estimation method $method finished scenario $scenario after $steps steps")

    else
        stepsdone = 0
        msg = "[LandUse.jl] doing steps $(stepsdone) -> $(stepsdone + savefreq)"
        println(msg)
        # msg2 = "Best candidate after $steps steps: $(best)"
        # println(msg2)
        # post_slack(msg)
        # post_slack(msg2)
        optctrl = bbsetup(x -> objective(x, scenario = scenario) ; SearchRange = collect(values(search_over( scenario = scenario))),MaxSteps = savefreq, Workers = workers(), Method = method,TraceMode = :verbose, lambda = length(workers()))
        res100 = bboptimize(optctrl,x0)
        best  = best_candidate(res100)
        stepsdone = stepsdone + savefreq
        println("doing steps after $(stepsdone) : ",best)
        println("saving")
        # Now serialize to a temp file:
        fp = joinpath(@__DIR__,"..","out","bboptim_$(stepsdone).dat")
        fh = open(fp, "w")
        serialize(fh, (optctrl, res100))
        close(fh)
        while stepsdone < steps
            # continue
            msg = "[LandUse.jl] doing steps $(stepsdone) -> $(stepsdone + savefreq)"
            println(msg)
            msg2 = "Best candidate after $steps steps: $(best)"
            post_slack(msg)
            post_slack(msg2)
            # start at best candidate from before
            optctrl = bbsetup(x -> objective(x, scenario = scenario) ; SearchRange = collect(values(search_over( scenario = scenario))),MaxSteps = savefreq, Workers = workers(), Method = method,TraceMode = :verbose, lambda = length(workers()))
            res100 = bboptimize(optctrl,best)
            best  = best_candidate(res100)


            stepsdone = stepsdone + savefreq
            println("best after $(stepsdone) : ",best)
            println("saving")
            # Now serialize to a temp file:
            fp = joinpath(@__DIR__,"..","out","bboptim_$(stepsdone).dat")
            fh = open(fp, "w")
            serialize(fh, (optctrl, res100))
            close(fh)
            # post_slack("[LandUse] estimation method $method finished after $steps steps")

        end
        # final save:
        rm(fp)
        fp = joinpath(@__DIR__,"..","out","bboptim_$(scenario)_$(Dates.today()).dat")
        fh = open(fp, "w")
        serialize(fh, (optctrl, res100))
        close(fh)
    end

    
    # try 
        x,m,pl = objective(best, showmoments = true, plot = true, save = true,scenario = scenario)
        txt = """
        [LandUse.jl] Estimation finished on $(gethostname()) $method scenario $scenario after $steps steps:

        *Results:*
        ========

        *best candidate*: 
        ```
        $(x2dict(best))
        ```

        *best moments*:
        ```
        $m
        ```
        """

        println(txt)
        post_file_slack(joinpath(dbplots(),"$(scenario)_estimation.pdf"))
        post_slack(txt)

        return res100
    # catch
    #     println("final eval of objective failed")
    #     txt = """
    #     [LandUse.jl] Estimation finished on $(gethostname())

    #     *Results:*
    #     ========

    #     *best candidate*: 
    #     ```
    #     $(x2dict(best))
    #     ```
    #     """
    # end

    
end

"Estimate exponential model on spatial density. "
function expmodel(xdata,ydata)
    mod2(x,par) = par[1] * exp.(par[2] .* x)
    out = curve_fit(mod2, xdata, ydata , [1.0, -0.3])
    (coef(out), out)
end

function comp_expmodels(b0,grad)
    xdata = range(0, stop=10, length=20)
    ydata = b0 .* exp.(grad .* xdata) + 0.01*randn(length(xdata))

    # model 1 normalizes data to first point
    m1(x,par) = exp.(par .* x)
    e1 = coef(curve_fit(m1, xdata, ydata ./ ydata[1], [-0.3]))

    # model 2 does nothing
    m2(x,par) = par[1] * exp.(par[2] .* x)
    e2 = coef(curve_fit(m2, xdata, ydata, [1.0,-0.3]))

    p1 = scatter(xdata, ydata ./ ydata[1], title = "normalized",leg = false)
    plot!(p1, x -> exp.(e1[1] * x), 0, 10, annotations = ([7],[0.7],["exp coef=$(round(e1[1],digits=1))"]))

    p2 = scatter(xdata, ydata , title = "true",leg = false)
    plot!(p2, x -> e2[1] * exp.(e2[2] * x), 0, 10, annotations = ([7],[0.7 * b0],["exp coef=$(round(e2[2],digits=1))"]))

    plot(p1,p2)
end