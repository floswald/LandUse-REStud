

function make_grid_df(p0::Param,n; center_on_p0 = false)
    so = search_over()
    arrs = []
    if center_on_p0
        for (k,v) in so
            gup   = range(getfield(p0,k), stop = v[2], length = div(n, 2))  #
            gdown = range(getfield(p0,k), stop = v[1], length = div(n, 2))[2:end]
            push!(arrs, [gup; gdown])
        end
    else
        for (k,v) in so
            push!(arrs, range(v[1], stop = v[2], length = n))
        end
    end
    d = DataFrame(gridmake(arrs...), collect(keys(so)) )
    d.id = 1:nrow(d)
    d

end

function grid_arr(p::Param,n)
    x = make_grid_df(p,n)
    Array(x[!,1:(ncol(x)-1)])
end

function eval_grid(g::Matrix)
    n = size(g)[1]
    r = zeros(n)
    @showprogress for i in 1:n
        r[i] = objective(g[i,:])
    end
    r
end

function worker_receiver(ids::Vector{Int},n::Int)
    # ids are indices to work on for this worker
    jobs = grid_arr(Param(),n)
    out = zeros(length(ids))
    for ix in 1:length(ids)
        out[ix] = objective(jobs[ix,:])
    end
    return out
end




function p_obj_grid(g::SharedArray)
    n = size(g)[1]
    out = @showprogress pmap(1:n) do x
        objective(g[x,:])
    end
    out
end

function p_start_grid(g::SharedArray)
    n = size(g)[1]
    out = @showprogress pmap(1:n) do x
        di = x2dict(g[x,:])
        p = Param(par = di, use_estimatedθ = false)
        try 
            x1,M1,p1 = LandUse.run(p, estimateθ = false)
            return x1[1]
        catch
            return (ρr = missing, ϕ = missing, r = missing, Lr = missing, pr =missing, Sr = missing, θu = missing, θr = missing)
        end
    end
    out
end

function clean_p_start(x)
    d = DataFrame

end

function parallel_starts(; npoints = 7)
    
    # create shared Array
    sg = SharedArray{Float64,2}(grid_arr(Param(),npoints))

    # run in parallel
    r = p_start_grid(sg)

    # add to grid, save and return
    sgr = [DataFrame(sg) DataFrame(r)]
    CSV.write(joinpath(@__DIR__,"..","out","par_starts.csv"), sgr)
    post_slack("done on $(gethostname()) with learning")
    sgr

end

function make_input_output()
    sgr = CSV.read(joinpath(@__DIR__,"..","out","par_starts.csv"), DataFrame)
    dropmissing!(sgr)
    a = Array(sgr)
    input = a[:,1:length(search_over())]
    output = a[:,(length(search_over())+1):(size(a,2)-2)]
    writedlm(joinpath(@__DIR__,"..","out","input.txt"), input)
    writedlm(joinpath(@__DIR__,"..","out","output.txt"), output)
end

function prep_learning(; npoints = 7)
    x = parallel_starts(npoints = npoints)
    make_input_output()
end

function parallel_obj(; npoints = 7)
    
    # create shared Array
    sg = SharedArray{Float64,2}(grid_arr(Param(),npoints))

    # run in parallel
    r = p_obj_grid(sg)

    # add to grid, save and return
    sg = hcat(sg,r)
    writedlm(joinpath(@__DIR__,"..","out","par_grid.txt"), sg)
    post_slack("done on scpo-floswald with learning")
    sg

end

# also need to permutedims and exchange which is the first column!
# only first col will work, because we start from know solution and extrapolte
function shared_grid(x::DataFrame) 
    a = Array(x[!,1:(ncol(x)-1)])
    a = hcat(a, fill(NaN,size(a)[1]))
    SharedArray{Float64,2}(a)
end



# get all reachable starting points from x0
# given our current starting value in startval(p) - which is constant - 
# compute for a grid of params all solutions that converge and record their first period solution
# from that dataset - param input and implied solution

"""
checks whether a certain row of the grid together with a starting value x0 
produces a valid solution in all periods.
"""
function isfeasible_grid_row(r::Array,x0::NamedTuple)
    p1 = Param(par = x2dict(r[1:(end-1)]))
    try
        x1,M1,p2 = run(x0,p1)
        return 1
    catch
        return 0
    end

end



function gridsearchPOC(n)

    # 1. starting value we know works is
    p0 = Param()
    x0,M0,p0 = run(p0)   # x0[1] is first period of a working solution 

    solsup = []
    solsdown = []

    # 2. make grids
    so = search_over()
    for (k,v) in so
        gup   = range(getfield(p0,k), stop = v[2], length = n)
        gdown = range(getfield(p0,k), stop = v[1], length = n)
        # go up 
        push!(solsup, x0[1])
        for (ip,p) in enumerate(gup[2:end])
            println(p)
            p1 = Param(par = Dict(k => p))
            x1,M1,p2 = run(solsup[ip],p1)
            push!(solsup, x1[1])
        end
        push!(solsdown, x0[1])

        for (ip,p) in enumerate(gdown[2:end])
            println(p)
            p1 = Param(par = Dict(k => p))
            x1,M1,p2 = run(solsdown[ip],p1)
            push!(solsdown, x1[1])
        end
    end
    [solsup ; solsdown]
end
