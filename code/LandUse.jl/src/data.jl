

"""
Read Input Dataset from Disk
"""
function get_data(;topn = 100)
    @chain CSV.read(joinpath(dbpath(),"data","statistique-agricole","petite-region-agricole","pra-output-$topn.csv"),DataFrame, types = Dict(:CODGEO => String)) begin
        append!(prices1892(), cols = :union)
        # throw out strasbourg
        subset(:LIBGEO => x -> x .!= "Strasbourg")
        # throw out missing prices
        dropmissing(:price)
        subset(:rank => LandUse.leq(topn))

        # normalize by paris prices in each year:
        groupby(:year)
        transform([:price, :CODGEO] => ((x,y) -> normx_by_y(x, y .== "75060")) => :n_price)

        # smooth prices
        groupby(:CODGEO)
        transform(:n_price => (x -> exp.(smooth(log.(Array(x)), 5))) => :s_price_data)
    end
end

function plot_smoothed_prices(d)
    n = length(unique(d.CODGEO))
    sort!(d,:year)
	pl = @df d plot(:year, :s_price_data, group = :LIBGEO, leg = n == 20 ? :outerright : false, color = reshape(StatsPlots.distinguishable_colors(n), 1,n), label = "",lw = 2)
	@df d scatter!(pl,:year, :n_price, group = :LIBGEO,color = reshape(StatsPlots.distinguishable_colors(n), 1,n), legendfontsize = 8)
    pl
end

function prices1892()
	# this has 1876 population data!
	x = @chain CSV.read(joinpath(dboutdata(), "relpop-100.csv"), DataFrame, types = Dict(:CODGEO => String,:DEP => String)) begin
        subset(:year => ieq(1876))
        transform(:DEP => (x -> lpad.(x, 2,"0")) => :DEP)
    end

	@chain CSV.read(joinpath(dbpath(),"data","statistique-agricole","departement","cleaned-output","table1-1892.csv"), DataFrame, types = Dict(:dep => String)) begin
        select( :dep => :DEP, :dep_clean, :year, :francs_per_hectare => :price)
        innerjoin(x, on = [:DEP,:year])
        select(:CODGEO, :LIBGEO,:DEP, :dep_clean => :LIBDEP, :year, :area_data, :price, :pop_data, :rank, :density_data, :pop_1975)
	end
end

function price_area_data_1876()
    p1872 = @chain popdata() begin
                filter(row -> row.year == 1876, _)
                select(:CODGEO, :LIBGEO, :DEP => :dep, :area_data => :area, :area_data => maxnorm => :area_n, :pop_data => :pop)
    end
    @chain CSV.read(joinpath(dbpath(),"data","statistique-agricole","departement","cleaned-output","table1-1892.csv"), DataFrame, types = Dict(:dep => String)) begin
        select(_, :dep, :dep_clean, :year, :francs_per_hectare => :ρr)
        innerjoin(p1872, on = :dep)
        select(:CODGEO, :LIBGEO, :dep => :DEP, :dep_clean => :LIBDEP, :year, :area, :area_n, :ρr, :pop)
    end
end

"1950 departement level land prices"
function price_area_data_1950()
    @warn "there's a weird join going on here. caution!"
    p1950 = @chain popdata() begin
                filter(row -> row.year == 1950, _)
                select(:CODGEO, :LIBGEO, :DEP => :dep, :area_data => :area, :area_data => maxnorm => :area_n, :pop_data => :pop, :year)
    end
    p = @chain CSV.read(joinpath(dbpath(),"data","statistique-agricole","departement","cleaned-output","1950-1968-p55-p121.csv"), DataFrame, types = Dict(:dep => String)) begin
        dropmissing(:dep)
        subset(:kind => x -> x.== "labourable")
        select(_, :dep, :departement, "1950" => :ρr)
    end
    #     innerjoin(p1950, on = :dep)
    #     select(:CODGEO, :LIBGEO, :dep => :DEP, :departement => :LIBDEP, :year, :area, :area_n, :ρr, :pop)
    # end
    (p1950,p)
end


function lininterp(x,y,newx::Number)
    i = extrapolate(interpolate((x,), y, Gridded(Linear())), Flat())
    i(newx)
end
function lininterp(x,y,newx::Array)
    o = zeros(length(newx))
    for i in eachindex(o)
        o[i] = lininterp(x,y,newx[i])
    end
    o
end


function areaprice_estim(p::Param)
    # get relevant data or interpolate it
    ydata = interp_data(p.citylist,p.T[p.it])
    sort!(ydata, :rank)
    ydata.rank = 1:nrow(ydata)
    transform!(ydata, :pop_data => (x -> x ./ sum(x)) => :popwgt )
    popwgt = ydata[!,:pop_data]  ./ sum(ydata[!,:pop_data])
    relpop = ydata[!,:pop_data]  ./  ydata[1,:pop_data]
    
    edata = select(ydata, :CODGEO, :LIBGEO,:s_price_data, :rank, :year,:area_data, :pop_data,:density_data)
    edata.popwgt = popwgt
    edata.relpop = relpop

    edata
end

"""
Returns measured area and price data (if `year` coincides with observed date)
or returns a linearly interpolated value using the closest dates.
"""
function interp_data(d::DataFrame,year::Int)
    # d = @chain areapricedata(departement = departement1950) begin
    #     transform(:s_price => :ρr_n)  # smoothed price is called like that
    # end
    if year ∈ d.year 
        @chain d begin
            subset(:year => x -> x .== year)
        end
    else
        dx = @chain d begin
            sort([:CODGEO, :year])
            groupby(:CODGEO)
            combine( :year => (x -> year) => :year, 
                    :LIBGEO  => first => :LIBGEO,
                    :rank => first => :rank,
                    [:year,:s_price_data] => ((x,y) -> lininterp(x,y,year)) => :s_price_data,
                    [:year,:n_price] => ((x,y) -> lininterp(x,y,year)) => :n_price,
                    [:year,:pop_data] => ((x,y) -> lininterp(x,y,year)) => :pop_data,
                    [:year,:area_data] => ((x,y) -> lininterp(x,y,year)) => :area_data)
        end
        dx.density_data = dx.pop_data ./ dx.area_data
        dx
    end
end

function areapricedata_all()
    p = Param()

    df = DataFrame()
    for iy in 1840:10:2020
        append!(df, interp_data(p.citylist,iy), cols = :union)
    end
    df
end
function plot_areapricedata()
    
    ap = areapricedata_all()  # interpolated data at all dates
    n = length(unique(ap.LIBGEO))

    od = popdata()   # original data on observed dates

    # price
    p = @df subset(ap, :LIBGEO => neq("Paris")) plot(:year,:s_price_data,group = :LIBGEO, leg = false,xticks = 1840:20:2020, color = dcol(n-1),label = "", title = "Relative Land Price", yscale = :log10, yticks = [0.5,1,2],yformatter = x -> string.(round(x,digits = 2)), lw = 2)
    # for price scatter, modify year to 1892
    od[!,:year] = ifelse.(od.year .== 1876, 1892,od.year)
    @df od scatter!(p,:year, :n_price, group = :LIBGEO,color = dcol(n), legendfontsize = 9, leg = :outerright,markersize = 5)

    # pop
    ap = @chain ap begin
        groupby(:year)
        combine([:pop_data,:LIBGEO] => ((x,y) -> normx_by_y(x, y .== "Paris")) => :relpop,
        :LIBGEO => :LIBGEO)
        subset(:LIBGEO => neq("Paris"))
    end

    po = @df ap plot(:year,:relpop,group = :LIBGEO, leg = false,xticks = 1840:20:2020, color = dcol(n-1),label = "" , title = "Relative Urban Population", yscale = :log10, yticks = [0.01,0.05,0.15],yformatter = x -> string.(round(x,digits = 2)),lw = 2)

    o = plot(po, p, size = (1200,500), top_margin = 0.5Plots.cm,titlefont = 12, layout = @layout [a{0.43w} b{0.57w}])

    savefig(o,joinpath(dbplots(),"revision$(revision())","baseline","input-pop-prices.pdf"))
    o
end


function highfreq_rawdata()
    d = DataFrame(CSV.File(rawdata_loc()))

    v = @view d[d.year .== 1840,:]
	replace!(v.Comm_speed_Paris, missing => 1)

    @chain d begin

        subset(:year => x -> (x .> 1839) .& (x .<= 2020))
        transform(
            [:P_rural_Sauvy, :P_rural_Toutain] => ((x,y) -> (x .^ 0.5) .* (y .^ 0.5)) => :P_rural)
        transform(
            ["P_rural", "year"] => ((x,y) -> normx_by_y(x, y .== 1950)) => "P_rural",
            ["P_rural_Sauvy", "year"] => ((x,y) -> normx_by_y(x, y .== 1950)) => "P_rural_Sauvy",
            ["P_rural_Toutain", "year"] => ((x,y) -> normx_by_y(x, y .== 1950)) => "P_rural_Toutain",
            )
        # innerjoin(
        #     CSV.read(joinpath(dbdata(),"paris-commuting","commspeed-paris.csv"), DataFrame),
        #     on = :year
        # )
    end
end

# function areapricedata( ; overwrite=false, departement = false)
#     # return CSV.read(joinpath(dbpath(),"data","statistique-agricole","petite-region-agricole","area-pop-prices-all-years-top-100.csv"),DataFrame, types = Dict(:CODGEO => String))
#     if overwrite
#         if departement
#             d = @chain CSV.read(joinpath(dbpath(),"data","statistique-agricole","petite-region-agricole","area-pop-prices-all-years-top-100.csv"),DataFrame, types = Dict(:CODGEO => String)) begin
#                 select(_ , :CODGEO,:LIBGEO, :DEP, :year, :price, :area, :pop)
#                 # dropmissing(:price)
#                 subset(:year => x -> x .!= 1950)  # drop 1950 PRA data
#                 rename(:price => :ρr)
#                 append!(price_area_data_1876(), cols = :union)
#                 append!(price_area_data_1950(), cols = :union)
#             end
#         else
#             d = @chain CSV.read(joinpath(dbpath(),"data","statistique-agricole","petite-region-agricole","area-pop-prices-all-years-top-100.csv"),DataFrame, types = Dict(:CODGEO => String)) begin
#                 # filter(row -> row.year == year, _)
#                 select(_ , :CODGEO,:LIBGEO, :DEP, :year, :price, :area, :pop)
#                 # dropmissing(:price)
#                 rename(:price => :ρr)
#                 append!(price_area_data_1876(), cols = :union)
#             end
#         end
#         CSV.write(joinpath(intables, "area-prices-departement-$(departement).csv"), d )
#     else
#         d = CSV.read(joinpath(intables, "area-prices-departement-$(departement).csv"), DataFrame, types = Dict(:CODGEO => String) )
#     end
#     d  
# end
function areapricedata_check(y1,y2)

    d = areapricedata(overwrite = true)
    p1 = @chain d begin
        subset(:year => x -> (x .== y1) .| (x .== y2))
        @df plot(:year, :ρr, group = :CODGEO)
    end
    p2 = @chain d begin
        subset(:year => x -> (x .== y1) .| (x .== y2))
        @df plot(:year, :ρr_n, group = :CODGEO)
    end
    l = @chain d begin
        subset(:year => x -> (x .== y1) .| (x .== y2))
        groupby(:CODGEO)
        combine(:ρr_n => (x -> abs.(diff(x))) => :ρ_d)  # growth in price
        transform(:ρ_d => (x -> competerank(x, rev = true)) => :ρ_d_rank)  # largest growth in prices
    end
    return d,l,p1,p2
    # @chain d begin
    #     subset(:CODGEO => (x -> in.(x, Ref(l[l.ρ_d_rank .< 3,:CODGEO]))), :year => x -> x .<= max(y1,y2))
    # end

end

"""
Get aggregate 1975 population-weighted density and total sum of urban population and area from data.
Used in `[plot_het_agg`](@ref) for plotting data, not part of input to model solution.
"""
function agg_pop_density()
    d = @chain CSV.read(joinpath(dboutdata(), "relpop-100.csv"), DataFrame, types = Dict(1 => String)) begin
        groupby(:year)
        combine([:density_data, :pop_1975] => ((x,y) -> mean(x, Weights(y))) => :agg_density,
                [:pop_data,:area_data] .=> sum .=> [:agg_pop,:agg_area] )
        dropmissing()
        transform([:agg_density, :agg_pop, :agg_area] .=> [:agg_density_0, :agg_pop_0, :agg_area_0])
        transform([:agg_density, :agg_pop, :agg_area] .=> firstnorm .=> [:agg_density, :agg_pop, :agg_area])
    end
    d[!,:year] = ifelse.(d.year .== 1999, 2000, d.year)
    d
end

"Cities not to be used in exercise for idiosyncratic reasons"
# city_blacklist() = ["Marseille","Toulon","Strasbourg","Nice"]
# city_blacklist() = ["Marseille","Toulon","Strasbourg","Nice"]
city_blacklist() = ["Strasbourg","Nice"]


function citysample_plot(d::Dict)

    x = Dict()

    for (k,v) in d 
        x[k] = @chain v begin
        transform([:Area,:Population] .=> (x -> log.(x)) .=> [:la, :lp])
        transform("Rural Land Price" => "price")
        @df scatter(:la, :lp, :price, xlab = "log area", ylab = "log pop", zlab = "price", leg = false, xlims = (2,7.5),ylims = (9,16.5),zlims = (0.35,1.3), size = (500,500))
        end
    end

    for (k,v) in x
        savefig(v, joinpath(dbplots(),"revision$(revision())","sample-compare-$k.pdf"))
    end
    x


end

function citysample_comparison()
    d0 = popdata(readdisk = true, fname = "baseline")
    d1 = popdata(readdisk = true, fname = "sample2")  # recreates the second sample. kwarg seed points to correct seed.

    d0 = @chain d0 begin
        subset(:year => ieq(2000))
        select( :LIBGEO => :City,
        [:area_data, :pop_data,:s_price_data] .=> ["Area", "Population","Rural Land Price"],
        :DEP => :Departement)
        sort(:Population,rev = true)
    end

    d1 = @chain d1 begin
        subset(:year => ieq(2000))
        select( :LIBGEO => :City,
        [:area_data, :pop_data,:s_price_data] .=> ["Area", "Population","Rural Land Price"],
        :DEP => :Departement)
        sort(:Population,rev = true)
    end
    d = Dict(:baseline => d0, :sample2 => d1)



    open(joinpath(dbtables(),"sample-baseline.tex"),"w") do io
        pretty_table(io,d[:baseline], tf = tf_latex_booktabs, nosubheader = true,wrap_table = false,
        formatters = ft_printf(["%s","%3.2f","%7.1f","%1.2f"],[1,2,3,4]),alignment = [:r,:r,:r,:c, :c])
    end
    open(joinpath(dbtables(),"sample-alternative.tex"),"w") do io
        pretty_table(io,d[:sample2], tf = tf_latex_booktabs, nosubheader = true,wrap_table = false,
        formatters = ft_printf(["%s","%3.2f","%7.1f","%1.2f"],[1,2,3,4]),alignment = [:r,:r,:r,:c, :c])
    end
    d


end

function CODGEO_REG(pd::DataFrame)
    @chain pd begin
        subset(:year => x -> x.==2015)
        select(:CODGEO,:REG => ByRow(x -> lpad(string(x),2,"0")) => :REG,:DEP => ByRow(x -> lpad(string(x),2,"0")) => :DEP)
    end
end


"""
## City Chooser

function to choose cities for exercise. 
    By default selects Paris, plus `nbins - 1` randomly chosen cities from each population size bin.
    We focus in the first year (1867) population distribution.
"""
function popdata( ; readdisk = true, writedisk = false, nbins = 20, do_smooth = 1, frac_below = 0.5, fname = "baseline",random_seed = 20230118)
    if !readdisk
        d = get_data()

        # set a random seed 
        rng = MersenneTwister(random_seed)

        t1 = subset(d, :rank => ieq(1))   # paris
        tr = @chain d begin
            subset(:rank => ge(1))  # rest 
        end
        # return tr
        # sample from rest 
        # make nbins-1 bins
        sa = @chain tr begin
            subset(:year => x -> x .== minimum(x))  # first year only
        end
        m = median(sa.pop_data)  # of rest without paris
        nbins = nbins - 1 # paris is gone
        below = @chain sa begin
            # draw nbins/2 cities from a single large bin below median without replacement
            subset(:pop_data => x -> x .< m)
            combine( :LIBGEO => (x -> sample(rng,convert(Vector{String},x), Int(floor(nbins * frac_below)), replace = false)) => :citysample)
            # and other nbins/2 draws from nbins/2 quantiles above the median exactly one
        end
        above = @chain sa begin
            # draw nbins/2 cities from bins above median without replacement
            subset(:pop_data => x -> x .>= m)
            transform( :pop_data => (x -> cut(x, Int(ceil(nbins * (1 - frac_below))) ,
                                            labels = (f,t,i;leftclosed,rightclosed) -> Int(i))) => :pop_bin)
            groupby(:pop_bin)
            combine(:LIBGEO => (x -> rand(rng,filter(z -> .∉(z,Ref(city_blacklist())) ,x))) => :citysample)   # randomly choose one city from the bin, excluding invalid cities.
        end
        append!(below,above, cols = :union)
        tr = subset(tr, :LIBGEO => x -> x .∈ Ref(below.citysample))

        append!(t1,tr)
        
        
        # recompute population weights!
        d20 = transform(groupby(t1, :year), :pop_data => (x -> x / sum(x)) => :popweight) 

        if writedisk
            popdata_writelist(d20,fname = fname)
        end

    else
        # d20 = CSV.read(joinpath(intables, "data-input-$fname.csv"), DataFrame, types = Dict(:CODGEO => String, :DEP => String, :AU2010 => String))
        # not reading from the out-pipe of LandUseR was unwise. the file joinpath(intables, "data-input-$fname.csv") was not in sync. 
        # in fact, it seems data.table::fread changed the default precision of csv output.
        # the file joinpath(intables, "data-input-$fname.csv"), which was used to estimate and produce all model results was created before this upstream change. In practice, this means that the older version of the csv contained a population measure for paris in 1876 of 1.99081e6 i.e. 1,990,810 (differently to the value as set by hand on line 140 of LandUseR/R/pipeline.R, which is 1,990,813). So the old `data.table::fread` basically cut of the value `3`.
        # now, creating the replication package, there was an update on many R packages, one of them `data.table`. Turns out that this now writes `1.990813e6` to disk, i.e. with an additional digit. Hence, an improvement. However, it turns out that there is a numerical instability which arises from this tiny difference, which means that the d1d2 counterfactual crashes at 47% of runtime. So, the pragmatic solution: I manually set this back to the old value (i.e. 1_990_810 instead of 1_990_813). This is well within the margin of error of the Paris census in 1876, hence, totally innocuous.
        pl = popdata_readlist(fname = fname)
        d = get_data()
        d[(d.year .== 1876) .& (d.LIBGEO .== "Paris"), :pop_data] .= 1_990_810
        d20 = @chain d begin
            subset(:CODGEO => x -> x .∈ Ref(pl.CODGEO))
            groupby(:year)
            transform(:pop_data => (x -> x / sum(x)) => :popweight)
        end
    end
    if do_smooth == 1
        d20 = @chain begin d20
            sort(:year)
            groupby(:CODGEO)
            transform(:n_price => (x -> exp.(smooth(log.(Array(x)), 5))) => :s_price_data, ungroup = false)
            transform([:n_price, :s_price_data] => ((x,y) -> [x[1],y[2:end]...]) => :s_price_data)
        end
    elseif do_smooth == 2
        # same as before but draw a straight line from first to last value
        d20 = @chain begin d20
            sort(:year)
            groupby(:CODGEO)
            transform(:n_price => (x -> exp.(smooth(log.(Array(x)), 5))) => :s_price_data, ungroup = false)
            transform([:n_price, :s_price_data] => ((x,y) -> [x[1],y[2:end]...]) => :s_price_data, ungroup = false)
            transform([:year,:s_price_data] => ((x,y) -> lininterp([extrema(x)...],[extrema(y)...],Array(x))) => :s_price_data)
        end
    elseif do_smooth == 0 
        transform!(d20,:n_price => :s_price_data)
    end
    return d20
end

function popdata_writelist(d; fname = "baseline")
    @chain d begin
        subset(:year => x -> x.==1876)
        groupby(:CODGEO)
        combine(:LIBGEO => first => :LIBGEO)
        CSV.write(joinpath(dbpath(),"data", "citylist-$fname.csv"), _ ) 
    end
end
function popdata_readlist(; fname = "baseline")
    # CSV.read(joinpath(intables, "relpop-citylist.csv"), DataFrame) 
    x = CSV.read(joinpath(dbpath(),"data",  "citylist-$fname.csv"), DataFrame, types = Dict(:CODGEO => String)) 
    x.CODGEO = lpad.(x.CODGEO,5,"0")
    x
end

"""
find closest year in population data to model years
"""
function popdata_mapyears(p::Param)
    d = popdata()
    # find closest year in data
    datayears = unique(d.year)
    modelyears = p.T
    df = DataFrame(modelyears = modelyears, datayears = zeros(Int,length(modelyears)))
    for ir in eachrow(df)
        ir[:datayears] = datayears[argmin( abs.(datayears .- ir[:modelyears]) )]
    end
    df
end