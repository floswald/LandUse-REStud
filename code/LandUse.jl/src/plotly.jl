
using PlotlyJS
using DataFrames
using CSV


loaddata() = CSV.read(joinpath(@__DIR__,"d1d2detail.csv"), DataFrame)

function jplot(x::Symbol, y::Symbol,d::DataFrame; dolog = true)
    d = if dolog 
        transform(d, [x,y] .=> (z -> log.(z)) .=> [x,y],
                     [:LIBGEO,:year] => ByRow((x,y) -> "$x ($y)") => :LIBGEO_year)
    else
        transform(d, 
                     [:LIBGEO,:year] => ByRow((x,y) -> "$x ($y)") => :LIBGEO_year)
    end
  
    ex = extrema(d[!,x])
    ey = extrema(d[!,y])

    p = PlotlyJS.plot(
        d, x = x, y = y, mode="markers", text = :LIBGEO_year, color= :year,
        kind = "scatter",labels=attr( year="Year"),
    Layout(title="$x vs $y"))
    # PlotlyJS.plot([p, PlotlyJS.scatter(x = minimum([ex[1],ey[1]]), y = maximum([ex[2],ey[2]]), mode = "lines")])

end



d = loaddata()

di = Dict()
di[:Lu] = jplot(:Lu_d1d2, :Lu, d)
di[:Lr] = jplot(:Lr_d1d2, :Lr, d)
di[:Sr] = jplot(:Sr_d1d2, :Sr, d)
di[:area] = jplot(:cityarea_d1d2, :cityarea, d)
di[:density] = jplot(:citydensity_d1d2, :citydensity, d)

