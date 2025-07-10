

"""
issue 168: population weighting

https://github.com/floswald/LandUse.jl/issues/168

1. share of parisian population by year
2. check on modvsdata(citydensity, data_density) also report changes in R2 , with/out paris
3. perform same check for fig B13 (d1d2 density modvsdata), with/out paris
4. Column 1 of table 2 does not change if we weigh by population

"""
function i168()

    # 1. share of parisian population by year
    popshare = @chain khet_run(readdisk = true) begin
        subset(:year => leq(2020))
        groupby(:year) 
        combine([:Lu,:LIBGEO] => ((x,y) -> x[y .== "Paris"] / sum(x) ) => :share_paris)
    end

    # 2. check on modvsdata(citydensity, data_density) also report changes in R2
    di = @chain khet_run(readdisk = true) begin
        interp_model_data(_)
    end
    di_nop = subset(di, :LIBGEO => ByRow(!=("Paris")))
    slopes_r2 = Dict(
        :all => Dict(
        "density" => check_modvsdataslopes_wtd(di,:citydensity,:density_data),
        "area"    => check_modvsdataslopes_wtd(di,:cityarea,:area_data),
        "Lu"      => check_modvsdataslopes_wtd(di,:Lu,:pop_data)),
        :noparis => Dict(
            "density" => check_modvsdataslopes_wtd(di_nop,:citydensity,:density_data),
            "area"    => check_modvsdataslopes_wtd(di_nop,:cityarea,:area_data),
            "Lu"      => check_modvsdataslopes_wtd(di_nop,:Lu,:pop_data)
        )
        )
    


    # 3. perform same check for fig B13 (d1d2 density modvsdata)
    d1d2 = sensitivity_d1d2(returnplots = false, readdisk = true)[Symbol("d0=0.05, d1=2.0")] |> interp_model_data
    d1d2_nop = subset(d1d2, :LIBGEO => ByRow(!=("Paris")))

    slopes_r2_d1d2 = Dict(
        :all => Dict(
        "density" => check_modvsdataslopes_wtd(d1d2,:citydensity,:density_data),
        "area"    => check_modvsdataslopes_wtd(d1d2,:cityarea,:area_data),
        "Lu"      => check_modvsdataslopes_wtd(d1d2,:Lu,:pop_data)),
        :noparis => Dict(
            "density" => check_modvsdataslopes_wtd(d1d2_nop,:citydensity,:density_data),
            "area"    => check_modvsdataslopes_wtd(d1d2_nop,:cityarea,:area_data),
            "Lu"      => check_modvsdataslopes_wtd(d1d2_nop,:Lu,:pop_data))
        )

    # 4. column 1 of table 2 does not change if weigh. well.
    # x = LandUseR:::table1()
    # x[[1]][["Model (2)"]]
    # x[[3]][["Model (2)"]]
    # x[[1]][["Data (5)"]]
    # x[[3]][["Data (5)"]]

    o = Dict(:pop_share => popshare, :slopes_r2 => slopes_r2, :slopes_r2_d1d2 => slopes_r2_d1d2)

    @info "printing output"
    @info "pop share is for all 100 cities, not just the ones in the model."
    @show o[:pop_share]

    println()
    @info "model vs data, slopes and R2"
    @info "======================================="
    for z in ["density","area","Lu"]
        println()
        @info "$z: all cities"
        println(json(o[:slopes_r2][:all][z],4))
        @info "$z: no paris"
        println(json(o[:slopes_r2][:noparis][z],4))
    end
    println()
    @info "model vs data, slopes and R2 in d1d2"
    @info "======================================="
    for z in ["density","area","Lu"]
        println()
        @info "$z: all cities"
        println(json(o[:slopes_r2_d1d2][:all][z],4))
        @info "$z: no paris"
        println(json(o[:slopes_r2_d1d2][:noparis][z],4))
    end
   
    o
end