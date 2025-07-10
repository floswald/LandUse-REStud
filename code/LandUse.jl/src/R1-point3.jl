# confront calibrated urban productivity in the model with a measure from the data.

function R1_point3()

    b = @chain khet_run(readdisk = true) begin
        interp_model_data(_)
        subset(:year => ieq(2015))
    end  # model baseline 

    casd = @chain CSV.read( joinpath(dbcasd() ,"2022-07-08","tables","paneledp2019-avgsalary-au.csv" ), DataFrame,types = Dict(:AU2010 => String)) begin
        subset(:year => ieq(2015))
        select(:AU2010, :logmean_netsalaire)
    end

    # labels
	sizebin_labels = ["[0,40K]","[40K,80K]","[80K, 120K]","[120K, 200K]","> 200K"]
	sizebins = [0,40_000,80_000,120_000,200_000,Inf]

    # get CODGEO-AU2010 mapping
    p = Param()
    camap = @chain p.citylist begin
        subset(:year => ieq(2015))
        select(:AU2010 => ByRow(x -> lpad(x,3,"0")) => :AU2010, :CODGEO) 
    end
    

	b_bins = @chain b begin
		transform( :pop_data => (x -> cut(x, sizebins,
		labels = sizebin_labels, extend = false)) => :pop_bin)
		select(:CODGEO,:pop_bin,)
		leftjoin(select(b, :thetau => :θu, :CODGEO, :LIBGEO), on = :CODGEO)
        leftjoin(camap, on = :CODGEO)
        leftjoin(casd, on = :AU2010)
        # select(:CODGEO,:AU2010,:pop_bin, :wu0)
	end

    CSV.write(joinpath(dbtables(),"R1-point3-DADS-model.csv"), b_bins)

    d1 = @chain b_bins begin
        transform(:θu => (x -> log.(x)) => :wage, :logmean_netsalaire => :wage_data)
        transform([:wage,:wage_data] => ((x,y) -> (x .- mean(x) .+ mean(y))) => :wage)
    end
    p1 = scatter(d1.wage, d1.wage_data, xlabel = "Urban wage Model", ylabel = "Urban wage Data", legend = false, lims = symlims(d1.wage,d1.wage_data,0.1))
    [annotate!(p1, r.wage + 0.01, r.wage_data, Plots.text.(r.LIBGEO, :left, 8)) for r in eachrow(d1)]
    plot!(x -> x, color = :black, label = "")
  
    

    # by groups
    p2 = @chain b_bins begin
        groupby(:pop_bin)
        combine(:θu => (x -> mean(log.(x))) => :wage, :logmean_netsalaire => mean => :wage_data)
        transform([:wage,:wage_data] => ((x,y) -> (x .- mean(x) .+ mean(y))) => :wage)
        scatter(_.wage, _.wage_data, group = _.pop_bin, xlabel = "Urban wage Model", ylabel = "Urban wage Data", legend = :outerright)
        plot!(x -> x, color = :black, label = "")
    end

    savefig(p1 , joinpath(dbletters(),"R1-point3-1.pdf"))
    savefig(p2 , joinpath(dbletters(),"R1-point3-2.pdf"))
    p1,p2
end