
"""
# Parameter Struct 

This struct defines the parameter type for the model.
It reads default values from the file `src/params.json`. Those values
can be overridden by supplying a `Dict` to the constructor with `key => value` pairs:
λ
```julia
p = Param(par = Dict(:λ => 0.1))
```
creates a `Param` instance with agglomeration forces set to 0.1

"""
mutable struct Param
	γ     :: Float64 # housing weight
	ϵr     :: Float64 # housing supply elasticity in rural sector
	ϵs    :: Float64  # current slope of elasticity function
	ϵflat :: Bool  # whether impose a flat elasticity function or not
	nsteps :: Int  # num steps in thetau search
	λ     :: Float64 # agglomeration forces
	ν     :: Float64 # weight of rural good consumption on consumption composite
	cbar  :: Float64 # agr cons subsistence level
	sbar  :: Float64 # neg urba subsistence level
	α     :: Float64 # labor weight on farm sector production function
	useless     :: Float64 # useless land: non-farm, non-urban land (forests, national parks...)
	σ      :: Float64 # land-labor elasticity of substitution in farm production function
	σc     :: Float64 # elasticity of sub between urban and rural consumption
	β     :: Float64 # discount factor


	# productivity setup
	θu_g  :: Float64 # constant growth factors by sector
	θr_g  :: Float64 # constant growth factors by sector
	θut   :: Vector{Float64}  # idiosyncratic level of urban producitivity in each period
	θrt   :: Vector{Float64}  # idiosyncratic level of rural producitivity in each period
	θr    :: Float64 # current period rural sector TFP = θut[t]
	θu    :: Float64 # current period urban sector TFP

	# commuting cost setup
	ηm     :: Float64   # speed elasticity of fixed cost
	ξl     :: Float64   # location elasticity of fixed cost
	ηl     :: Float64   # derived location elasticity of fixed cost
	ξw     :: Float64   # wage elasticity of fixed cost
	ηw     :: Float64   # derived wage elasticity of fixed cost
	cτ     :: Float64   # efficiency of transport technology
	ζ      :: Float64   # valuation of commuting time in terms of forgone wages
	a      :: Float64   # implied combination of above parameters
	μ     :: Float64   # congestion parameter
	d1     :: Float64   # extension for variable commuting cost
	d2     :: Float64   # extension for variable commuting cost
	# speed thresholds
	speed_thresholds :: Vector{Float64}
	nspeeds :: Int


	L     :: Float64 # total population
	Lt     :: Vector{Float64} # total population by year
	Lflat :: Bool # whether population is not growing, default false
	T     :: StepRange{Int,Int}
	sT    :: Int # simulating model until year sT
	sS    :: Int # forware simulating model start year sT
	t     :: Int
	it     :: Int
	Ψ     :: Float64  # urban ammenities rel to rural
	int_nodes :: Int  # number of integration nodes
	int_bins :: Int  # number of bins into which to split distance
	iweights :: Vector{Float64}  # int weights
	inodes    :: Vector{Float64}  # points where to evaluate integrand (inodes scaled into [0,ϕ])
	S     :: Float64  # area of region
	ϕ1  :: Float64   # first period fringe
	ϕ1x  :: Float64   # fraction of first fringe that defines "center"

	# Country setup
	# country growth: θ_it = θ_i exp(g_i * t) * θ_ut
	K :: Int # number of regions
	kshare :: Vector{Float64} # share of space of each region
	factors :: Vector{Float64} # city specific baseline shift on aggregate theta_u
	gs :: Vector{Float64} # city specific growth shift on aggregate theta_u
	gsr :: Vector{Float64} # city specific growth shift on aggregate theta_r
	# kθu :: Dict  # collection of θu's for each region for each period
	# kθr :: Dict
	citylist :: DataFrame
	data_input :: String   # file name on disk for data input file. "baseline" by default
	data_seed_sample2 :: Int   # random seed for selecting alternative city sample
	𝕊 :: Matrix{Float64}  # estimate of future land rents in each region

	trace :: Bool  # whether to trace solver
	iters :: Int  # max iterations
	ma    :: Int  # moving average window size
	magr    :: Float64  # assumed growth for extraploating rural producivities.
	magu    :: Float64  # assumed growth for extraploating urban producivities.
	magt    :: Int  # moving avg over magt periods
	popg    :: Float64  # assumed growth for extraploating population
	psmooth :: Int  # price smoothing level : 0 is nothing
	pweight :: Vector{Float64} # weight of land prices in objective function of model solution (jump.jl)
	adjust_parisϕ :: Bool # whether to start at fringe of paris for price integration or a bit further out


	moments :: DataFrame
	thetas :: DataFrame

	# neural network of starting values
	Chain :: Flux.Chain

	function Param(;par=Dict(),use_estimatedθ = false)
        f = open(joinpath(dirname(@__FILE__),"params.json"))
        # j = JSON.parse(f,inttype = Sys.ARCH == :x86_64 ? Int64 : Int32)
        j = JSON.parse(f,inttype = Int)
        close(f)
        this = new()

		# read data from json file
        for (k,v) in j
			# if v["type"] == "region"
	            if v["value"] isa Vector{Any}
	                if k == "T"
	                	vv = v["value"]
		                setfield!(this,Symbol(k),vv[1]:vv[2]:vv[3])
		            else
		                setfield!(this,Symbol(k),convert(Vector{Float64},v["value"]))
		            end
	            else
	                setfield!(this,Symbol(k),v["value"])
	            end
			# elseif v["type"] == "numerical"
			# 	setfield!(this,Symbol(k),v["value"])
			# end
		end

		# set some defaults
		T = length(this.T)
		this.t = 1815
		this.it = 1
		this.S = 1.0  # set default values for space and population
		this.ϕ1 = NaN
		this.ηm = 1.0   # old formulation used this. now fixed at 1.0
		# this.ϕ1x = 0.5
		this.ϵflat = false
		this.Lflat = false
		this.μ = 0.0  # no congestion by default
		this.λ = 0.0  # no agglomteration by default
		this.d1 = 0.0  # recovers baseline commuting distances
		this.d2 = 0.0  # 
		this.psmooth = 1   # baseline smoothing level
		this.pweight = ones(length(this.T))   # baseline weight
		this.adjust_parisϕ = false

		this.data_input = "baseline"  # by default, baseline city sample
		this.data_seed_sample2 = 20230118  # sample2 seed


		# read data from disk
		# this.thetas = select(CSV.read(joinpath(dbtables(),"thetas_data.csv"), DataFrame), :year , :stheta_rural => :thetar, :stheta_urban => :thetau)
		# moments = CSV.read(joinpath(intables,"data-moments-backup.csv"), DataFrame)
		moments = CSV.read(joinpath(intables,"data-moments.csv"), DataFrame)
		this.thetas = select(moments, :year , :stheta_rural => :thetar, :stheta_urban => :thetau)

		if use_estimatedθ
			this.thetas = CSV.read(joinpath(intables,"export_theta_pr.csv"), DataFrame)
		end



		# override parameters from dict par, if any
        if length(par) > 0
            for (k,v) in par
				if hasfield(Param,k)
					if (k == :θut)
						if length(v) > 1
							setfield!(this,k,v)
						else
							# this.θut = [v ; Float64[growθ(v, [this.θu_g for i in 2:it]) for it in 2:T]]
						end
					elseif (k == :θrt)
						if length(v) > 1
							setfield!(this,k,v)
						else
							# this.θrt = [v ; Float64[growθ(v, [this.θr_g for i in 2:it]) for it in 2:T]]
						end
					# elseif (k == :τ0t)
					# 	if length(v) > 1
					# 		setfield!(this,k,v)
					# 	else
					# 		this.τ0t = [v ; Float64[growθ(v, [this.τ0_g for i in 2:it]) for it in 2:T]]
					# 	end
					else
                		setfield!(this,k,v)
					end
				end
            end
			# if (:τ0t ∉ collect(keys(par)))
			# 	this.τ0t = [this.τ0t[1] ; Float64[growθ(this.τ0t[1], [this.τ0_g for i in 2:it]) for it in 2:T]]
			# end
        else
			# this.τ0t = [this.τ0t[1] ; Float64[growθ(this.τ0t[1], [this.τ0_g for i in 2:it]) for it in 2:T]]
		end


		# # pick out correct years
		tt = this.thetas[ this.thetas.year .∈ Ref(this.T),  : ]
		this.θut = tt.thetau
		this.θrt = tt.thetar
		this.moments = moments[ moments.year .∈ Ref(this.T), : ]


		Lt = CSV.read(joinpath(intables,"population.csv"), DataFrame)
		Lt = Lt[ Lt.year .∈ Ref(this.T), : ]
		this.Lt = !this.Lflat ? Lt.population ./ Lt.population[1] : ones(length(this.T))
		# this.Lt = exp.(collect(range(log(1.0),log(2.42),length = T)))
		this.L = this.Lt[1]

		this.moments = innerjoin(this.moments, Lt, on = :year)


		this.nspeeds = length(this.speed_thresholds) + 1

		this.θu = this.θut[1]
		this.θr = this.θrt[1]

		if this.ηm < 0 error("ηm > 0 violated") end
		if this.ζ > 1.0 || this.ζ < 0.0 error("ζ ∈ (0,1) violated") end
		if this.ξl > 1.0 || this.ξl < 0.0 @warn("ξl ∈ (0,1) violated") end
		if this.ξw > 1.0 || this.ξw < 0.0 @warn("ξw ∈ (0,1) violated") end

		if !issorted(this.speed_thresholds) error("speed thresholds must be increasing") end

		# derived parameters
		
		this.ηm = 1.0 # by default as normalization
		# this.cτ = (0.5 * this.a)^2 / (2 * this.ζ)
		this.ηw = 2 * this.ξw - 1
		this.ηl = 2 * this.ξl - 1

		this.inodes, this.iweights = gausslegendre(this.int_nodes)

		if (isnan(this.ϕ1x) || (this.ϕ1x <= 0)) error("invalid value for ϕ1x: $(this.ϕ1x)") end

		# this.taul = 0.571
		# println("taum = $(this.taum)")
		# println("taul = $(this.taul)")
		# this.ew = (-1)/(1+this.ηm)
		# this.el = (this.ηm + this.ηl)/(1+this.ηm)
		# as ηm goes to infinity the transport cost goes to 2 ζ w l

		this.Chain = BSON.load(joinpath(@__DIR__,"..","in","tables","mymodel.bson"))[:model]

		# data size classifications
		this.citylist = popdata(readdisk = true,do_smooth = this.psmooth, fname = this.data_input) 
		sort!(this.citylist, [:year, :rank])
		# this.citygroups = CSV.read(joinpath(intables, "relpop-means.csv"), DataFrame)


    	return this
	end
end
function getgrowth(p::Param,s::Symbol,g::Float64)
	x = getfield(p,s)
	[x[1] ; Float64[growθ(x[1], [g for i in 2:it]) for it in 2:length(p.T)]]
end


function constant_growth_θs!(p::Param,gr,gu)
	p.θut = [1.0 ; Float64[growθ(1.0, [gu for i in 2:it]) for it in 2:length(p.T)]]
	p.θrt = [1.0 ; Float64[growθ(1.0, [gr for i in 2:it]) for it in 2:length(p.T)]]
end


function backoutη(p::Param)
	function eq!(F,x,p::Param)
		#x = [ηm, ηl]
		F[1] = p.taul - ((x[1] + x[2])/(1 + x[1]))
		F[2] = p.taum - ((x[1])/(1 + x[1]))
	end
	r = nlsolve((F,x) -> eq!(F,x,p),ones(2))
	if converged(r)
		return(Dict(:ηm => r.zero[1], :ηl => r.zero[2]))
	else
		error("not converged")
	end
end

getline(x,digits) = [latexstring(x["symbol"]), x["description"], round(x["value"],digits = digits)]
getline(x,y,digits) = [latexstring(x["symbol"]), x["description"], round(x["value"],digits = digits),round(y["value"],digits = digits)]

"print params for both default and altnerative to latex table"
function latex_param_alt()
	# get baseline param
	j = open(joinpath(dirname(@__FILE__),"params.json")) do f
		JSON.parse(f)
	end
	# get sample2 estimates
	p2 = x2dict(parssample2())
	for (k,v) in p2
		j[string(k)]["value"] = v
	end
	# return j

	latex_param(par = j,digits = 3, fname = "sample2")
end

# prefixers
pf_base_agg(s) = joinpath(dbplots(),"revision$(revision())","baseline","aggregation",s)
pf_base(s) = joinpath(dbplots(),"revision$(revision())","baseline",s)
pf_sens_agg(s,p) = joinpath(dbplots(),"revision$(revision())",string("sensitivity-",s),"aggregation",p)
pf_sens(s,p) = joinpath(dbplots(),"revision$(revision())",string("sensitivity-",s),p)
pf_app(s,p) = joinpath(dbplots(),"revision$(revision())",string("appendix-",s),p)

function map_pkg_names_to_paper()

	# this should only ever be run when not in dev mode
	haskey(ENV,"LANDUSE_DEV") && error("you cannot run this function while in dev mode. Unset LANDUSE_DEV")

	mkpath(dbtables())

	cp(joinpath(dbtables(),"params-2-digits-baseline.tex"), joinpath(dbtables(),"table1.tex"),force = true)
	cp(joinpath(dbtables(),"sample-baseline.tex"), joinpath(dbtables(),"tableB1.tex"),force = true)
	cp(joinpath(dbtables(),"sample-alternative.tex"), joinpath(dbtables(),"tableB2.tex"),force = true)
	cp(joinpath(dbtables(),"params-3-digits-sample2.tex"), joinpath(dbtables(),"tableB3.tex"),force = true)
	cp(joinpath(dbtables(),"moments.tex"), joinpath(dbtables(),"tableB4.tex"),force = true)
	cp(joinpath(dbtables(),"moments-nontarget.tex"), joinpath(dbtables(),"tableB5.tex"),force = true)

	mkpath(nsplots())

	# main text
	register = 
	[
		pf_base_agg("mod_data_Lrshare.pdf") => joinpath(dbplots(),"figure8a.pdf"),
		pf_base_agg("mod_data_pr.pdf") => joinpath(dbplots(),"figure8b.pdf"),
		pf_base_agg("model_spending.pdf") => joinpath(dbplots(),"figure8c.pdf"),

		pf_base_agg("mod_data_urban_pop_area.pdf") => joinpath(dbplots(),"figure9a.pdf"),
		pf_base_agg("mod_data_avgdensity.pdf") => joinpath(dbplots(),"figure9b.pdf"),

		pf_base_agg("model_density_0_r.pdf") => joinpath(dbplots(),"figure10a.pdf"),
		pf_base_agg("mod_data_expdecay.pdf") => joinpath(dbplots(),"figure10b.pdf"),

		pf_base_agg("mod_data_commute_speed.pdf") => joinpath(dbplots(),"figure11a.pdf"),
		pf_base_agg("mod_APG_agg.pdf") => joinpath(dbplots(),"figure11b.pdf"),

		pf_base_agg("mod_data_piketty.pdf") => joinpath(dbplots(),"figure12a.pdf"),
		pf_base_agg("model_HPI.pdf") => joinpath(dbplots(),"figure12b.pdf"),

		pf_base("mod_data_pop_allyears_square_45deg.pdf") => joinpath(dbplots(),"figure13a.pdf"),
		pf_base("mod_data_area_allyears_square_color_45deg.pdf") => joinpath(dbplots(),"figure13b.pdf"),
		pf_base("mod_data_density_allyears_color_square_45deg.pdf") => joinpath(dbplots(),"figure13c.pdf"),

		pf_sens_agg("R3-fixedtheta","mod_data_urban_pop_area.pdf") => joinpath(dbplots(),"figure14a.pdf"),
		pf_sens_agg("R3-fixedtheta","mod_data_avgdensity.pdf") => joinpath(dbplots(),"figure14b.pdf"),

		pf_sens("R3-fixedtheta","mod_data_pop_allyears_square_45deg.pdf") => joinpath(dbplots(),"figure15a.pdf"),
		pf_sens("R3-fixedtheta","mod_data_area_allyears_square_color_45deg.pdf") => joinpath(dbplots(),"figure15b.pdf"),
		pf_sens("R3-fixedtheta","mod_data_density_allyears_color_square_45deg.pdf") => joinpath(dbplots(),"figure15c.pdf"),

		pf_sens("thetar","citydensity_1840.pdf") => joinpath(dbplots(),"figure16a.pdf"),
		pf_sens("thetar","dr_1840.pdf") => joinpath(dbplots(),"figure16b.pdf"),
		pf_sens("thetar","ρr_1840.pdf") => joinpath(dbplots(),"figure16c.pdf"),

		pf_sens("commutecost","commuting_speed_1840.pdf") => joinpath(dbplots(),"figure17a.pdf"),
		pf_sens("commutecost","citydensity_1840.pdf") => joinpath(dbplots(),"figure17b.pdf"),
		pf_sens("commutecost","HPI_1840.pdf") => joinpath(dbplots(),"figure17c.pdf"),

		# appendix B

		pf_app("numillust","Lr.pdf") => joinpath(dbplots(),"figureB1a.pdf"),
		pf_app("numillust","spending.pdf") => joinpath(dbplots(),"figureB1b.pdf"),
		pf_app("numillust","pr.pdf") => joinpath(dbplots(),"figureB1c.pdf"),
		pf_app("numillust","LuArea.pdf") => joinpath(dbplots(),"figureB1d.pdf"),
		pf_app("numillust","AggDensities.pdf") => joinpath(dbplots(),"figureB1e.pdf"),
		pf_app("numillust","landrents.pdf") => joinpath(dbplots(),"figureB1f.pdf"),

		pf_app("numillust","Lu_spread.pdf") => joinpath(dbplots(),"figureB2a.pdf"),
		pf_app("numillust","density_spread_log.pdf") => joinpath(dbplots(),"figureB2b.pdf"),
		pf_app("numillust","Lr_spread.pdf") => joinpath(dbplots(),"figureB2c.pdf"),
		pf_app("numillust","ρr_spread.pdf") => joinpath(dbplots(),"figureB2d.pdf"),

		pf_app("numillust-sbar","spending.pdf") => joinpath(dbplots(),"figureB3a.pdf"),
		pf_app("numillust-sbar","aggDensities.pdf") => joinpath(dbplots(),"figureB3b.pdf"),
		pf_app("numillust-sbar","LuArea.pdf") => joinpath(dbplots(),"figureB3c.pdf"),

		pf_base("smoothed-thetas.pdf") => joinpath(dbplots(),"figureB4.pdf"),
		pf_sens("nopopgrowth","population.pdf") => joinpath(dbplots(),"figureB5.pdf"),
		pf_base("input-pop-prices.pdf") => joinpath(dbplots(),"figureB6.pdf"),

		pf_sens("sigma","Lrshare_agg.pdf") => joinpath(dbplots(),"figureB7a.pdf"),
		pf_sens("sigma","pr.pdf") => joinpath(dbplots(),"figureB7b.pdf"),
		pf_sens("sigma","citydensity_1840.pdf") => joinpath(dbplots(),"figureB7c.pdf"),

		pf_sens("omega","citydensity_1840.pdf") =>      joinpath(dbplots(),"figureB8a.pdf"),
		pf_sens("omega","dr_1840.pdf") =>               joinpath(dbplots(),"figureB8b.pdf"),
		pf_sens("omega","ρr_1840.pdf") => joinpath(dbplots(),"figureB8c.pdf"),

		pf_sens("epsilonr","citydensity_1840.pdf") => joinpath(dbplots(),"figureB9a.pdf"),
		pf_sens("epsilonr","area_pop.pdf") =>          joinpath(dbplots(),"figureB9b.pdf"),
		pf_sens("epsilonr","dens_decay.pdf") =>          joinpath(dbplots(),"figureB9c.pdf"),

		pf_sens("agglomeration","congest-cityarea_agg_1840.pdf") => joinpath(dbplots(),"figureB10a.pdf"),
		pf_sens("agglomeration","congest-citydensity_1840.pdf")  => joinpath(dbplots(),"figureB10b.pdf"),
		pf_sens("agglomeration","congest-d0_1840.pdf")           => joinpath(dbplots(),"figureB10c.pdf"),
		pf_sens("agglomeration","congest-pr_1840.pdf")           => joinpath(dbplots(),"figureB10d.pdf"),
		pf_sens("agglomeration","congest-ρr_1840.pdf")           => joinpath(dbplots(),"figureB10e.pdf"),
		pf_sens("agglomeration","congest-HPI_1840.pdf")          => joinpath(dbplots(),"figureB10f.pdf"),

		pf_sens("agglomeration","cityarea_agg_1840.pdf") => joinpath(dbplots(),"figureB11a.pdf"),
		pf_sens("agglomeration","citydensity_1840.pdf") =>  joinpath(dbplots(),"figureB11b.pdf"),
		pf_sens("agglomeration","d0_1840.pdf") =>           joinpath(dbplots(),"figureB11c.pdf"),
		pf_sens("agglomeration","pr_1840.pdf") =>           joinpath(dbplots(),"figureB11d.pdf"),
		pf_sens("agglomeration","ρr_1840.pdf") =>           joinpath(dbplots(),"figureB11e.pdf"),
		pf_sens("agglomeration","HPI_1840.pdf") =>          joinpath(dbplots(),"figureB11f.pdf"),

		pf_sens("d1d2","cityarea_agg_1840.pdf") => joinpath(dbplots(),"figureB12a.pdf"),
		pf_sens("d1d2","citydensity_1840.pdf") =>  joinpath(dbplots(),"figureB12b.pdf"),
		pf_sens("d1d2","d0_1840.pdf") =>           joinpath(dbplots(),"figureB12c.pdf"),
		pf_sens("d1d2","dr_1840.pdf") =>           joinpath(dbplots(),"figureB12d.pdf"),
		pf_sens("d1d2","dens_decile.pdf") =>       joinpath(dbplots(),"figureB12e.pdf"),
		pf_sens("d1d2","ρr_1840.pdf") =>           joinpath(dbplots(),"figureB12f.pdf"),

		pf_sens("d1d2","Lu_allyears_d1d2.pdf") =>   joinpath(dbplots(),"figureB13a.pdf"),
		pf_sens("d1d2","area_allyears_d1d2.pdf") => joinpath(dbplots(),"figureB13b.pdf"),
		pf_sens("d1d2","dens_allyears_d1d2.pdf") => joinpath(dbplots(),"figureB13c.pdf"),

		pf_sens("d1d2","Lu_allyears.pdf") =>   joinpath(dbplots(),"figureB14a.pdf"),
		pf_sens("d1d2","area_allyears.pdf") => joinpath(dbplots(),"figureB14b.pdf"),
		pf_sens("d1d2","dens_allyears.pdf") => joinpath(dbplots(),"figureB14c.pdf")
	]
	
	for r in register
		cp(r.first, r.second, force = true)
	end

	if make_appendix_C   # defined in top level of package
		cp(pf_base("mod_data_density_xsect_time_median.pdf"),joinpath(nsplots(), "mod_data_density_xsect_time_median.pdf"), force = true)
	end

	rm(joinpath(dbplots(),"revision$(revision())"), force = true, recursive = true)



end

"print default param to latex table"
function latex_param(;par = Dict(),digits = 2, fname = "baseline")
	f = open(joinpath(dirname(@__FILE__),"params.json"))
	j = JSON.parse(f)
	close(f)

	docompare = length(par) > 0
	if docompare
		j2 = deepcopy(j)
		for (k,v) in par
			j2[string(k)]["value"] = v["value"]
		end
	end
	# return j2

	if docompare
		

		latex_tabular(joinpath(dbtables(),"params-$digits-digits-$fname.tex"), Tabular("l l D{.}{.}{1.$(digits)}@{} D{.}{.}{1.$(digits)}@{}"), [
		Rule(:top),
		["Parameter", "Description", MultiColumn(1,:c,"Baseline"), MultiColumn(1,:c,"Alternative")],
		Rule(:mid),
		getline(j["S"],j2["S"],1),
		[latexstring("L_0"), j["L"]["description"], round(j["L"]["value"],digits = 2), round(j2["L"]["value"],digits = 2)],
		[latexstring("\\theta_0"), "Initial Productivity in 1840", 1.0, 1.0],
		getline(j["α"],j2["α"],digits),
		getline(j["σ"],j2["σ"],digits),
		getline(j["σc"],j2["σc"],digits),
		getline(j["ν"],j2["ν"],digits),
		getline(j["γ"],j2["γ"],3),
		getline(j["cbar"],j2["cbar"],digits),
		getline(j["sbar"],j2["sbar"],digits),
		getline(j["β"],j2["β"],digits),
		getline(j["ξl"],j2["ξl"],digits),
		getline(j["ξw"],j2["ξw"],digits),
		getline(j["a"],j2["a"],digits),
		getline(j["ϵr"],j2["ϵr"],digits),
		getline(j["ϵs"],j2["ϵs"],digits),
		Rule(:bottom)
		]
	)

	else
		
		@info "writing to $(joinpath(dbtables(),"params-$digits-digits-$fname.tex"))"
		latex_tabular(joinpath(dbtables(),"params-$digits-digits-$fname.tex"), Tabular("l l D{.}{.}{1.$(digits)}@{}"), [
		Rule(:top),
		["Parameter", "Description", MultiColumn(1,:c,"Value")],
		Rule(:mid),
		getline(j["S"],1),
		[latexstring("L_{1840}"), j["L"]["description"], round(j["L"]["value"],digits = 2)],
		[latexstring("\\theta_{s,1840}"), "Initial Agg. Productivity in sector \$s\$", 1.0],
		getline(j["α"],digits),
		getline(j["σ"],digits),
		getline(j["σc"],digits),
		getline(j["ν"],3),
		getline(j["γ"],3),
		getline(j["cbar"],digits),
		getline(j["sbar"],digits),
		getline(j["β"],digits),
		getline(j["ϵr"],digits),
		getline(j["ϵs"],digits),
		getline(j["ξl"],digits),
		getline(j["ξw"],digits),
		getline(j["a"],digits),

		Rule(:bottom)
		]
		)
	end

end


"copy dropbox data to in/ folder"
function copydb()
	cp(dbpath())
end


function show(io::IO, ::MIME"text/plain", p::Param)
    print(io,"LandUse Param:\n")
	print(io,"      γ       : $(p.γ   )\n")
	print(io,"      ϵr      : $(p.ϵr  )\n")
	print(io,"      ϵs      : $(p.ϵs  )\n")
	print(io,"      ν       : $(p.ν   )\n")
	print(io,"      cbar    : $(p.cbar)\n")
	print(io,"      sbar    : $(p.sbar)\n")
	print(io,"      θr      : $(p.θr  )\n")
	print(io,"      θu      : $(p.θu  )\n")
	print(io,"      α       : $(p.α   )\n")
	print(io,"      useless       : $(p.useless   )\n")
	print(io,"      a       : $(p.a   )\n")
	print(io,"      L       : $(p.L   )\n")
	print(io,"      S       : $(p.S   )\n")
	print(io,"      T       : $(p.T   )\n")
	print(io,"      t       : $(p.t   )\n")
	print(io,"      σ       : $(p.σ   )\n")
	print(io,"      K       : $(p.K  )\n")
end


@doc raw"""
	compute_ξw()

Computes input parameter ξw directly from data. Notice that ``1 - \xi_w`` is the 
elasticity of speed to wage income. We measure from individual commuting data in ENL (Enquete National de Logement)
an increase in commuting speed of about 11% from 1984 to 2013. This function returns 

``\xi_w = 1 - \frac{11}{\%\Delta \theta_u}``

where ``\%\Delta \theta_u`` stands for the percentage increase in urban wage from 1984 to 2013.
"""
function compute_ξw(;  ENL = 0.109)
	d = DataFrame(CSV.File(joinpath(dbpath(),"data","nico-output","FRA_model.csv")))
	y = subset(select(d, :year, :theta_urban), :year => x -> x .∈ Ref([1984, 2013]))
	sort!(y, :year)
	dθ = (y.theta_urban[2] - y.theta_urban[1]) / y.theta_urban[1]
	1 - ENL / dθ
end


function smooth_p_check(periods)
	d = DataFrame(CSV.File(joinpath(dbpath(),"data","nico-output","FRA_model.csv")))
	d = d[.!ismissing.(d.P_rural),:]
	return d
	z = transform(d[!,[:year,:P_rural]], :P_rural => (y -> smooth(collect(skipmissing(y)),periods)))
	@df z plot(:year, [:P_rural , :P_rural_function], seriestype = [:path],lw = 2,
			   title = "smoothing over $periods periods",
			   labels = ["" "Smoothed"])
end

@doc raw"""
	prepare_data(p::Param; digits = 9)

Takes raw csv data and prepares to be used in model.
writes to csv input files.

## Smoothing of Productivity Series

We take the following steps to obtain a smoothed series for ``\theta_r`` and ``\theta_u``:

1. We obtain the estimated series at annual frequency.
1. We subset both series to start in 1840 and end in 2015 (rural productivity ends in that year)
1. We linearly interpolate the missing interwar years.
1. Smoothing is done via the [`smooth`] function from the QuantEcon package: we use the default [Hann window](https://en.wikipedia.org/wiki/Hann_function) and a 15-year window size. We experimented with the window size until high-frequency oscillations disappear.
1. Our rural productivity series gets very volatile from 2000 onwards, and in fact one would find a decreasing rural productivity series if we applied our smoother to post 2000 data. Therefore from the year 2000 onwards, we grow the smoothed series forward with 1% annual growth. Given that 2000 is very close to the final new steady state of the model, the actual choice of growth rate has only a small impact on our results.


"""
function prepare_data(digits = 9; writedisk = false)

	p = Param(par = Dict(:T => 1840:10:2350))   # until end of time.

	# plotting path
	pth = joinpath(dbplots(),"revision$(revision())","baseline")
	mkpath(pth)

	# population data for each year (needs interpolation)
	pop = DataFrame(CSV.File(joinpath(dbpath(),"data","France-population.csv")))
	append!(pop, DataFrame(year = 2050, population = 74.0))  # https://www.insee.fr/fr/statistiques/2859843
	itp = interpolate((pop.year,), pop.population, Gridded(Linear()))
	popd = DataFrame(year = 1815:2050, population = itp(1815:2050))
	# extrapolate with p.popg until year p.sT
	append!(popd, DataFrame(year = collect(2051:p.sT), population = [popd[end,:population] * (1 + p.popg) ^ i for i in 1:(p.sT - 2051 + 1)] ))
	if writedisk
		CSV.write(joinpath(dbtables(),"population.csv"),popd)
		CSV.write(joinpath(intables,"population.csv"),popd)
	end


	# productivity and employment moments

	dt = p.T
	ma = p.ma
	rgrowth = 1 + p.magr
	ugrowth = 1 + p.magu

	d = DataFrame(CSV.File(rawdata_loc()))
	# d = DataFrame(CSV.File(joinpath(dbpath(),"data","nico-output","FRA_model.csv")))
	# @df d plot(:year, :P_rural)

	# fix commuting speed for paris in year 1840
	v = @view d[d.year .== 1840,:]
	replace!(v.Comm_speed_Paris, missing => 1)

	# plot(d.year, d.theta_rural)
	# plot(rand(10))
	x = @chain d begin
		subset(:year => ByRow(<=(2015)), :year => ByRow(>=(dt.start)))   # rural data stops in 2015
	end

	x.P_rural = (x.P_rural_Sauvy .^ 0.5) .* (x.P_rural_Toutain .^ 0.5)
	



	# normalize year 1 to 1.0
	# x = transform(x, :theta_rural => (x -> x ./ x[1]) => :theta_rural, :theta_urban => (x -> x ./ x[1]) => :theta_urban)

	#impute
	Impute.interp!(x)

	# moving average smoother
	transform!(x, :theta_rural => (y -> smooth(collect(skipmissing(y)),ma)) => :stheta_rural,
	              :theta_urban => (y -> smooth(collect(skipmissing(y)),ma)) => :stheta_urban,
	              :P_rural => (y -> smooth(collect(skipmissing(y)),61)) => :P_rural)

	if maximum(x.year) < dt.stop
		# append the future to end of data
		x1 = vcat([DataFrame(Dict(zip(names(x), [missing for i in 1:length(names(x))]))) for i in 1:length(2016:dt.stop)]...)
		x1.year = 2016:dt.stop
		allowmissing!(x)
		append!(x,x1)


		# interpolate forward
		Impute.locf!(x)
	end

	# from year 2000 onwards, replace rural with `growth` percent growth
	x[:, :stheta_rural] .= ifelse.(x.year .> 2000,vcat(zeros(sum(x.year .<= 2000)),[x[x.year .== 2000,:stheta_rural] * rgrowth^i for i in 1:sum(x.year .> 2000)]...),x.stheta_rural)
    x[:, :stheta_urban] .= ifelse.(x.year .> 2000,vcat(zeros(sum(x.year .<= 2000)),[x[x.year .== 2000,:stheta_urban] * ugrowth^i for i in 1:sum(x.year .> 2000)]...),
   							   x.stheta_urban)

    # round to 9 digits
	x[:, [:stheta_rural, :stheta_urban]] .= mapcols(col -> round.(collect(skipmissing(col)),digits = digits),x[:, [:stheta_rural, :stheta_urban]])

	x = transform(x, :theta_rural => (x -> x ./ x[1]) => :theta_rural, :theta_urban => (x -> x ./ x[1]) => :theta_urban)
	x = transform(x, :stheta_rural => (x -> x ./ x[1]) => :stheta_rural, :stheta_urban => (x -> x ./ x[1]) => :stheta_urban)

	s0 = ": Rural"
	s1 = ": Urban"
	s3 = "Smoothed"
	x2020 = @chain x begin
		subset(:year => leq(2020))
	end
    p1 = @df x2020 plot(:year, [:theta_rural :stheta_rural],leg=:topleft, lw = 3, 
	yformatter = x -> string.(Int(round(x,digits = 0))),
	                 label = ["Data"  "Smoothed"], 
					 title = L"\theta_r\textbf{%$s0}",
					 yscale = :log10,
					 yticks = [1,2,6,15],
					 ylabel = L"\log \theta")
	
	savefig(p1, joinpath(pth,"smooth-theta-rural.pdf"))

	p2 = @df x2020 plot(:year, [:theta_urban :stheta_urban],leg=false, lw = 3, label = ["Data"  "Smoothed"], title = L"\theta_u\textbf{%$s1}",
		yscale = :log10,
		yticks = [1,2,6,15],
		yformatter = x -> string.(Int(round(x,digits = 0))))
	savefig(p2, joinpath(pth,"smooth-theta-urban.pdf"))

	p3 = @df x2020 plot(:year, [:stheta_rural :stheta_urban],leg=:left, lw = 2, label = [L"\theta_r" L"\theta_u"], title = L"\textbf{%$s3}",
	yscale = :log10)


	p11 = plot(p1,p2,layout = (1,2), size = (900,450),
			  link = :y ,
			  left_margin = 5Plots.mm,
			  xtickfontsize = 8)
	# p11 = plot(p1,p2,p3, layout = (1,3), size = (900,300),yformatter = x -> round(x,digits = 1) )
	savefig(p11, joinpath(pth,"smoothed-thetas.pdf"))


	# p3 = @df x plot(:year, [:stheta_rural :stheta_urban],leg=:left, lw = 2)
	savefig(p3, joinpath(pth,"smooth-thetas-model.pdf"))

	p4 = @df x2020 scatter(:year, :theta_rural, title = "Rural", label = "data",leg=:left)
	plot!(p4, x.year, x.stheta_rural, lw = 2, label = "model")

	p5 = @df x2020 scatter(:year, :theta_urban, title = "Urban", label = "data",leg=:left)
	plot!(p5, x.year, x.stheta_urban, lw = 2, label = "model")

	p6 = plot(p4,p5,layout = (2,1),size = (800,500))
	savefig(p6, joinpath(pth,"smooth-thetas-data-model.pdf"))

	p7 = @df popd plot(:year,:population)

	if writedisk 
		CSV.write(joinpath(intables,"data-moments.csv"),x)
	end
	#
	#
	# (ret, pl)
	# x
	(p1,p2,p3,p6,p11,p7,x)
end



function plot_shares()
	d = DataFrame(CSV.File(rawdata_loc()))
	x = @chain d begin
		where((:year .< 2018) .& (:year .> 1895))
	end

	x = select!(x,:year,:SpendingShare_Rural => :Rural, :SpendingShare_Urban => :Urban, :SpendingShare_Housing => :Housing)
	x = stack(x, Not(:year))

	pl = @df x plot(:year, :value, group = :variable,
	           linewidth = 2,ylab = "Spending Share", leg = :bottomright)

   savefig(pl, joinpath(dbdataplots(),"spending-shares.pdf"))



end

# set period specific values
function setperiod!(p::Param,i::Int)
	setfield!(p, :θr, p.θrt[i])   # this will be constant across region.
	setfield!(p, :θu, p.θut[i])   # in a country setting, we construct the growth rate differently for each region.
	setfield!(p, :L, p.Lt[i])

	# setfield!(p, :θr, i == 1 ? p.θr0 : growθ(p.θr0,p.θrg[1:(i-1)]))   # this will be constant across region.
	# setfield!(p, :θu, i == 1 ? p.θu0 : growθ(p.θu0,p.θug[1:(i-1)]))   # in a country setting, we construct the growth rate differently for each region.
	setfield!(p,  :t , p.T[i] )
	setfield!(p,  :it , i )
end

growθ(θ0,g::Vector{Float64}) = θ0 * prod(g)






function setperiod!(p::Vector{Param},i::Int)
	for ip in eachindex(p)
		setperiod!(p[ip],i)
	end
end


function setfields!(p::Vector{Param},name::Symbol,x)
	for ip in eachindex(p)
		setfield!(p[ip],name,x)
	end
end

# ϵfun(d,s,ϕ,ϵtarget) = ϵtarget * exp(-s * max(ϕ-d,0.0))
function ϵfun_tmp(d,s,ϕ,p::Param)
	setfield!(p, :ϵs, s)
	ϵ(d,ϕ,p)
end





"""
A Country-wide Parameter struct
"""
mutable struct CParam
	L     :: Float64 # total population
	Lt     :: Vector{Float64} # total population
	S     :: Float64 # total space
	K     :: Int  # number of Regions
	kshare    :: Vector{Float64}  # region k's share of total space


	function CParam(;par=Dict())
        f = open(joinpath(dirname(@__FILE__),"params.json"))
        j = JSON.parse(f)
        close(f)
        this = new()

        for (k,v) in j
			if v["type"] == "country"
	            if v["value"] isa Vector{Any}
	                setfield!(this,Symbol(k),convert(Vector{Float64},v["value"]))
	            else
	                setfield!(this,Symbol(k),v["value"])
	            end
			end
        end

        if length(par) > 0
            # override parameters from dict par
            for (k,v) in par
				if hasfield(CParam, k)
                	setfield!(this,k,v)
				end
            end
        end
        if length(this.kshare) != this.K
        	throw(ArgumentError("your settings for number of regions are inconsistent"))
        end
        if (this.K > 1) & !(sum(this.kshare) ≈ 1.0)
        	throw(ArgumentError("Shares of regions space must sum to 1.0"))
        end
    	return this
	end
end

function show(io::IO, ::MIME"text/plain", p::CParam)
    print(io,"LandUse Country Param:\n")
	print(io,"      L       : $(p.L   )\n")
	print(io,"      S       : $(p.S   )\n")
	print(io,"      K       : $(p.K   )\n")
	print(io,"      kshare  : $(p.kshare   )\n")
end

"convert a country param to one param for each region"
function convert(c::CParam; par = Dict())
	if length(par) > 0
		p = [Param(par = par[ik]) for ik in 1:c.K]
	else
		p = [Param(par = par) for ik in 1:c.K]
	end
	# do adjustment of parameters one by one
	for ik in 1:c.K
		# p[ik].L = c.kshare[ik] * c.L  # by default allocate this share of people to k
		# p[ik].S = c.kshare[ik] * c.S  # true by definition
		# these entries are only used to compute a one-region country (i.e. first step to get starting values)
		# we allocated total space and population to each country.
		p[ik].L = c.L  # by default allocate this share of people to k
		p[ik].S =  c.S  # true by definition

		# transform aggregate productivity series into region equivalentes

	end
	return p
end
