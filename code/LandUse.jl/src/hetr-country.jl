

"""
	Heterogenous-θr Country

Like [`Country`](@ref) but with heterogeneous rural productivity. This implies that
there is a different value of rural land, house price, housing consumption etc in each region.

## Different Characteristics for each Region

In each region, given different θr, there will be different

* rural land value
* rural house price 
* rural wage

## Shared Characteristics

In all regions, the following characteristics are shared:

* total population
* rent and food price
"""
mutable struct HetCountry <: COUNTRY
	R  :: Vector{Region}   # a set of regions
	pp  :: Vector{Param}   # a set of Params
	p0  :: Param   # one macro parameter to be kept constant throughout all experiments.

	K  :: Int              # number of regions
	r  :: Float64          # country-wide per capita rental income
	pr  :: Float64          # country-wide per capita rental income
	Sk :: Vector{Float64}  # total space for each region
	L  :: Float64  # total population
	S  :: Float64 # total space
	T  :: StepRange{Int,Int}

	"""
		Het θr Country constructor

	want to give single p and get back
	differnet theta series for each country
	"""
	function HetCountry(p::Param;istest = false)
		this = new()
		@assert p.K == length(p.kshare)
		this.K = p.K

		setthetas = all(p.gsr .!= zeros(p.K))  # if you set growth rates, we will set the thetar's, starting from 1.0
		if setthetas
			thetarmat = [[1.0 ; Float64[growθ(1.0, [p.gsr[ik] for i in 2:it]) for it in 2:length(p.T)]] for ik in 1:p.K]
			thetaumat = [[1.0 ; Float64[growθ(1.0, [p.gs[ik] for i in 2:it]) for it in 2:length(p.T)]] for ik in 1:p.K]
		end

		# create K copies of parameter
		this.pp = Param[deepcopy(p) for _ in 1:p.K]


		# create the macro parameter
		this.p0 = deepcopy(p)



		# modify θus for each

		for ik in 1:p.K
			if setthetas
				if p.it == 1
					this.pp[ik].θr = 1.0
					this.pp[ik].θu = 1.0
				else
					this.pp[ik].θr = thetarmat[ik][p.it]
					this.pp[ik].θu = thetaumat[ik][p.it]
				end
			else
				this.pp[ik].θr =  p.θr * p.factors[ik] * exp(p.gs[ik] * (p.it-1))
			end
		end

		this.R = [Region(this.pp[ik]) for ik in 1:p.K]


		this.pr = NaN
		this.r  = NaN
		this.L  = p.Lt[p.it] * p.K  # pop = 20
		this.S  = p.S * p.K # total space = 20
		this.Sk  = p.kshare .* this.S
		this.T = p.T
		return this
	end
end

hetcountry(; par = Dict(:K => 2, :kshare => [0.5,0.5], :factors => [1.0,1.05], :gs => zeros(2), :gsr => zeros(2))) = HetCountry(Param(par = par))
# hetcountry(; par = Dict(:K => 2, :kshare => [0.5,0.5], :factors => [1.0,1.05], :gs => zeros(2), :gsr => [1.15,1.12])) = HetCountry(Param(par = par))


"""
	update!(c::HetCountry,p::Vector{Param},x::Vector{Float64})

Update a `Country` object with a current vector of choice variables `x` supplied by the solver.
The ordering in `x` is:

1. r: land rent
2. pr: relative price rural good
3. 3 : (K+2), Sr: amount of land use in rural production (complement of Srh)
4. (K+3) : (2K+3), Lu: urban pop in each k
5. Lr: (2K+4) : (3K+4) rural labor allocations
6. (3K+5) : end, θu: urban prod in each k
"""
function update!(c::HetCountry,x::NamedTuple;estimateθ=false)
	K = c.K
	p = c.pp

	# update country wide components
	Lr   = x.Lr     # constant labor/land share in region 1. i called that b>0 in the doc.
	c.r  = x.r      # land rent
	c.pr = x.pr     # relative price rural good
	Srk  = x.Sr      # Sr for each region k
	Lu   = x.Lu       # Lu for each region k
	ϕs   = x.ϕ        # Lu for each region k
	if estimateθ
		θus    = x.θu  # θu for each region k
		θrs    = x.θr  # θu for each region k
	end
# @infiltrate
	@assert K == length(c.R)

	# update each region
	# 2. update other equations in each region
	for ik in 1:K
		cx = (ρr = x.ρr[ik], 
		      ϕ = ϕs[ik], 
		      ℙ = x.ℙ[ik], 
			  r = c.r, 
			  Lr = Lr[ik], 
			  pr = c.pr, 
			  Sr = Srk[ik],
			  θu = estimateθ ? θus[ik] : p[ik].θu,
			  θr = estimateθ ? θrs[ik] : p[ik].θr)
		update!(c.R[ik],p[ik],cx, Lu = Lu[ik])
		denserr = abs(c.R[ik].Lu - c.R[ik].iDensity)
		if denserr > 1e-5
			@warn "abs(Lu[$ik] - iDensity[$ik]) = $denserr"
		end
		# @assert abs(c.R[ik].Lu - c.R[ik].iDensity) < 1e-4 
	end
end


