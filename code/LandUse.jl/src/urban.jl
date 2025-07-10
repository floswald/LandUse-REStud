"""

# Urban Model

This model has a fixed price of land ρr in the rural sector.
Differently to our baseline model, where ρr is a GE object.
"""
mutable struct Urban <: Model
	ρr   :: Float64   # land price in rural sector
	qr   :: Float64     # housing price in rural sector
	ρ0   :: Float64     # land price in center of city
	q0   :: Float64     # housing price in center of city

	Lr   :: Float64   # employment in rural sector
	Lu   :: Float64   # employment in urban sector
	wu0  :: Float64   # wage in urban sector (at center)
	wr   :: Float64   # wage in rural sector
	Sr   :: Float64   # Amount of land used in rural production
	Srh  :: Float64   # Amount of land used for rural housing
	r    :: Float64   # per capita land rental income
	pr   :: Float64   # relative price of rural good
	ϕ    :: Float64   # size of the city
	xsr  :: Float64  # excess subsistence rural worker
	U    :: Float64  # common utility level
	θu   :: Float64  # current productivity
	θr   :: Float64  # current productivity
	cr01 :: Tuple{Float64,Float64}  # consumption at locations 0 and 1. temps to check.
	cu01 :: Tuple{Float64,Float64} # consumption at locations 0 and 1. temps to check.
	# integration setup
	inodes   :: Vector{Float64}  # theoretical integration nodes
	iweights :: Vector{Float64}  # int weights
	nodes    :: Vector{Float64}  # points where to evaluate integrand (inodes scaled into [0,ϕ])

	# resulting integrals from [0,ϕ]
	icu_input :: Float64   # ∫ cu_input(l) dl
	iDensity  :: Float64   # ∫ D(l) dl
	icu       :: Float64   # ∫ cu(l) D(l) dl
	icr       :: Float64   # ∫ cr(l) D(l) dl
	iτ        :: Float64   # ∫ τ(l) D(l) dl
	iq        :: Float64   # ∫ ρ(l) dl
	iy        :: Float64   # ∫ w(l) D(l) dl
	function Urban(p::Param)
		# creates a model fill with NaN
		m      = new()
		m.ρr   = p.ρrbar
		m.qr   = NaN
		m.ρ0   = NaN
		m.q0   = NaN
		m.Lr   = NaN
		m.Lu   = NaN
		m.wu0  = NaN
		m.wr   = NaN
		m.Sr   = NaN
		m.Srh  = NaN
		m.r    = NaN
		m.pr   = NaN
		m.ϕ    = NaN
		m.xsr  = NaN
		m.θu   = p.θu
		m.θr   = p.θr
		m.U    = NaN
		m.cr01 = (NaN,NaN)
		m.cu01 = (NaN,NaN)
		m.inodes, m.iweights = gausslegendre(p.int_nodes)
		m.nodes = zeros(p.int_nodes)
		icu_input = NaN
		iDensity  = NaN
		icu       = NaN
		iτ        = NaN
		iq        = NaN
		iy        = NaN
		return m
	end

end

"""
	update!(m::Urban,p::Param,x::Vector{Float64})

update a single region a parameter vector at choices `x`.
"""
function update!(m::Urban,p::Param,x::Vector{Float64})
	m.r    = x[1]   # land rent
	m.Lr   = x[2]   # employment in rural sector
	m.pr   = x[3]   # relative price rural good
	m.Sr   = x[4]   # amount of land used in rural production
	# this fourth choice is implied:
	# @assert p.ρrbar == m.ρr   # just to make sure
	#
	# 	# println("Lr = $(m.Lr)")
	#
	# function sfun(Sr)
	# 	if m.Lr / Sr < 0
	# 		println("giving PEN")
	# 		PEN
	# 	else
	# 		x = m.ρr - foc_Sr(m.Lr / Sr, m.pr, p)
	# 		# println("Sr = $Sr, val = $x, pr = $(m.pr)")
	# 		x
	# 	end
	# end
	# m.Sr = fzero(x -> sfun(x), 0.001)
	# m.Sr = fzero(x -> m.ρr - foc_Sr(m.Lr / x, m.pr, p), m.Lr)
	# m.Sr = find_zero(x -> sfun(x), (0.001,0.98*p.S) )
	# m.Sr = find_zero(x -> m.ρr - foc_Sr(m.Lr / x, m.pr, p), (0.001,0.98*p.S) )
	@debug "implied" Sr=m.Sr


	# update equations
	m.Lu   = p.L - m.Lr   # employment in urban sector
	m.wu0  = wu0(m.Lu,p)   # wage rate urban sector at city center (distance = 0)
	m.wr   = foc_Lr(m.Lr / m.Sr , m.pr, p)
	m.ρr   = foc_Sr(m.Lr / m.Sr , m.pr, p)
	m.ϕ    = getfringe(m.wr / p.θu,p)

	m.xsr  = xsr(p,m)
	m.Srh  = Srh(p,m)
	m.qr   = qr(p,m)
	# compute consumption at locations 0 and 1 to check both positive in both sectors.
	m.cr01 = (cr(0.0,p,m)-p.cbar, cr(1.0,p,m)-p.cbar)
	m.cu01 = (cu(0.0,p,m)       , cu(1.0,p,m)       )
	m.U    = all( (m.cr01 .>= 0.0) .* (m.cu01 .>= 0.0) ) ? utility(0.0,p,m) : NaN
	m.q0   = q(0.0,p,m)
	m.ρ0   = ρ(0.0,p,m)
	# if !all((m.cr01 .>= 0.0) .* (m.cu01 .>= 0.0) )
	# println("neg cons")
	# end
	# display(m)
	m.nodes[:] .= m.ϕ / 2 .+ (m.ϕ / 2) .* m.inodes   # maps [-1,1] into [0,ϕ]
	integrate!(m,p)
end


"""
	integrate!(m::Urban,p::Param)

could be made much faster by filling a matrix col-wise with integration nodes and
doing a matmul on it? have to allocate memory though.
"""
function integrate!(m::Urban,p::Param)

	m.icu_input = (m.ϕ/2) * sum(m.iweights[i] * cu_input(m.nodes[i],p,m) for i in 1:p.int_nodes)[1]
	m.iDensity  = (m.ϕ/2) * sum(m.iweights[i] * D(m.nodes[i],p,m) for i in 1:p.int_nodes)[1]
	m.icu       = (m.ϕ/2) * sum(m.iweights[i] * cu(m.nodes[i],p,m) * D(m.nodes[i],p,m) for i in 1:p.int_nodes)[1]
	m.icr       = (m.ϕ/2) * sum(m.iweights[i] * cr(m.nodes[i],p,m) * D(m.nodes[i],p,m) for i in 1:p.int_nodes)[1]
	m.iτ        = (m.ϕ/2) * sum(m.iweights[i] * τ(m.nodes[i],m.ϕ,p) * D(m.nodes[i],p,m) for i in 1:p.int_nodes)[1]
	m.iq        = (m.ϕ/2) * sum(m.iweights[i] * ρ(m.nodes[i],p,m) for i in 1:p.int_nodes)[1]
	m.iy        = (m.ϕ/2) * sum(m.iweights[i] * w(m.Lu,m.nodes[i],m.ϕ,p) * D(m.nodes[i],p,m) for i in 1:p.int_nodes)[1]
	# @debug "integrate!" icu_input=m.icu_input iDensity=m.iDensity icu=m.icu iτ=m.iτ iq=m.iq phi=m.ϕ
	# @assert m.icu_input > 0
	# @assert m.iDensity > 0
	# @assert m.icu      > 0
	# @assert m.icr      > 0
	# @assert m.iτ       > 0
	# @assert m.iq       > 0
	# @assert m.iy       > 0
end

"""
	Eqsys!(F::Vector{Float64},m::Urban,p::Param)

compute system of equations for the general (with flexible ϵ).
"""
function Eqsys!(F::Vector{Float64},m::Urban,p::Param)
	# land market clearing: after equation (20)
	# F[1] = p.S - p.useless - m.ϕ - m.Sr - m.Srh
	F[1] = m.ρr - p.ρrbar

	# city size - Urban population relationship: equation (19)
	F[2] = m.Lu - m.iDensity

	#  total land rent per capita: equation (23)
	F[3] = m.r * p.L - m.iq - m.ρr * (m.Sr + m.Srh)

	# urban goods market clearing. equation before (25) but not in per capita terms
	#      rural cu cons + urban cu cons + rural constr input + urban constr input + commuting - total urban production
	F[4] = m.Lr * cur(p,m) + m.icu + m.Srh * cu_input(m.ϕ,p,m) + m.icu_input + m.wu0 * m.iτ - wu0(m.Lu, p)*m.Lu
end
