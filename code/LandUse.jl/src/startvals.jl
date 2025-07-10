
function nocommute!(F::Vector,x::Vector,p::Param)
	gamma2 = p.γ / (1 + p.ϵr)

	if any(x .< 0)
		F[:] .= PEN
	else
		ρ = x[1]
		Lr = x[2]
		# println("rho = $ρ, Lr = $Lr")

		Lu = p.L - Lr
		wu = p.θu*Lu^p.λ
		wr = wu
		Sr = (((1 - p.α)/ p.α) * wr / ρ)^p.σ * Lr # farm land input
		r  = ρ * (p.S - p.useless) / p.L
		pr = wr / (p.α * p.θr) * (p.α + (1-p.α) * (Lr / Sr)^((1-p.σ)/p.σ))^(1/(1-p.σ))
		ϕ  = gamma2 * (wu + r - pr * p.cbar + p.sbar ) * Lu / (ρ)
		Srh= gamma2 * (wr + r - pr * p.cbar + p.sbar ) * Lr / (ρ)

		F[1] = (1 - p.γ) * (1 - p.ν) * (wr + r - pr * p.cbar + p.sbar ) + p.ϵr * ρ * (Srh + ϕ) - p.sbar * p.L - Lu * p.θu
		F[2] = Sr + Srh + ϕ - (p.S - p.useless)

	end
end

function stmodel(p::Param)
	x0 = [1.0,0.5]
	x00 = nlsolve((F,x) -> nocommute!(F,x,p),x0)
	gamma2 = p.γ / (1 + p.ϵr)

	ρ  = x00.zero[1]
	Lr = x00.zero[2]
	Lu = p.L - Lr
	wu = p.θu*Lu^p.λ
	wr = wu
	Sr = (((1 - p.α)/ p.α) * wr / ρ)^p.σ * Lr # farm land input
	r  = ρ * (p.S - p.useless) / p.L
	pr = wr / (p.α * p.θr) * (p.α + (1-p.α) * (Lr / Sr)^((1-p.σ)/p.σ))^(1/(1-p.σ))
	ϕ  = gamma2 * (wu + r - pr * p.cbar + p.sbar ) * Lu / (ρ)
	Srh= gamma2 * (wr + r - pr * p.cbar + p.sbar ) * Lr / (ρ)

	(ρr = ρ, ϕ = ϕ/10, r = r, Lr = Lr, pr = pr, Sr = Sr, θu = p.θu, θr = p.θr)

end
startnames() = (:ρr , :ϕ, :r , :Lr , :pr , :Sr, :θu, :θr )

startval(p::Param) = stmodel(p)

"""
    p2Flux(p::Param)

map param to Flux Chain to get starting value
"""
function p2Flux(p::Param)
    [ p.cbar,  p.sbar, p.ηl, p.ηw, p.a ]
end

"you just had a failed start at p"
function nearstart(p::Param)
	x = p2Flux(p)
	(; zip(startnames(), [p.Chain(x)...,1.0,1.0])...)
end
