@testset "Single Baseline Region" begin
	x,M,p = LandUse.runm()  # run baseline model

	@testset "components in general model checks" begin
		
		for it in 1:length(p.T)

			LandUse.setperiod!(p,it)
			m = M[it]
			# commuting cost
			@test LandUse.τ(0.4,m.ϕ,p,m.Lu) > 0
			@test LandUse.τ(0.5,m.ϕ,p,m.Lu) > 0
			@test LandUse.τ(0.0,m.ϕ,p,m.Lu) == 0
		
		
			# test wage function
			@test LandUse.w(m.Lu,0.0,m.ϕ,p,m) > LandUse.w(m.Lu,m.ϕ,m.ϕ,p,m)
			@test LandUse.w(m.Lu,0.0,m.ϕ,p,m) > LandUse.w(m.Lu,1.0,m.ϕ,p,m)
			@test LandUse.w(m.Lu,m.ϕ-eps(),m.ϕ,p,m) > LandUse.wu(m.Lu,m.ϕ,m.ϕ,p)
			@test LandUse.w(m.Lu,1.0,m.ϕ,p,m) == LandUse.w(m.Lu,m.ϕ,m.ϕ,p,m)
			@test LandUse.w(m.Lu,0.0,m.ϕ,p,m) == LandUse.wu(m.Lu,m.ϕ,0.0,p)
			@test LandUse.w(m.Lu,0.0,m.ϕ,p,m) == LandUse.wu0(m.Lu,p)
			@test LandUse.w(m.Lu,1.0,m.ϕ,p,m) < LandUse.wu0(m.Lu,p)
		
			# test optimal consumption rules
			@test LandUse.cu(0.0,p,m) == (1.0 - p.γ)*(1.0 - p.ν)*(1/LandUse.pces(m.pr,p))^(1-p.σc) *(LandUse.w(m.Lu,0.0,m.ϕ,p,m) .+ (m.r + p.sbar - m.pr * p.cbar)) - p.sbar
			@test LandUse.cu(m.ϕ,p,m) == (1.0 - p.γ)*(1.0 - p.ν)*(1/LandUse.pces(m.pr,p))^(1-p.σc) *(LandUse.w(m.Lu,m.ϕ,m.ϕ,p,m) .+ (m.r + p.sbar - m.pr * p.cbar)) - p.sbar
			@test LandUse.cr(0.0,p,m) ≈ p.cbar + (1.0 - p.γ)*(p.ν)*(m.pr/LandUse.pces(m.pr,p))^(1-p.σc) *(LandUse.w(m.Lu,0.0,m.ϕ,p,m) .+ (m.r + p.sbar - m.pr * p.cbar)) ./ m.pr
			@test LandUse.cr(m.ϕ,p,m) ≈ p.cbar + (1.0 - p.γ)*(p.ν)*(m.pr/LandUse.pces(m.pr,p))^(1-p.σc) *(LandUse.w(m.Lu,m.ϕ,m.ϕ,p,m) .+ (m.r + p.sbar - m.pr * p.cbar)) ./ m.pr
		
		
			# test excess consumption
			@test LandUse.xsr(p,m) ≈ m.wr + m.r + p.sbar - m.pr * p.cbar
			@test LandUse.xsu(0.0,p,m) ≈ LandUse.w(m.Lu,0.0,m.ϕ,p,m) + m.r + p.sbar - m.pr * p.cbar
			@test LandUse.xsu(0.0,p,m) > LandUse.xsr(p,m)
			@test LandUse.xsu(m.ϕ,p,m) ≈ LandUse.xsr(p,m)
			@test LandUse.xsu(1.0,p,m) ≈ LandUse.xsr(p,m)
		
			# test house price function
			@test LandUse.q(0.0,p,m) >   LandUse.q(1.0,p,m)
			@test LandUse.q(0.0,p,m) >   m.qr
			@test LandUse.q(m.ϕ,p,m) ==  m.qr
			@test LandUse.q(p.S,p,m) ==  m.qr
		
			# test housing consumption
			@test LandUse.h(0.0,p,m) <   LandUse.h(m.ϕ,p,m)
			@test LandUse.h(1.0,p,m) ==  LandUse.h(m.ϕ,p,m)
			@test LandUse.h(1.0,p,m) ≈  p.γ * (m.wr + m.r + p.sbar - m.pr * p.cbar) / m.qr
			@test LandUse.h(0.0,p,m) ≈  p.γ * (m.wu0 + m.r + p.sbar - m.pr * p.cbar) / LandUse.q(0.0,p,m)
			@test LandUse.Srh(p,m) ≈ m.Lr * LandUse.γ(m.ϕ,m.ϕ,p) * (m.wr + m.r + p.sbar - m.pr * p.cbar) / m.ρr
		
			# equation (17)
			l = 0.05
			@test LandUse.D(l,p,m) ≈ (LandUse.q(l,p,m)^(1+LandUse.ϵ(l,m.ϕ,p))) / (p.γ * (LandUse.w(m.Lu,l,m.ϕ,p,m) + m.r + p.sbar - m.pr * p.cbar))
		
			@test LandUse.Yu(m,p) == p.θu * m.Lu

			# integrations
			@test m.Lu ≈ m.iDensity atol=1e-4
			@test m.imode < m.modeϕ
		end
	end

	@testset "full baseline solution" begin	
		tol =  p.S == 1 ? 0.02 : 0.02
		# rutol = 1.0e-2
		# @warn("utility at precision $tol, rural cons $rutol")
		# for it in 1:length(p.T)
		# it = 1
		for it in 1:length(p.T)
			# if it > 8
			# 	tol = 1e-1
			# 	@warn "period $it on requires low tol=$tol" maxlog=1
			# end
			@testset "period $it" begin
				LandUse.setperiod!(p,it)

				rtol = it > 15 ? 0.15 : tol

	
				@test M[it].Sr ≈ p.S - M[it].ϕ^2 * π - M[it].Srh
				@test M[it].Lu ≈ M[it].iDensity atol=1e-4
				@test p.ϵr ≈ LandUse.ϵ(M[it].ϕ,M[it].ϕ,p)
				@test M[it].ρr ≈ (1.0/(1+LandUse.ϵ(M[it].ϕ,M[it].ϕ,p))) * M[it].qr^(1+LandUse.ϵ(M[it].ϕ,M[it].ϕ,p))
				# @test M[it].ρr ≈ (1-p.α) * M[it].pr * p.θr * (p.α * (M[it].Lr / M[it].Sr)^((p.σ-1)/p.σ) + (1-p.α))^(1/(p.σ-1))
				@test p.L  ≈ M[it].Lu + M[it].Lr
	
				# equation (23)
				@test M[it].r * p.L ≈ M[it].iq + M[it].ρr * (p.S - M[it].ϕ^2 * π) atol = 1e-5
		
				# test walras' law: also the rural goods market must clear at equilibrium:
				# equation (25)
				@testset "Walras' Law" begin
					@test p.ν * (1 - p.γ) * (LandUse.pcy(M[it],p) + p.sbar) + M[it].pr * p.cbar * (1.0 - p.ν * (1 - p.γ)) ≈  M[it].pr * LandUse.Yr(M[it],p) / p.L atol=tol
					# @test isapprox( p.ν * (1 - p.γ) * (LandUse.pcy(M[it],p) - M[it].pr * p.cbar) + M[it].pr * p.cbar , M[it].pr * LandUse.Yr(M[it],p) /	 p.L, atol = rutol)
					@test isapprox(p.ν * (1 - p.γ) * (LandUse.pcy(M[it],p) + p.sbar) + M[it].pr * p.cbar * (1.0 - p.ν * (1 - p.γ)) - M[it].pr * LandUse.Yr(M[it],p) / p.L  , 0.0, atol = tol)
					@test isapprox(LandUse.Rmk(M[it],p), 0.0,  atol = rtol)
	
					# try with different formulation of total rural demand:
					# rural cons in city + rural cons for each rural worker == total rural good production
					@test isapprox(M[it].icr + M[it].Lr * LandUse.cr(M[it].ϕ,p,M[it]), LandUse.Yr(M[it],p), atol = rtol)
				end
				# tight tol on utility
				l = rand()
				utol = 1e-9
				@test LandUse.D(l,p,M[it]) ≈ (LandUse.q(l,p,M[it])^(1+LandUse.ϵ(l,M[it].ϕ,p))) / (p.γ * (LandUse.w(M[it].Lu,l,M[it].ϕ,p,M[it]) + M[it].r + p.sbar - M[it].pr * p.cbar))
				@test isapprox(LandUse.utility(1.0,p,M[it]), M[it].U, atol = utol)
				@test isapprox(LandUse.utility(0.1,p,M[it]), M[it].U, atol = utol)
				@test isapprox(LandUse.utility(M[it].ϕ,p,M[it]), M[it].U, atol = utol)
			end
		end
	end

	@testset "Single Region with d1d2" begin
		p1 = LandUse.Param(par = Dict(:d1 => 0.04, :d2 => 1.0))
		x1,M1,p1 = LandUse.run(p1)
	
		@testset "components" begin
			
			for it in 1:length(p1.T)
	
				LandUse.setperiod!(p1,it)
				m = M1[it]
				# commuting distance
				@test LandUse.d(0.0,m.ϕ,p1) > 0
				if it > 2
					@test LandUse.d(m.ϕ,m.ϕ,p1) < m.ϕ
				end
				@test LandUse.d(m.ϕ,m.ϕ,p1) == p1.d1 * m.ϕ + m.ϕ / (1 + p1.d2 * m.ϕ)
				@test LandUse.d(10 * eps(),m.ϕ,p1) == p1.d1 * m.ϕ + 10 * eps() / (1 + p1.d2 * m.ϕ)

				# commuting cost
				@test LandUse.τ(0.4,m.ϕ,p1,m.Lu) > 0
				@test LandUse.τ(0.5,m.ϕ,p1,m.Lu) > 0
				@test LandUse.τ(0.0,m.ϕ,p1,m.Lu) > 0
			
			
				# test wage function
				@test LandUse.w(m.Lu,0.0,m.ϕ,p1,m) > LandUse.w(m.Lu,m.ϕ,m.ϕ,p1,m)
				@test LandUse.w(m.Lu,0.0,m.ϕ,p1,m) > LandUse.w(m.Lu,m.ϕ,m.ϕ,p1,m)
				@test LandUse.w(m.Lu,0.0,m.ϕ,p1,m) == LandUse.wu(m.Lu,m.ϕ,0.0,p1)
				@test LandUse.w(m.Lu,1.0,m.ϕ,p1,m) < LandUse.wu0(m.Lu,p1)
				@test m.wu0 > LandUse.w(m.Lu,0.0,m.ϕ,p1,m)
				@test LandUse.wu0(m.Lu,p1) > LandUse.w(m.Lu,0.0,m.ϕ,p1,m)
			
				# test optimal consumption rules
				@test LandUse.cu(0.0,p1,m) == (1.0 - p1.γ)*(1.0 - p1.ν)*(1/LandUse.pces(m.pr,p1))^(1-p1.σc) * (LandUse.w(m.Lu,0.0,m.ϕ,p1,m) .+ (m.r + p1.sbar - m.pr * p1.cbar)) - p1.sbar
				@test LandUse.cu(m.ϕ,p1,m) == (1.0 - p1.γ)*(1.0 - p1.ν)*(1/LandUse.pces(m.pr,p1))^(1-p1.σc) * (LandUse.w(m.Lu,m.ϕ,m.ϕ,p1,m) .+ (m.r + p1.sbar - m.pr * p1.cbar)) - p1.sbar
				@test LandUse.cr(0.0,p1,m) ≈ p1.cbar + (1.0 - p1.γ)*(p1.ν)*(m.pr/LandUse.pces(m.pr,p1))^(1-p1.σc) * (LandUse.w(m.Lu,0.0,m.ϕ,p1,m) .+ (m.r + p1.sbar - m.pr * p1.cbar)) ./ m.pr
				@test LandUse.cr(m.ϕ,p1,m) ≈ p1.cbar + (1.0 - p1.γ)*(p1.ν)*(m.pr/LandUse.pces(m.pr,p1))^(1-p1.σc) * (LandUse.w(m.Lu,m.ϕ,m.ϕ,p1,m) .+ (m.r + p1.sbar - m.pr * p1.cbar)) ./ m.pr
			
			
				# test excess consumption
				@test LandUse.xsr(p1,m) ≈ m.wr + m.r + p1.sbar - m.pr * p1.cbar
				@test LandUse.xsu(0.0,p1,m) ≈ LandUse.w(m.Lu,0.0,m.ϕ,p1,m) + m.r + p1.sbar - m.pr * p1.cbar
				@test LandUse.xsu(0.0,p1,m) > LandUse.xsr(p1,m)
				@test LandUse.xsu(m.ϕ,p1,m) ≈ LandUse.xsr(p1,m)
				@test LandUse.xsu(1.0,p1,m) ≈ LandUse.xsr(p1,m)
			
				# test house price function
				@test LandUse.q(0.0,p1,m) >   LandUse.q(1.0,p1,m)
				@test LandUse.q(0.0,p1,m) >   m.qr
				@test LandUse.q(m.ϕ,p1,m) ==  m.qr
				@test LandUse.q(p1.S,p1,m) ==  m.qr
			
				# test housing consumption
				@test LandUse.h(0.0,p1,m) <   LandUse.h(m.ϕ,p1,m)
				@test LandUse.h(1.0,p1,m) ≈  p1.γ * (m.wr + m.r + p1.sbar - m.pr * p1.cbar) / m.qr
				@test LandUse.h(0.0,p1,m) ≈  p1.γ * (LandUse.w(m.Lu,0.0,m.ϕ,p1,m) + m.r + p1.sbar - m.pr * p1.cbar) / LandUse.q(0.0,p1,m)
				@test LandUse.Srh(p1,m) ≈ m.Lr * LandUse.γ(m.ϕ,m.ϕ,p1) * (m.wr + m.r + p1.sbar - m.pr * p1.cbar) / m.ρr
			
				# equation (17)
				l = 0.05
				@test LandUse.D(l,p1,m) ≈ (LandUse.q(l,p1,m)^(1+LandUse.ϵ(l,m.ϕ,p1))) / (p1.γ * (LandUse.w(m.Lu,l,m.ϕ,p1,m) + m.r + p1.sbar - m.pr * p1.cbar))
			
				@test LandUse.Yu(m,p1) == p1.θu * m.Lu
			end
		end
	
		@testset "full solution" begin	
			tol =  p1.S == 1 ? 0.03 : 0.03
			# rutol = 1.0e-2
			# @warn("utility at precision $tol, rural cons $rutol")
			# for it in 1:length(p.T)
			# it = 1
			for it in 1:length(p.T)
				# if it > 8
				# 	tol = 1e-1
				# 	@warn "period $it on requires low tol=$tol" maxlog=1
				# end
				@testset "period $it" begin
					LandUse.setperiod!(p1,it)

					rtol = it > 15 ? 0.15 : tol
		
					@test M[it].Sr ≈ p1.S - M[it].ϕ^2 * π - M[it].Srh
					@test M[it].Lu ≈ M[it].iDensity atol=1e-4
					@test p1.ϵr ≈ LandUse.ϵ(M[it].ϕ,M[it].ϕ,p1)
					@test M[it].ρr ≈ (1.0/(1+LandUse.ϵ(M[it].ϕ,M[it].ϕ,p1))) * M[it].qr^(1+LandUse.ϵ(M[it].ϕ,M[it].ϕ,p1))
					# @test M[it].ρr ≈ (1-p1.α) * M[it].pr * p1.θr * (p1.α * (M[it].Lr / M[it].Sr)^((p1.σ-1)/p1.σ) + (1-p1.α))^(1/(p1.σ-1))
					@test p1.L  ≈ M[it].Lu + M[it].Lr
		
					# equation (23)
					@test M[it].r * p1.L ≈ M[it].iq + M[it].ρr * (p1.S - M[it].ϕ^2 * π) atol = 1e-5
			
					# test walras' law: also the rural goods market must clear at equilibrium:
					# equation (25)
					@testset "Walras' Law" begin
						@test p1.ν * (1 - p1.γ) * (LandUse.pcy(M[it],p1) + p1.sbar) + M[it].pr * p1.cbar * (1.0 - p1.ν * (1 - p1.γ)) ≈  M[it].pr * LandUse.Yr(M[it],p1) / p1.L atol=tol
						# @test isapprox( p1.ν * (1 - p1.γ) * (LandUse.pcy(M[it],p1) - M[it].pr * p1.cbar) + M[it].pr * p1.cbar , M[it].pr * LandUse.Yr(M[it],p1) /	 p1.L, atol = rutol)
						@test isapprox(p1.ν * (1 - p1.γ) * (LandUse.pcy(M[it],p1) + p1.sbar) + M[it].pr * p1.cbar * (1.0 - p1.ν * (1 - p1.γ)) - M[it].pr * LandUse.Yr(M[it],p1) / p1.L  , 0.0, atol = tol)
						@test isapprox(LandUse.Rmk(M[it],p1), 0.0,  atol = rtol)
		
						# try with different formulation of total rural demand:
						# rural cons in city + rural cons for each rural worker == total rural good production
						@test isapprox(M[it].icr + M[it].Lr * LandUse.cr(M[it].ϕ,p1,M[it]), LandUse.Yr(M[it],p1), atol = rtol)
					end
					# tight tol on utility
					l = rand()
					utol = 1e-9
					@test LandUse.D(l,p1,M[it]) ≈ (LandUse.q(l,p1,M[it])^(1+LandUse.ϵ(l,M[it].ϕ,p1))) / (p1.γ * (LandUse.w(M[it].Lu,l,M[it].ϕ,p1,M[it]) + M[it].r + p1.sbar - M[it].pr * p1.cbar))
					@test isapprox(LandUse.utility(1.0,p1,M[it]), M[it].U, atol = utol)
					@test isapprox(LandUse.utility(0.1,p1,M[it]), M[it].U, atol = utol)
					@test isapprox(LandUse.utility(M[it].ϕ,p1,M[it]), M[it].U, atol = utol)
				end
			end
		end

		@testset "compare to baseline" begin
			
			for it in 1:length(p1.T)
				@test M[it].ϕ < M1[it].ϕ
				@test LandUse.cityarea(M[it]) < LandUse.cityarea(M1[it])
			end
		end
	end

	@testset "changing param constructor" begin
		p = LandUse.Param()
		p1 = LandUse.Param(par = Dict(:Lflat => true))
		@test !all(p.Lt .== p1.Lt)
		@test all(p1.Lt .== 1.0)
	end

		


end

