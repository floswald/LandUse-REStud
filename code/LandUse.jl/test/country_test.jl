
@testset "A Country: baseline" begin

	@testset "5 regions: equal utility" begin
		K = 5
		x,C1,p1 = LandUse.khet(K)
		@test length(C1[1].R) == K
		for it in 1:length(C1)
			for i in 2:K
				@test C1[it].R[i].U ≈ C1[it].R[1].U
			end
		end
	end

	@testset "20 regions with full heterogeneity sanity" begin
		LandUse.khet_run(readdisk = false, writedisk = true);
		x = LandUse.khet_run(readdisk = true)
		@test maximum(abs, x.iDensity - x.Lu) < 1.0e-5
		@test all(x.imode .< x.modeϕ)
	end

	@testset "figure 1 single city with different growth profiles" begin
		p = LandUse.param_4_cities()
		offs = LandUse.OrderedDict("low θr" => 0.95,"high θr" => 1.05,"low θu" => 0.95,"high θu" => 1.05)
        arti = LandUse.artificialθ(p,offs)
		arti.θkt_r_test = arti.θkt_r .* arti.θrt
		arti.θkt_u_test = arti.θkt_u .* arti.θut

		gs = groupby(arti,:year)
		for (i,g) in enumerate(gs)
			if i == 1
				@test all(g.θrt .== 1.0)
				@test all(g.θut .== 1.0)
			else
				@test g[g.region .== 1,:θkt_r_test]  < g[g.region .== 1,:θrt]
				@test g[g.region .== 1,:θkt_u_test] == g[g.region .== 1,:θut]

				@test g[g.region .== 2,:θkt_r_test] > g[g.region .== 2,:θrt]
				@test g[g.region .== 2,:θkt_u_test] == g[g.region .== 2,:θut]

				@test g[g.region .== 3,:θkt_u_test] < g[g.region .== 3,:θut]
				@test g[g.region .== 3,:θkt_r_test] == g[g.region .== 3,:θrt]
				
				@test g[g.region .== 4,:θkt_u_test] > g[g.region .== 4,:θut]
				@test g[g.region .== 4,:θkt_r_test] == g[g.region .== 4,:θrt]
			end
		end
	end
end
