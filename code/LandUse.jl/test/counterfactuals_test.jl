
@testset "sensitivity_ϵ stepper" begin
    o = LandUse.sensitivity_ϵ(readdisk = false, returnplots = false)

    for (k,v) in o
        @test maximum(abs, v.iDensity - v.Lu) < 1.0e-5
        @test all(v.imode .< v.modeϕ)
    end

end

@testset "commuting cost counterfactual" begin
    o = LandUse.sensitivity_ξ(readdisk = false, save = false, returnplots = false)
    
    for (k,v) in o
        @info "$k"
        @test maximum(abs, v.iDensity - v.Lu) < 1.0e-5
        @test all(v.imode .- v.modeϕ .< 1.0e-5) 
    end
end

@testset "commuting d2d2 counterfactual" begin
    o = LandUse.sensitivity_d1d2(readdisk = false, save = false, returnplots = false)
    
    for (k,v) in o
        @info "$k"
        @test maximum(abs, v.iDensity - v.Lu) < 1.0e-5
        @test all(v.imode .- v.modeϕ .< 1.0e-5) 
    end
end