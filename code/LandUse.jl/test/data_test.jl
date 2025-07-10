@testset "Data Inputs" begin

    @testset "test baseline city sample" begin
        d = LandUse.popdata(readdisk = true)
        @test length(unique(d.CODGEO)) == 20
        @test "Paris" ∈ unique(d.LIBGEO)
    end

    @testset "interpolation of model output in odd years" begin
        b = @chain LandUse.khet_run(readdisk = true) begin
            subset(:year => LandUse.leq(2020))
        end
        z = LandUse.interp_model_data(b)

        means = @chain b begin
            subset(:year => LandUse.geq(2010),  :LIBGEO => ByRow(==("Paris")))
            mean(_.citydensity)
        end

        itp = subset(z,:year => LandUse.ieq(2015),  :LIBGEO => ByRow(==("Paris"))).citydensity[1]

        @test itp ≈ means
    end
end