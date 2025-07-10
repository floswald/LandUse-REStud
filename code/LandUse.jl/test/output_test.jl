@testset "model outputs" begin

    @testset "per capita in output correct" begin
        b = LandUse.khet_run(readdisk = true)

        # check that GDP per capita uses the correct pop measure
        @test all( b.GDP .≈ (b.Yu + b.pr .* b.Yr) ./ b.pop )
    end


    @testset "baseline aggregation is correct" begin
        b = LandUse.khet_run(readdisk = true)

        # create constant Lu columns
        b1870 = select(@subset(b, :year .== 1870), :region, :Lu => :Lu_1870_)
        b2020 = select(@subset(b, :year .== 2020), :region, :Lu => :Lu_2020_)
        
        # join to baseline
        b = @chain b begin
            innerjoin(b1870, on = :region)
            innerjoin(b2020, on = :region)
        end

        # final result
        a = @chain b begin
            LandUse.aggregator(_)
        end

        # check GDP per capita is correctly aggregating
        bc = combine(
            groupby(b, :year),
            [:GDP, :pop] => ((y,p) -> sum(y .* p)) => :GDP_agg,
            [:y, :pop] => ((y,p) -> sum(y .* p)) => :y_disposable_agg,
            )
        
        @test maximum(abs, a.GDP_agg .- bc.GDP_agg) < 1e-10
        @test maximum(abs, a.y_disposable_agg .- bc.y_disposable_agg) < 1e-10
        

        # we want to compare some basic outcomes when going from b to a
        # this tests aggregation by years - no normalization yet!

        @test @chain b begin
            groupby(:year)
            combine(
                [:Yu, :Yr, :pr] => ((yu,yr,p) -> sum(yu .+ (p .* yr))) => :GDP_agg_,
                [:Lu, :Lr]      => ((x,y) -> sum(x .+ y)) => :pop_agg_,
                [:Lu, :Lr]     .=> sum .=> [:Lu_agg_, :Lr_agg_],
                :cityarea       => sum => :cityarea_agg_,
                :iτ       => sum => :iτ_agg_,
                [:d0, :Lu]      => ((x,y) -> mean(x, Weights(y))) => :mean_d0_,
                [:citydensity, :Lu] => ((x,y) -> mean(x, Weights(y))) => :mean_citydensity_,
                [:citydensity, :Lu_1870_] => ((x,y) -> mean(x, Weights(y))) => :mean_citydensity_Lu_1870_,
                [:citydensity, :Lu_2020_] => ((x,y) -> mean(x, Weights(y))) => :mean_citydensity_Lu_2020_,
                [:imode, :Lu]       => ((x,y) -> mean(x, Weights(y))) => :mean_commuting_speed_,
            )

            leftjoin(a,on = :year)  # output of the aggregator function
            transform(
                # check whether computed and values in `a`` are approx the same
                [
                    ["$i","$(i)_"] => ((x,y) -> x .≈ y) => "$(i)_test" for i in 
                        ["GDP_agg","pop_agg","Lu_agg","Lr_agg","cityarea_agg", "mean_citydensity","mean_citydensity_Lu_1870","mean_citydensity_Lu_2020","mean_commuting_speed","iτ_agg"]
                ]
            )
            select(r"_test")
            prod(Matrix(_)) == 1  # all true?
        end

        # test normalizations
        @test @chain a begin
            subset(:year => LandUse.leq(2020))
            
            select(
                [:GDP_agg,:iτ_agg, :y_disposable_agg] => ((x,y,z) -> x .- y .== z),
                [:Lu_agg, :year,:Lu_agg_1870] => ((x,y,z) -> (x ./ x[y .== 1870]) .== z),
                [:mean_citydensity, :year,:mean_citydensity_1870] => ((x,y,z) -> (x ./ x[y .== 1870]) .== z),
                [:mean_citydensity_Lu_1870, :year,:mean_citydensity_Lu_1870_1870] => ((x,y,z) -> (x ./ x[y .== 1870]) .== z),
                [:mean_citydensity_Lu_2020, :year,:mean_citydensity_Lu_2020_1870] => ((x,y,z) -> (x ./ x[y .== 1870]) .== z),
                [:mean_HPI_center, :year,:mean_HPI_center_1870] => ((x,y,z) -> 100 .* (x ./ x[y .== 1870]) .== z),
                [:mean_HPI, :year,:mean_HPI_1870] => ((x,y,z) -> 100 .* (x ./ x[y .== 1870]) .== z),

            # 1840 norms
                [:Lu_agg, :year,:Lu_agg_1840] => ((x,y,z) -> (x ./ x[y .== 1840]) .== z),
                [:mean_ρr, :year,:mean_ρr_1840] => ((x,y,z) -> (x ./ x[y .== 1840]) .== z),
                [:mean_commuting_speed, :year,:mean_commuting_speed_1840] => ((x,y,z) -> (x ./ x[y .== 1840]) .== z),
                [:mean_citydensity, :year,:mean_citydensity_1840] => ((x,y,z) -> (x ./ x[y .== 1840]) .== z),
                [:mean_HPI_center,  :year,:mean_HPI_center_1840] => ((x,y,z) -> 100 .* (x ./ x[y .== 1840]) .== z),
                [:mean_HPI,         :year, :mean_HPI_1840] => ((x,y,z) -> 100 .* (x ./ x[y .== 1840]) .== z),

            # p1950 norm
                [:mean_pr,:year, :mean_pr_1950] => ((x,y,z) -> (x ./ x[y .== 1950]) .== z)
            )
            prod(Matrix(_)) == 1 # all true?
        end
    end



    @testset "test baseline aggreation from csvs" begin
        b = LandUse.read_baseline_csv()
        a = LandUse.read_baseline_aggregated_csv() |> dropmissing

        @test @chain b begin
            subset(:year => LandUse.leq(2020))

            groupby(:year)
            combine(
                :itau       => sum => :itau_agg_,
                [:Yu, :Yr, :pr] => ((yu,yr,p) -> sum(yu .+ (p .* yr))) => :GDP_agg_,
                [:Lu, :Lr]      => ((x,y) -> sum(x .+ y)) => :pop_agg_,
                [:Lu, :Lr]     .=> sum .=> [:Lu_agg_, :Lr_agg_],
                :cityarea       => sum => :cityarea_agg_,
                [:d0, :Lu]      => ((x,y) -> mean(x, Weights(y))) => :mean_d0_,
                [:citydensity, :Lu] => ((x,y) -> mean(x, Weights(y))) => :mean_citydensity_,
                [:imode, :Lu]       => ((x,y) -> mean(x, Weights(y))) => :mean_commuting_speed_,
            )

            leftjoin(a ,on = :year)  # output of the aggregator function
            transform(
                # check whether computed and values in `a`` are approx the same
                [
                    ["$i","$(i)_"] => ((x,y) -> x .≈ y) => "$(i)_test" for i in 
                        ["GDP_agg","pop_agg","Lu_agg","Lr_agg","cityarea_agg", "mean_citydensity","mean_commuting_speed","itau_agg"]
                ]
            )
            select(r"_test")
            prod(Matrix(_)) == 1  # all true?
        end

         # test normalizations
         @test @chain a begin
            subset(:year => LandUse.leq(2020))
            # 1870 norms
            select(
                [:GDP_agg,:itau_agg, :y_disposable_agg] => ((x,y,z) -> x .- y .≈ z),
                [:Lu_agg, :year,:Lu_agg_1870] => ((x,y,z) -> (x ./ x[y .== 1870]) .≈ z),
                [:mean_citydensity, :year,:mean_citydensity_1870] => ((x,y,z) -> (x ./ x[y .== 1870]) .≈ z),
                [:mean_HPI_center, :year,:mean_HPI_center_1870] => ((x,y,z) -> 100 .* (x ./ x[y .== 1870]) .≈ z),
                [:mean_HPI, :year,:mean_HPI_1870] => ((x,y,z) -> 100 .* (x ./ x[y .== 1870]) .≈ z),

            # 1840 norms
                [:Lu_agg, :year,:Lu_agg_1840] => ((x,y,z) -> (x ./ x[y .== 1840]) .≈ z),
                [:mean_rhor, :year,:mean_rhor_1840] => ((x,y,z) -> (x ./ x[y .== 1840]) .≈ z),
                [:mean_commuting_speed, :year,:mean_commuting_speed_1840] => ((x,y,z) -> (x ./ x[y .== 1840]) .≈ z),
                [:mean_citydensity, :year,:mean_citydensity_1840] => ((x,y,z) -> (x ./ x[y .== 1840]) .≈ z),
                [:mean_HPI_center,  :year,:mean_HPI_center_1840] => ((x,y,z) -> 100 .* (x ./ x[y .== 1840]) .≈ z),
                [:mean_HPI,         :year, :mean_HPI_1840] => ((x,y,z) -> 100 .* (x ./ x[y .== 1840]) .≈ z),

            # p1950 norm
                [:mean_pr,:year, :mean_pr_1950] => ((x,y,z) -> (x ./ x[y .== 1950]) .≈ z)
            )
            prod(Matrix(_)) == 1 # all true?
        end
    end
end