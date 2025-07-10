using LandUse
using Flux
using Test
using LinearAlgebra
using DataFrames
using Chain
using StatsBase
using DataFramesMeta

@testset "LandUse.jl" begin
	include("data_test.jl")
	include("model_test.jl")
	include("country_test.jl")
	include("counterfactuals_test.jl")
	include("output_test.jl")
end
