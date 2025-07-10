# run file on scpoflo
using LandUse, Flux, CUDA


include("src/flux.jl")

fl.train()