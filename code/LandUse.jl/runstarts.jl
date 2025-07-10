# run file on scpoflo
using LandUse, Flux, CUDA
using Distributed
addprocs(7, exeflags = "--project=.")

@everywhere using LandUse, Flux, CUDA
LandUse.prep_learning()  # 7 points