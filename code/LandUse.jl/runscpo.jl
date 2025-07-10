# run file on scpoflo
# start with `julia --project=. -p 7 runscpo.jl`
@everywhere using LandUse
LandUse.parallel_starts(4)