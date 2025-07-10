# using DocOpt  # import docopt function

using DocOpt  # import docopt function
using Distributed


doc = """

Land Use, Structural Change and Urban Expansion

     | Welcome to the LandUse.jl runfile!
     | See below how to use this code.
     | Thanks!

Usage:
  run.jl -h | --help
  run.jl estimate [--nwork=<nworkers>] [--maxsteps=<nsteps>] [--savefreq=<nsave>] [--method=<mname>] [--scenario=<sname>]
  run.jl produce [--output=<oname>] [--readdisk]
  run.jl --version

Options:
  -h --help     Show this screen.
  --version     Show version.
  --nwork=<nworkers>    number of workers [default: 1]
  --maxsteps=<nsteps>   maximum steps to take [default: 8]
  --savefreq=<nsave>    after how many steps to save [default: 8]
  --method=<mname>      use method [default: dxnes ]. choices: `adaptive_de_rand_1_bin`, `xnes`, `separable_nes`
  --scenario=<sname>    estimation scenario [default: default ]. choices: `agglomu`, `agglola`, `d1d2`
  --output=<oname>      run model and produce [default: all]. baseline, all, nopopgrowth
"""


args = docopt(doc, version=v"1.4")

# to debug
# for (k,v) in args
#     println("key $k, value = $v")
# end

if args["estimate"]
    nw = parse(Int,args["--nwork"])
    ns = parse(Int,args["--maxsteps"])
    sf = parse(Int,args["--savefreq"])

    scen = args["--scenario"]
    if scen == "agglola1" 
     	scen = :aggloλ1
    elseif scen == "agglola2" 
        scen = :aggloλ2
    elseif scen == "agglomu"
      	scen = :aggloμ
    elseif scen == "d1d2"
      	scen = :d1d2
    elseif scen == "sample2"
      	scen = :sample2
    else 
      	scen = :default
    end

    println("julia version: $VERSION")

    println("estimating on $nw workers, doing $ns steps, saving after $sf steps")
    println("using $(args["--method"])")
    println("estimating scenario $scen")

    addprocs(nw, exeflags = "--project=.")
    @everywhere using Pkg
    @everywhere Pkg.instantiate()

    @everywhere using LandUse, Flux
    @info "This is LandUse.jl, Revision $(LandUse.revision())"
    
    LandUse.runm() # warm up Ipopt on master

    # run estimation proper
    LandUse.runestim(steps = ns, method = Symbol(strip(args["--method"])), scenario = scen, savefreq = sf)  # 7 points

elseif args["produce"]

	using Pkg
	Pkg.instantiate()
    using LandUse, Flux
    @info "This is LandUse.jl, Revision $(LandUse.revision())"

    scen = args["--scenario"]
    # readdisk = args["--readdisk"]

    if args["--output"] == "baseline"
		LandUse.export_baseline(readdisk = false, save = true)
    elseif args["--output"] == "nopopgrowth"
		LandUse.sensitivity_nopopgrowth(readdisk = false)
    elseif args["--output"] == "all"
		LandUse.full_pipeline()
    end
end
