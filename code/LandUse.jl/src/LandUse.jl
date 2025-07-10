module LandUse

	# dependendencies
	using Base: root_module
	using JSON
	using NLsolve
	using FastGaussQuadrature
	using Plots
	using LaTeXStrings
	# using Interact
	using DataFrames
	using StatsPlots
	using DataFramesMeta
	using Printf
	using LineSearches
	using Formatting
	using Roots
	using ColorSchemes
	using CSV
	using SmoothingSplines
	using LaTeXTabulars
	using QuantEcon: smooth, gridmake
	using JuMP
	using Ipopt
	using Interpolations
	using Impute
	# using Blink
	using BlackBoxOptim
	using Serialization
	using Dates
	using LsqFit
	using SharedArrays
	using ProgressMeter
	using Distributed
	using DelimitedFiles
	using BSON
	using Flux
	using GLM
	using FileIO
	using JLD2
	using StatsBase
	using Statistics
	using StatFiles
	using Chain
	using RegressionTables
	using CategoricalArrays
	using ShiftedArrays: lag
	using OrderedCollections
	using LinearAlgebra
	using Random
	using PrettyTables
	using Infiltrator
	using NLboxsolve


	# constants
	const PEN = 100.0  # penalty for nl solver

	# set up file paths
	isCI = if haskey(ENV,"CI") || haskey(ENV,"MAGI")
			true 
		else
			false
		end

	make_appendix_C = haskey(ENV,"LANDUSE_C")

	# root dir
	root = joinpath(@__DIR__,"..")
	indir = joinpath(root,"in")
	outdir = joinpath(root,"out")
	intables = joinpath(indir,"tables")

	revision() = 3  # makes file paths in accordance to revision
	
	function dbpath()
		if haskey(ENV,"LANDUSE_DEV")
			p = joinpath(ENV["HOME"],"Dropbox","research","LandUse")
			@warn "You are running this package in development mode. We will *not* write results into the replication packages `output/` directory, but rather to this location: $p" maxlog=1
			# this is a fail safe setting for the dev version:
			return p
		else
			return joinpath(@__DIR__,"..","..","..")
		end
	end			
	nsdir() = joinpath(dbpath(),"not-shared")
	nsplots() = joinpath(nsdir(),"output","plots")
	dbplots()     = joinpath(dbpath(),"output","model","plots")
	dboutdata()   = joinpath(dbpath(),"output","data")
	dbdataplots() = joinpath(dbpath(),"output","data","plots")
	dbtables()    = joinpath(dbpath(),"output","model","tables")
	dbdata()    = joinpath(dbpath(),"data")
	dbcasd()      = joinpath(dbpath(),"output","CASD")
	dbletters()      = joinpath(dbpath(),"RESTud","round" * "$(revision())")
	slide_dir   = joinpath(@__DIR__,"..","tex","slides")
	
	rawdata_loc() = joinpath(dbpath(),"data","processed","FRA_model.csv")

	# imports
	import Base.show, Base.convert

	# our code
	include("param.jl")
	include("model.jl")
	include("dataframe-utils.jl")
	include("urban.jl")
	include("country.jl")
	include("hetr-country.jl")
	include("startvals.jl")
	include("running.jl")
	include("plotter_old.jl")
	include("plotter.jl")
	include("sensi-lowthetar.jl")
	include("sensi-agglo.jl")
	include("sensi-epsilon.jl")
	include("sensi-omega.jl")
	include("sensi-sigma.jl")
	include("sensi-commcost.jl")
	include("sensi-d1d2.jl")
	include("sensi-nopopgrowth.jl")
	include("sensi-hetgrowth.jl")
	include("experiments.jl")
	# include("interact.jl")
	include("jump-old.jl")
	include("jump.jl")
	include("estimation.jl")
	include("learning.jl")
	include("data.jl")
	include("app-numillust.jl")
	include("app-numillust-sbar.jl")
	include("sensi-R3-fixedthetas.jl")
	include("sensi-fixedrho.jl")
	include("R1-point3.jl")
	include("issues.jl")
	include("sensi-flat-hshare.jl")
	

	function cpslides(name)
		cp(joinpath(root,"tex","slides","COT_slides.pdf"),
	                   joinpath(dbpath(),"slides","flo-slides","COT_slides-$name.pdf"),
					   force = true)
		cp(joinpath(root,"tex","slides","COT_slides.tex"),
	                   joinpath(dbpath(),"slides","flo-slides","COT_slides-$name.tex"),
					   force = true)
		cp(joinpath(root,"tex","slides","preamble.tex"),
	                   joinpath(dbpath(),"slides","flo-slides","preamble.tex"),
					   force = true)
	end

	"Function to export this repo to a separate replication package folder"
	function cpthis(to)
		mkpath(joinpath(to,"src"))
		mkpath(joinpath(to,"test"))
		mkpath(joinpath(to,"in","tables"))
		mkpath(joinpath(to,"out"))

		Base.Filesystem.cptree(@__DIR__, joinpath(to,"src"),force = true)
		Base.Filesystem.cptree(joinpath(@__DIR__,"..","test"), joinpath(to,"test"),force = true)
		cp(joinpath(@__DIR__,"..","in","tables","mymodel.bson"),joinpath(to,"in","tables","mymodel.bson"))
		cp(joinpath(@__DIR__,"..","in","tables","population.csv"),joinpath(to,"in","tables","population.csv"))
		cp(joinpath(@__DIR__,"..","in","tables","data-moments.csv"),joinpath(to,"in","tables","data-moments.csv"))

		rd = readdir(joinpath(@__DIR__,".."))
		for r in rd[endswith.(rd, ".run")]
			cp(r,joinpath(to,r))
		end
		for r in rd[endswith.(rd,".jl")]
			cp(r,joinpath(to,r))
		end
		for r in rd[endswith.(rd,".toml")]
			cp(r,joinpath(to,r))
		end
	end


	slides_towebsite() = cp(joinpath(slide_dir,"COT_slides.pdf"),joinpath(ENV["HOME"],"git","floswald.github.io","static","pdf","landuse-slides.pdf"),force = true)


end # module
