#!/bin/bash
set -e

runall () {

    echo "WELCOME!"
    echo "========"
    echo ""

    echo "this script will attempt to run the entire replication package in one go."
    echo ""

    echo "step 0.1: which R are you running?"
    echo "======================="
    echo ""

    Rscript -e '(getRversion() == "4.4.2") || stop("you must use R 4.4.2 - look at github.com/r-lib/rig ")'
    echo "step 0.1: done. "
    echo ""


    echo "step 0.2: setup of renv"
    echo "======================="
    echo ""

    cd LandUseR
    Rscript -e 'install.packages("renv", repos = "https://cloud.r-project.org/"); renv::restore(project = ".", prompt = FALSE, clean = TRUE)'
    Rscript -e 'renv::load("."); install.packages(".",type = "source", repos = NULL)' > logR0.log
    Rscript -e '(packageVersion("tmap") < "4.0") || stop("something went wrong with your renv setup. in this project, you need version tmap < 4.0")'


    echo ""
    echo "step 0 done: R ready to go."
    echo "==========================="
    echo ""
    cd ..
    
    echo "step 1: stata."    
    echo "=============="
    echo ""


    stata-mp -b stata/replication_aggregate_main.do

    echo ""
    echo "stata done."
    echo "==========="
    echo ""

    echo "step 2: R data generation"
    echo "========================="
    echo ""


    cd LandUseR
    Rscript -e 'library(LandUseR); LandUseR:::run_data()' > logR2.log
    cd ..

    echo ""
    echo "step 2 done"
    echo "==========="
    echo ""

    echo "step 3: julia structural model - this is long a job!"
    echo ""

    julia -e 'VERSION == v"1.8.5" || error("You MUST use julia 1.8.5 for this. Look at `juliaup` and do `juliaup override set 1.8.5` in the directory where you will start julia")'

    julia --color=yes --project=LandUse.jl -e 'using Pkg; Pkg.instantiate()'
    julia --color=yes --project=LandUse.jl LandUse.jl/run.jl produce --output=all > logjulia.log

    echo ""
    echo "step 3 done"
    echo "==========="
    echo ""

    echo "step 4 R assembling tables"
    echo "=========================="
    echo ""

    cd LandUseR
    Rscript -e 'LandUseR:::run_step3()' > logR3.log
    cd ..

    echo ""
    echo "All done."
    echo "========="
    echo ""


}

time runall