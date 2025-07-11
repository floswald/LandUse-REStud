#!/bin/bash
set -e
export R_LIBS_USER=/opt/rlibs

runall () {

    echo "WELCOME!"
    echo "========"
    echo ""

    echo "this script will attempt to run the entire replication package inside the prebuilt docker image."
    echo ""

    if [ -d "/workspace/code" ]; then
        echo "chaning dirs into /workspace/code"
    else
        echo "Directory /workspace/code does not exist. something is wrong here."
    fi
    cd /workspace/code

    stata-mp -b stata/replication_aggregate_main.do

    echo ""
    echo "stata done."
    echo "==========="
    echo ""

    echo "step 2: R data generation"
    echo "========================="
    echo ""


    cd LandUseR
    Rscript -e ".libPaths('${R_LIBS_USER}'); library(LandUseR); LandUseR:::run_data()" > logR2.log
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
    Rscript -e ".libPaths('${R_LIBS_USER}'); LandUseR:::run_step3()" > logR3.log
    cd ..

    echo ""
    echo "All done."
    echo "========="
    echo ""


}

time runall