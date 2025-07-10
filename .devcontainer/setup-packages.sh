#!/bin/bash
set -e

# Set explicit paths
R_LIBS_USER="/opt/rlibs"

echo "Setting up development environment..."

# Install Julia packages
echo "Installing Julia packages..."
julia --project=code/LandUse.jl -e 'using Pkg; Pkg.instantiate()'

# Install R packages via renv
echo "Installing R packages..."
Rscript -e "install.packages('renv', lib='${R_LIBS_USER}', repos='https://cloud.r-project.org')"

# Add libs location to .Rprofile
echo ".libPaths('${R_LIBS_USER}')" >> code/LandUseR/.Rprofile

# Restore R environment
Rscript -e ".libPaths('${R_LIBS_USER}'); renv::restore(project = 'code/LandUseR', prompt = FALSE, clean = FALSE)"

# Install local R package
Rscript -e ".libPaths('${R_LIBS_USER}');renv::load('code/LandUseR'); install.packages('code/LandUseR',type = 'source', lib='${R_LIBS_USER}', repos = NULL)"

# Verify tmap version
Rscript -e '(packageVersion("tmap") < "4.0") || stop("something went wrong with your renv setup. in this project, you need version tmap < 4.0")'

# Test installations
echo "Testing installations..."
julia --version
R --version
stata-mp -b do code/stata/hello.do || true

echo "Setup complete!"