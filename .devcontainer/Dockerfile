FROM --platform=linux/amd64 rocker/r-ver:4.4.0 AS rstage

FROM --platform=linux/amd64 dataeditors/stata18_5-mp:2025-02-26

USER root

# Add ubuntugis PPA for spatial libraries
RUN apt-get update && apt-get install -y \
    software-properties-common \
    && add-apt-repository ppa:ubuntugis/ubuntugis-unstable \
    && apt-get update

# Install base dependencies + terra/spatial dependencies
RUN apt-get install -y \
    wget curl gnupg ca-certificates \
    build-essential libreadline-dev libx11-dev libxt-dev \
    libpng-dev libjpeg-dev libcairo2-dev libssl-dev libxml2-dev \
    zlib1g-dev libbz2-dev liblzma-dev libcurl4-openssl-dev \
    libgdal-dev libgeos-dev libproj-dev libtbb-dev libnetcdf-dev \
    cmake libabsl-dev\
    && rm -rf /var/lib/apt/lists/*

# Install R runtime dependencies
RUN apt-get update && apt-get install -y \
    libblas3 liblapack3 libgfortran5 libreadline8 \
    libpcre2-8-0 libcurl4 libssl3 libxml2 libxt6 \
    libjpeg-turbo8 libpng16-16 gfortran \
    liblapack-dev libblas-dev libudunits2-dev \
    libharfbuzz-dev libfribidi-dev libfreetype6-dev pkg-config \
    && rm -rf /var/lib/apt/lists/* \
    && ldconfig

# Copy R from rocker stage
COPY --from=rstage /usr/local/bin/R* /usr/local/bin/
COPY --from=rstage /usr/local/lib/R /usr/local/lib/R

RUN ldconfig && R --version

# --------------------------------------
# 1. Root user: Install Julia, R, tools, set env vars
# --------------------------------------
ENV JULIA_VERSION=1.8.5
ENV JULIA_DIR=/opt/julia-${JULIA_VERSION}

# --- Julia install ---
RUN curl -fsSL https://julialang-s3.julialang.org/bin/linux/x64/1.8/julia-${JULIA_VERSION}-linux-x86_64.tar.gz \
    | tar -xz -C /opt && \
    ln -s ${JULIA_DIR}/bin/julia /usr/local/bin/julia

# --------------------------------------
# 2. Setup persistent library paths
# --------------------------------------

# Shared R library location
ENV RENV_PATHS_LIBRARY=/opt/renv
ENV R_LIBS_USER=/opt/rlibs
RUN mkdir -p ${RENV_PATHS_LIBRARY} && chown -R statauser:stata ${RENV_PATHS_LIBRARY}
RUN mkdir -p ${R_LIBS_USER} && chown -R statauser:stata ${R_LIBS_USER}

RUN mkdir -p /usr/local/lib/R/etc
RUN echo "R_LIBS_USER=$R_LIBS_USER" >> /usr/local/lib/R/etc/Renviron.site

# fixing permissions issue
RUN chown -R statauser:stata /usr/local/lib/R/site-library

# Shared Julia package depot
ENV JULIA_DEPOT_PATH=/opt/julia_depot
RUN mkdir -p /opt/julia_depot && chown -R statauser:stata /opt/julia_depot
# turn off active display for julia Plots.jl on the container
ENV GKSwstype="nul"

# Create mount points (empty directories that will be mounted)
RUN mkdir -p /workspace/data /workspace/output /workspace/code && \
    chown -R statauser:stata /workspace

# Copy static files that don't change often
COPY LICENSE.txt /home/statauser/LICENSE.txt
COPY README.pdf /home/statauser/README.pdf

# set env variable for R package
RUN echo 'R_LANDUSE=/workspace' > /home/statauser/.Renviron && \
    chown statauser:stata /home/statauser/.Renviron

# --------------------------------------
# 3. Switch to statauser for final setup
# --------------------------------------
USER statauser
WORKDIR /workspace


# unset stata entrypoint
ENTRYPOINT []

# Default command
CMD ["/bin/bash"]