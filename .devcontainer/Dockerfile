# Use CUDA 13 devel image (required for NVRTC/Numba per RAPIDS docs)
FROM ghcr.io/rocker-org/cuda


# Default to standard build (reusing base image torch 2.10)
ARG BUILD_TYPE=standard

# Set reticulate to use the system python (in /opt/venv for this image)
ENV RETICULATE_PYTHON=/opt/venv/bin/python

# Install Python packages
# If BUILD_TYPE is "nightly", install the heavy nightly torch requirements.
# If "standard", skip this to reuse the base image's existing torch 2.10.
COPY requirements.txt /tmp/requirements.txt
RUN if [ "$BUILD_TYPE" = "nightly" ]; then \
        pip install --no-cache-dir -r /tmp/requirements.txt; \
    else \
        echo "Standard build: Skipping requirements.txt (reusing base image torch)"; \
    fi

# Install system dependencies required by OpenCV and SpeciesNet
# libgl1 is the vendor-neutral GL runtime (provides libGL.so.1) needed by opencv-python
# libglib2.0-0 is typically needed as well
USER root
RUN apt-get update && apt-get install -y \
    libgl1 \
    libglib2.0-0 \
    locales \
    && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
    && locale-gen \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8

# Install speciesnet package into the system python environment (as root)
# This ensures it is available to all users and is in the same environment as torch
RUN pip install --no-cache-dir speciesnet

# Switch to user for R package installation
USER ${NB_USER}

RUN R -e "install.packages(c('devtools', 'remotes')); remotes::install_github('boettiger-lab/speciesnet')"
RUN R -e "speciesnet::install_speciesnet()"
RUN R -e "speciesnet::load_speciesnet()"

# The model weights (~214MB) will be downloaded on first use
# and cached in /home/${NB_USER}/.cache/kagglehub/

USER ${NB_USER}
