ARG BASE_IMAGE=ghcr.io/rocker-org/ml:latest
FROM ${BASE_IMAGE}

# Install system dependencies required by OpenCV and SpeciesNet
# libgl1 is the vendor-neutral GL runtime (provides libGL.so.1) needed by opencv-python
# libglib2.0-0 is typically needed as well
USER root
RUN apt-get update && apt-get install -y \
    libgl1 \
    libglib2.0-0 \
    && rm -rf /var/lib/apt/lists/*

USER ${NB_USER}

# Pre-install ALL speciesnet dependencies in the base conda environment
# rocker/ml already has torch, torchvision, numpy, pandas, pillow, scikit-learn, opencv
# We add the remaining dependencies that speciesnet needs
RUN mamba install -y -c conda-forge \
    python=3.11 \
    humanfriendly \
    matplotlib \
    seaborn \
    pyyaml \
    tqdm \
    scipy \
    libmamba \
    && mamba clean -afy

# Configure reticulate to use the base conda environment
# This prevents creating a new environment and avoids duplicate torch installations
ENV RETICULATE_PYTHON=/opt/conda/bin/python
ENV RETICULATE_PYTHON_ENV=/opt/conda
ENV LD_LIBRARY_PATH=/opt/conda/lib:$LD_LIBRARY_PATH


# Install remaining pip-only dependencies
# Note: SpeciesNet is large (~2GB with dependencies)
RUN /opt/conda/bin/pip install --no-cache-dir \
    speciesnet

# Finally install the R package from local source (to verify changes)
COPY . /src
RUN R -e 'remotes::install_local("/src", upgrade="never")'

# The model weights (~214MB) will be downloaded on first use
# and cached in /home/${NB_USER}/.cache/kagglehub/

USER ${NB_USER}
