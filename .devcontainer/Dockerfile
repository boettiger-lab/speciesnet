# Use CUDA 13 devel image (required for NVRTC/Numba per RAPIDS docs)
FROM ghcr.io/rocker-org/cuda


# Install Python packages from requirements
COPY requirements.txt /tmp/requirements.txt
RUN pip install --no-cache-dir -r /tmp/requirements.txt

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

USER ${NB_USER}
RUN pip install --no-cache-dir speciesnet

RUN R -e "install.packages(c('devtools', 'remotes')); remotes::install_github('boettiger-lab/speciesnet')"
# The model weights (~214MB) will be downloaded on first use
# and cached in /home/${NB_USER}/.cache/kagglehub/

USER ${NB_USER}
