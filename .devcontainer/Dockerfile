# Use CUDA 13 devel image (required for NVRTC/Numba per RAPIDS docs)
FROM nvidia/cuda:13.0.0-devel-ubuntu24.04

ENV SHELL=/bin/bash
# Don't buffer Python stdout/stderr output
ENV PYTHONBUFFERED=1
# Don't prompt in apt commands
ENV DEBIAN_FRONTEND=noninteractive
# persistent install for vscode extensions
ENV CODE_EXTENSIONSDIR=/opt/share/code-server

# Set up JupyterLab user
ARG NB_USER=jovyan
ENV NB_USER=${NB_USER}
ENV NB_UID=1000
ENV USER="${NB_USER}"
ENV HOME="/home/${NB_USER}"

# Set up system-wide Python virtual environment
ENV VIRTUAL_ENV=/opt/venv
ENV PATH="$VIRTUAL_ENV/bin:$PATH"
ENV RETICULATE_PYTHON="$VIRTUAL_ENV/bin/python"
# Prevent conflict between host/system libraries and pip-installed torch wheels
ENV LD_LIBRARY_PATH="/usr/local/nvidia/lib:/usr/local/nvidia/lib64"

USER root

# nvidia/cuda base has ubuntu user/group at UID/GID 1000, rename to jovyan
RUN groupmod -n ${NB_USER} ubuntu && \
    usermod -l ${NB_USER} -d /home/${NB_USER} -m ubuntu

# Install Python 3.12 (default in Ubuntu 24.04) and system dependencies, then create venv
RUN apt-get update && apt-get install -y \
    python3 \
    python3-venv \
    python3-pip \
    git \
    curl \
    build-essential \
    && rm -rf /var/lib/apt/lists/* && \
    python3 -m venv $VIRTUAL_ENV && \
    chgrp -R $NB_USER $VIRTUAL_ENV && \
    chmod -R g+rwx $VIRTUAL_ENV

# Upgrade pip and install core packages
RUN pip install --no-cache-dir --upgrade pip setuptools wheel

USER ${NB_USER}
# Install Python packages from requirements
COPY requirements.txt /tmp/requirements.txt
RUN pip install --no-cache-dir -r /tmp/requirements.txt

USER root

# install vscode
RUN curl -fsSL https://code-server.dev/install.sh | sh && \
    rm -rf .cache /tmp/* /var/tmp/* 

# apt utilities, code-server setup
RUN curl -s https://raw.githubusercontent.com/rocker-org/ml/refs/heads/master/install_utilities.sh | bash

## Grant user sudoer privileges
RUN adduser "$NB_USER" sudo && echo '%sudo ALL=(ALL) NOPASSWD:ALL' >>/etc/sudoers

# Install R
RUN curl -s https://raw.githubusercontent.com/rocker-org/ml/refs/heads/master/install_r.sh | bash
RUN curl -s https://raw.githubusercontent.com/rocker-org/ml/refs/heads/master/Rprofile -o /usr/lib/R/etc/Rprofile.site

WORKDIR /home/$NB_USER 


# Install system dependencies required by OpenCV and SpeciesNet
# libgl1 is the vendor-neutral GL runtime (provides libGL.so.1) needed by opencv-python
# libglib2.0-0 is typically needed as well
USER root
RUN apt-get update && apt-get install -y \
    libgl1 \
    libglib2.0-0 \
    && rm -rf /var/lib/apt/lists/*

USER ${NB_USER}
RUN pip install --no-cache-dir speciesnet

RUN R -e "install.packages(c('devtools', 'remotes')); remotes::install_github('boettiger-lab/speciesnet')"
# The model weights (~214MB) will be downloaded on first use
# and cached in /home/${NB_USER}/.cache/kagglehub/

USER ${NB_USER}
