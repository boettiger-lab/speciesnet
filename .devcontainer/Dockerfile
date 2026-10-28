FROM ghcr.io/rocker-org/cuda

# rocker/cuda ships matched stable torch/torchvision/torchaudio for CUDA 13 in
# /opt/venv (RETICULATE_PYTHON already points there), so speciesnet installs
# on top of them without touching torch.

# OpenCV (pulled in by speciesnet) needs libGL.so.1 and glib; rocker/cuda
# does not set a locale, so generate en_US.UTF-8 for devcontainer use.
USER root
RUN apt-get update && apt-get install -y \
    libgl1 \
    libglib2.0-0 \
    locales \
    && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
    && locale-gen \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8

# /opt/venv is owned by ${NB_USER}; install as that user so pip leaves no
# root-owned files in it.
USER ${NB_USER}
RUN pip install --no-cache-dir speciesnet

# Install the R package from this checkout (build context is the repo root),
# so the image matches the commit it was built from.
COPY --chown=${NB_USER}:${NB_USER} . /tmp/speciesnet
RUN R -e "remotes::install_local('/tmp/speciesnet', dependencies = TRUE, upgrade = 'never')" \
    && rm -rf /tmp/speciesnet

# Build-time smoke test: fails the build if speciesnet cannot import against
# the base image's torch. The model weights (~214MB) are downloaded on first
# use and cached in ~/.cache/kagglehub/.
RUN R -e "speciesnet::install_speciesnet(); speciesnet::load_speciesnet()"
