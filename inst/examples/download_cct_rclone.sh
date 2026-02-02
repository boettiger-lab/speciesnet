#!/bin/bash
# Optimized rclone script for downloading Caltech Camera Traps images from GCS
# Saturates high-bandwidth networks for small file transfers

rclone copy :gcs:public-datasets-lila/caltech-unzipped/cct_images ./cct_images \
    --gcs-anonymous \
    -P \
    --transfers 256 \
    --checkers 512 \
    --buffer-size 128M \
    --fast-list \
    --use-mmap \
    --s3-no-check-bucket \
    --ignore-existing
