#' Download Caltech Camera Traps Images via Rclone
#'
#' @param n Number of images to download. Default 1000.
#' @param dest Destination directory. Default "/tmp/cct_images".
#' @param ignore_existing Logical. If TRUE, pass --ignore-existing to rclone. Default FALSE.
#' @return Invisible TRUE on success.
download_cct <- function(n = 1000, 
                         dest = "/tmp/cct_images", 
                         ignore_existing = FALSE) {
  
  # Check if rclone is available
  if (Sys.which("rclone") == "") {
    stop("rclone is not found in PATH. Please install rclone.")
  }
  
  source_path <- ":gcs:public-datasets-lila/caltech-unzipped/cct_images"
  file_list <- tempfile(pattern = "cct_files_", fileext = ".txt")
  
  # Ensure destination exists
  if (!dir.exists(dest)) {
    dir.create(dest, recursive = TRUE)
  }
  
  message(sprintf("Listing first %d files from %s...", n, source_path))
  
  # Step 1: List files and filter
  # rclone lsf <source> --recursive --files-only | head -n <n> > <file_list>
  list_cmd <- sprintf(
    "rclone lsf '%s' --gcs-anonymous --recursive --files-only | head -n %d > '%s'",
    source_path, n, file_list
  )
  
  res_list <- system(list_cmd)
  if (res_list != 0) {
    stop("Failed to list files using rclone.")
  }
  
  num_files <- length(readLines(file_list, warn = FALSE))
  message(sprintf("Downloading %d files to %s...", num_files, dest))
  
  # Step 2: Download files
  # Construct command with flags
  flags <- c(
    "--gcs-anonymous",
    "-P",
    "--transfers 256",
    "--checkers 512",
    "--buffer-size 128M",
    "--fast-list",
    "--use-mmap",
    "--s3-no-check-bucket",
    sprintf("--files-from '%s'", file_list)
  )
  
  if (ignore_existing) {
    flags <- c(flags, "--ignore-existing")
  }
  
  copy_cmd <- sprintf(
    "rclone copy '%s' '%s' %s",
    source_path, dest, paste(flags, collapse = " ")
  )
  
  res_copy <- system(copy_cmd)
  
  # Cleanup
  unlink(file_list)
  
  if (res_copy != 0) {
    stop("Failed to download files using rclone.")
  }
  
  invisible(TRUE)
}

# Example usage (if running script directly):
# download_cct(n = 10)
