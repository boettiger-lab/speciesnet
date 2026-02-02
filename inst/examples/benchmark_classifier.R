
# benchmark_classifier.R
# Benchmarking speciesnet on a batch of local images from the Caltech Camera Traps dataset.

library(speciesnet)

script_start_time <- Sys.time()

# 1. Configuration & Path Detection
# Detect if running from project root or inside analysis directory (e.g. Docker)
if (dir.exists("/tmp/cct_images")) {
  image_dir <- "/tmp/cct_images"
  output_path <- "benchmark_results.csv"
} else {
  stop("Could not find 'cct_images' directory")
}

subset_size <- 1000

cli::cli_alert_info("Using image directory: {image_dir}")

# 2. List available images directly
files <- list.files(image_dir, pattern = "\\.jpg$", full.names = TRUE, recursive = TRUE)

if (length(files) == 0) {
  stop("No images found in ", image_dir)
}

# 2.5 Check GPU Availability
cli::cli_alert_info("Checking GPU availability...")
tryCatch({
  torch <- reticulate::import("torch")
  cuda_available <- torch$cuda$is_available()
  cli::cli_alert_info("CUDA Available: {cuda_available}")
  if (cuda_available) {
    cli::cli_alert_info("GPU Device: {torch$cuda$get_device_name(0L)}")
    cli::cli_alert_info("Device Count: {torch$cuda$device_count()}")
  } else {
    cli::cli_alert_warning("CUDA is NOT available. Running on CPU.")
  }
}, error = function(e) {
  cli::cli_alert_warning("Could not check CUDA status: {e$message}")
})

# 3. Take a subset
# Ensure we don't try to take more than we have
n <- min(subset_size, length(files))
batch_files <- files[1:n]

cli::cli_alert_info("Benchmarking on a batch of {n} images.")

# 4. Load SpeciesNet Model
# (This downloads model weights if not already cached)
model <- load_speciesnet()

# 5. Run Predictions in Batch
# Pass the entire vector of file paths to the model for optimized processing
start_time <- Sys.time()
predictions <- predict_species(model, batch_files)
end_time <- Sys.time()

duration <- end_time - start_time
cli::cli_alert_success("Batch prediction complete in {round(duration, 2)} {units(duration)}.")

# Calculate throughput
seconds <- as.numeric(duration, units = "secs")
throughput <- n / seconds
cli::cli_alert_info("Throughput: {round(throughput, 2)} images/sec")

# 6. Process Results
# Convert raw predictions list to a tidy data frame
cli::cli_alert_info("Parsing results...")
results_df <- predictions_to_df(predictions)

# 7. Save results
cli::cli_alert_success("Saving results to {output_path}...")
write.csv(results_df, output_path, row.names = FALSE)

cli::cli_alert_info("Sample Results:")
print(head(results_df))

# 8. Report Total Runtime
script_end_time <- Sys.time()
total_duration <- script_end_time - script_start_time
cli::cli_alert_success("Total script runtime: {round(total_duration, 2)} {units(total_duration)}")
