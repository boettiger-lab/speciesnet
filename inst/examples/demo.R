# SpeciesNet Demo Script

# 1. Load Package
if (!require("speciesnet")) {
  devtools::load_all()
}

# 2. Install Python Dependencies
# Only needed if not already installed. Downloads ~2GB of dependencies.
# install_speciesnet()

# 3. Load Model
# Automatically downloads default model weights on first use
model <- load_speciesnet()

# 4. Predict Species
# Images are bundled with the package
image_files <- c(
  "Caltech_Animal.jpg",
  "Caltech_Vehicle.jpg",
  "Caltech_Empty.jpg"
)
images <- sapply(image_files, function(f) {
  system.file("img", f, package = "speciesnet")
})

cli::cli_h1("Running SpeciesNet Predictions")

# Run batch prediction
results <- predict_species(model, images)

for (i in seq_along(images)) {
  name <- image_files[i]
  # Results are returned in order
  pred <- results$predictions[[i]]

  top_species <- get_top_species(pred)
  detections <- get_detections(pred)

  cli::cli_alert_info("Image: {.file {name}}")
  cli::cli_alert_info("Top Prediction: {.strong {top_species}}")
  cli::cli_alert_info("Detections: {.strong {length(detections)}}")
  cli::cli_text("")
}

cli::cli_alert_success("Demo Complete!")
