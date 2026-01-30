
# MegaDetector Demo Script

# 1. Install & Load
# installs automatically if needed
if (!require("megadetector")) devtools::load_all() # Or library(megadetector) if installed
install_megadetector() 

# 2. Load Model
# Automatically downloads default model (MDV5A) to cache if missing
model <- load_model()

# 3. Detect & Classify
# Images are bundled with the package
image_files <- c("Caltech_Animal.jpg", "Caltech_Vehicle.jpg", "Caltech_Empty.jpg")
images <- sapply(image_files, function(f) system.file("img", f, package = "megadetector"))

cli::cli_h1("Running Detections")

for (i in seq_along(images)) {
  name <- names(images)[i]
  img_path <- images[[i]]
  
  detections <- detect_image(model, img_path)
  class_label <- get_classification(detections)
  
  cli::cli_alert_info("Image: {.file {name}} | Class: {.strong {class_label}}")
  
  if (length(detections) > 0) {
    cli::cli_ul("Top Confidence: {detections[[1]]$conf}")
  }
}

cli::cli_alert_success("Demo Complete!")
