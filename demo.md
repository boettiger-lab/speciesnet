# SpeciesNet R Package - Test Demo

This demo script tests the SpeciesNet R wrapper.

## Installation

First, install the python package:

```r
library(speciesnet)
install_speciesnet()
```

## Load Model

Load the SpeciesNet ensemble model:

```r
model <- load_speciesnet()
```

## Predict on Images

Download a sample image for testing:

```r
# Create a temp directory for test images
test_dir <- tempdir()

# Download a sample image (you'll need to provide your own test images)
# For now, we'll create a placeholder
test_image <- file.path(test_dir, "test_image.jpg")

# In practice, you would download or use your own camera trap images
# For testing, you can use images from the cameratrapai repo or your own data
```

Run prediction:

```r
# Example: predict on a single image
predictions <- predict_species(model, test_image)

# View results
print(predictions)

# Get top species
if (length(predictions$predictions) > 0) {
  top_species <- get_top_species(predictions$predictions[[1]])
  print(paste("Top species:", top_species))
  
  # Get detections
  detections <- get_detections(predictions$predictions[[1]])
  print(paste("Number of detections:", length(detections)))
}
```

## Options

You can also specify geographic location for geofencing:

```r
# Predict with country code
predictions <- predict_species(
  model, 
  test_image, 
  country = "USA",
  admin1_region = "CA"
)

# Or with lat/lon
predictions <- predict_species(
  model, 
  test_image, 
  latitude = 38.5,
  longitude = -120.5
)
```

## Multiple Images

Process multiple images at once:

```r
image_paths <- c(
  "path/to/image1.jpg",
  "path/to/image2.jpg",
  "path/to/image3.jpg"
)

predictions <- predict_species(model, image_paths)

# Process results
for (i in seq_along(predictions$predictions)) {
  pred <- predictions$predictions[[i]]
  top_sp <- get_top_species(pred)
  cat(sprintf("Image %d: %s\n", i, top_sp))
}
```
