test_that("SpeciesNet loads and predicts species", {
  skip_if_not_installed("reticulate")

  # Ensure python dependency is present (install in temp path if feasible, or skip)
  if (!reticulate::py_module_available("speciesnet")) {
    skip("SpeciesNet python package not available")
  }

  # Load model
  model <- load_speciesnet()
  expect_true(!is.null(model))

  # Use bundled image (we'll need to check if these exist or create new ones)
  img_path <- system.file("img", "Caltech_Animal.jpg", package = "speciesnet")

  # Skip if image doesn't exist (we may need to update inst/img later)
  if (!file.exists(img_path) || img_path == "") {
    skip("Test images not available")
  }

  # Run prediction
  predictions <- predict_species(model, img_path)
  expect_true(is.list(predictions))
  expect_true(length(predictions$predictions) > 0)

  # Check top species
  top_species <- get_top_species(predictions$predictions[[1]])
  expect_true(is.character(top_species))
  expect_true(nchar(top_species) > 0)
})

test_that("SpeciesNet handles multiple images", {
  skip_if_not_installed("reticulate")

  if (!reticulate::py_module_available("speciesnet")) {
    skip("SpeciesNet python package not available")
  }

  model <- load_speciesnet()

  # Test with multiple images (if available)
  image_files <- c(
    system.file("img", "Caltech_Animal.jpg", package = "speciesnet"),
    system.file("img", "Caltech_Vehicle.jpg", package = "speciesnet"),
    system.file("img", "Caltech_Empty.jpg", package = "speciesnet")
  )

  # Filter out non-existent images
  image_files <- image_files[file.exists(image_files) & image_files != ""]

  if (length(image_files) == 0) {
    skip("Test images not available")
  }

  predictions <- predict_species(model, image_files)
  expect_true(is.list(predictions))
  expect_equal(length(predictions$predictions), length(image_files))
})

test_that("get_top_species handles empty predictions", {
  # Test with blank prediction
  blank_pred <- list(classifications = NULL)
  result <- get_top_species(blank_pred)
  expect_equal(result, "blank")

  # Test with empty classes
  empty_pred <- list(classifications = list(classes = list()))
  result <- get_top_species(empty_pred)
  expect_equal(result, "blank")
})

test_that("get_detections extracts detection info", {
  # Mock detection result
  mock_pred <- list(
    detections = list(
      list(
        category = "1",
        label = "animal",
        conf = 0.95,
        bbox = c(0.1, 0.2, 0.3, 0.4)
      )
    )
  )

  detections <- get_detections(mock_pred)
  expect_true(is.list(detections))
  expect_equal(length(detections), 1)

  # Test empty detections
  empty_pred <- list(detections = NULL)
  detections <- get_detections(empty_pred)
  expect_equal(length(detections), 0)
})
