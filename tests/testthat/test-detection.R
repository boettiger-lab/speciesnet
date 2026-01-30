test_that("SpeciesNet loads and predicts species", {
  skip_if_not_installed("reticulate")

  # Ensure python dependency is present
  if (!reticulate::py_module_available("speciesnet")) {
    skip("SpeciesNet python package not available")
  }

  # Load model
  model <- load_speciesnet()
  expect_true(!is.null(model))

  # Use bundled image
  img_path <- system.file("img", "Caltech_Animal.jpg", package = "speciesnet")

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

test_that("SpeciesNet handles multiple images and named vectors", {
  skip_if_not_installed("reticulate")

  if (!reticulate::py_module_available("speciesnet")) {
    skip("SpeciesNet python package not available")
  }

  model <- load_speciesnet()

  # Test with multiple images and names (reproduction of the fix)
  image_files <- c(
    animal = system.file("img", "Caltech_Animal.jpg", package = "speciesnet"),
    vehicle = system.file("img", "Caltech_Vehicle.jpg", package = "speciesnet")
  )

  # Filter out non-existent images
  image_files <- image_files[file.exists(image_files) & image_files != ""]

  if (length(image_files) == 0) {
    skip("Test images not available")
  }

  # Test named vector (this previously threw TypeError)
  predictions <- predict_species(model, image_files)
  expect_true(is.list(predictions))
  expect_equal(length(predictions$predictions), length(image_files))

  # Test unnamed version
  predictions_unnamed <- predict_species(model, unname(image_files))
  expect_equal(length(predictions_unnamed$predictions), length(image_files))
})

test_that("SpeciesNet handles coordinates and geofencing parameters", {
  skip_if_not_installed("reticulate")

  if (!reticulate::py_module_available("speciesnet")) {
    skip("SpeciesNet python package not available")
  }

  model <- load_speciesnet()
  img_path <- system.file("img", "Caltech_Animal.jpg", package = "speciesnet")

  if (!file.exists(img_path) || img_path == "") {
    skip("Test images not available")
  }

  # Test with single coordinates
  predictions_single <- predict_species(
    model,
    img_path,
    latitude = 34.147,
    longitude = -118.144,
    country = "USA"
  )
  expect_true(is.list(predictions_single))

  # Test with vector coordinates (multiple images)
  imgs <- rep(img_path, 2)
  predictions_vector <- predict_species(
    model,
    imgs,
    latitude = c(34.1, 35.1),
    longitude = c(-118.1, -119.1)
  )
  expect_equal(length(predictions_vector$predictions), 2)
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
