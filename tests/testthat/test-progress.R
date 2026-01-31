
test_that("predict_species processes in batches and shows progress", {
  # Mock model object
  # We need a way to verify that it was called multiple times.
  # Using a closure to track calls
  
  call_count <- 0
  
  mock_model <- list(
    predict = function(instances_dict) {
      call_count <<- call_count + 1
      # Return dummy results equal to number of inputs
      n <- length(instances_dict$instances)
      results <- lapply(1:n, function(i) list(classifications = list(classes = list("mock_species"))))
      list(predictions = results) 
    }
  )

  # Mock data
  image_paths <- paste0("img_", 1:22) # 22 images
  
  # Run prediction with batch size 5
  # Expected batches: 5, 5, 5, 5, 2 => 5 calls
  batch_size <- 5
  
  # Use show_progress = FALSE to keep test output clean
  res <- predict_species(mock_model, image_paths, batch_size = batch_size, show_progress = FALSE)
  
  expect_equal(length(res$predictions), 22)
  expect_equal(res$predictions[[1]]$classifications$classes[[1]], "mock_species")
  expect_equal(call_count, 5) # 22 / 5 = 4 full batches + 1 partial
  
})

test_that("predict_species correctly handles metadata across batches", {
  
  last_received_instances <- list()
  
  mock_model <- list(
    predict = function(instances_dict) {
      # Store received instances for verification
      last_received_instances <<- c(last_received_instances, instances_dict$instances)
      
      n <- length(instances_dict$instances)
      results <- lapply(1:n, function(i) list(classifications = list(classes = list("mock_species"))))
      list(predictions = results) 
    }
  )
  
  image_paths <- paste0("img_", 1:10)
  lats <- 1:10
  lons <- 1:10
  
  # Batch size 3 -> batches of 3, 3, 3, 1
  predict_species(mock_model, image_paths, latitude = lats, longitude = lons, batch_size = 3, show_progress = FALSE)
  
  # Verify we reconstructed the order correctly
  expect_equal(length(last_received_instances), 10)
  
  # Check 1st item
  expect_equal(last_received_instances[[1]]$latitude, 1)
  
  # Check 4th item (start of 2nd batch)
  expect_equal(last_received_instances[[4]]$latitude, 4)
  
  # Check 10th item
  expect_equal(last_received_instances[[10]]$latitude, 10)
})
