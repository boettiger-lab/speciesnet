
test_that("predictions_to_df parses predictions correctly", {
  
  # Mock prediction object
  # Based on user provided structure
  mock_preds <- list(
    list(
      filepath = "img1.jpg",
      prediction = "id1;mammalia;carnivora;felidae;panthera;leo;lion",
      prediction_score = 0.99,
      classifications = list(
        classes = c(
            "id1;mammalia;carnivora;felidae;panthera;leo;lion",
            "id2;mammalia;carnivora;felidae;panthera;pardus;leopard"
        ),
        scores = c(0.99, 0.01)
      ),
      detections = list(
        list(category = "1", label = "animal", conf = 0.95, bbox = c(0.1, 0.1, 0.2, 0.2)),
        list(category = "1", label = "animal", conf = 0.90, bbox = c(0.5, 0.5, 0.3, 0.3))
      )
    ),
    list(
      filepath = "img2.jpg",
      prediction = "id3;;;;;;blank", # Empty/blank
      prediction_score = 0.8,
      classifications = list(classes = c("id3;;;;;;blank"), scores = c(0.8)),
      detections = list() # Empty detections
    )
  )
  
  df <- predictions_to_df(list(predictions = mock_preds))
  
  # Checks
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 3) # 2 detections for img1, 1 row for img2
  
  # Check columns
  expect_true("common_name" %in% names(df))
  expect_true("bbox_conf" %in% names(df))
  expect_true("alternatives" %in% names(df))
  
  # Check contents
  # Img 1 (2 rows) - should have flattened detections
  img1_rows <- df[df$filepath == "img1.jpg", ]
  expect_equal(nrow(img1_rows), 2)
  expect_equal(img1_rows$common_name[1], "lion")
  expect_equal(img1_rows$bbox_conf[1], 0.95)
  expect_equal(img1_rows$bbox_conf[2], 0.90)
  
  # Check alternatives
  expect_true(grepl("leopard", img1_rows$alternatives[1]))
  
  # Img 2 (1 row) - should have NA detections
  img2_row <- df[df$filepath == "img2.jpg", ]
  expect_equal(nrow(img2_row), 1)
  expect_equal(img2_row$common_name, "blank")
  expect_true(is.na(img2_row$bbox_conf))
})
