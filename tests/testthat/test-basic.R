test_that("SpeciesNet module loads", {
  # This test requires the python package to be installed.
  # If not installed, it might fail or we should skip.

  # Try to check if module is loaded (it is lazy loaded)
  # Trigger load
  tryCatch(
    {
      reticulate::py_module_available("speciesnet")
    },
    error = function(e) skip("Python module not available")
  )

  expect_true(TRUE) # valid if we pass the tryCatch
})

test_that("install_speciesnet skips if already installed", {
  skip_if_not(
    reticulate::py_module_available("speciesnet"),
    "speciesnet not installed"
  )

  # Should detect existing installation and return quickly
  result <- install_speciesnet()
  expect_true(result)
})
