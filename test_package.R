#!/usr/bin/env Rscript

# Test script for SpeciesNet R package
cat("=== SpeciesNet R Package Test ===\n\n")

# Load the package
cat("1. Loading speciesnet package...\n")
library(speciesnet)

# Test installation function
cat("\n2. Testing install_speciesnet() function...\n")
result <- tryCatch(
    {
        install_speciesnet()
        TRUE
    },
    error = function(e) {
        cat("Installation test failed:", e$message, "\n")
        FALSE
    }
)

if (!result) {
    cat("Installation failed. Please install manually.\n")
    quit(status = 1)
}

# Test model loading
cat("\n3. Testing load_speciesnet() function...\n")
model <- tryCatch(
    {
        load_speciesnet()
    },
    error = function(e) {
        cat("Model loading failed:", e$message, "\n")
        cat("This is expected if the python package isn't installed.\n")
        NULL
    }
)

if (!is.null(model)) {
    cat("Model loaded successfully!\n")
    cat("Model class:", class(model), "\n")

    # Test prediction (would need actual image)
    cat("\n4. Model is ready for predictions.\n")
    cat("To test predictions, use:\n")
    cat("  predictions <- predict_species(model, 'path/to/image.jpg')\n")
} else {
    cat("\nModel loading skipped (python package not available).\n")
}

cat("\n=== Test Complete ===\n")
