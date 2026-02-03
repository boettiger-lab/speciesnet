# Build the benchmark vignette
# This script should be run on a machine with a GPU to generate the pre-rendered vignette

# Ensure package is installed
library(speciesnet)

# Input/Output paths
input_file <- system.file("examples", "benchmark.Rmd", package = "speciesnet")
if (input_file == "") {
    # Fallback if running from source root without install
    input_file <- "inst/examples/benchmark.Rmd"
}

output_file <- "vignettes/benchmark.Rmd"

# Render the vignette
# We use knitr::knit to transform the Rmd (with execution) to markdown-ish Rmd (with output hardcoded)
# But wait, knitr::knit produces .md. We want .Rmd.
# And we want the YAML header preserved.

# The "classic trick" typically involves rendering to .md and renaming,
# ensuring the YAML is compatible.

if (!dir.exists("vignettes")) {
    dir.create("vignettes")
}

knitr::knit(
    input = input_file,
    output = output_file
)

message("Vignette built at ", output_file)
