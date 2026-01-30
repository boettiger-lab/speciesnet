# Declare global variables to avoid R CMD check notes
utils::globalVariables("sn")

#' Load SpeciesNet Model
#'
#' Loads the SpeciesNet model which provides an interface for running inference.
#'
#' @param model Model identifier (default: uses SpeciesNet default model v4.0.2a).
#' @param components Which components to load: "all" (default), "classifier", "detector", or "ensemble".
#' @param geofence Whether to enable geofencing (default: TRUE).
#' @return A loaded SpeciesNet object (Python object).
#' @export
load_speciesnet <- function(
  model = NULL,
  components = "all",
  geofence = TRUE
) {
  if (is.null(sn)) {
    cli::cli_abort(
      "SpeciesNet module not loaded. Try running install_speciesnet() first."
    )
  }

  cli::cli_alert_info("Loading SpeciesNet model...")

  # Import the SpeciesNet class
  speciesnet_class <- tryCatch(
    {
      sn$SpeciesNet
    },
    error = function(e) {
      cli::cli_abort(
        "Could not import SpeciesNet: {e$message}"
      )
    }
  )

  # Use default model if not specified
  if (is.null(model)) {
    # Get the default model from the module
    model <- sn$DEFAULT_MODEL
  }

  # Create SpeciesNet instance
  model_obj <- speciesnet_class(
    model_name = model,
    components = components,
    geofence = geofence
  )

  cli::cli_alert_success("SpeciesNet model loaded successfully.")
  return(model_obj)
}

#' Predict Species in Image(s)
#'
#' Runs species prediction on one or more images.
#'
#' @param model A loaded SpeciesNet model object from \code{load_speciesnet}.
#' @param image_paths Character vector of image file paths.
#' @param country Optional: 3-letter ISO country code for geofencing.
#' @param admin1_region Optional: First-level administrative division (e.g., US state code).
#' @param latitude Optional: Latitude where images were taken.
#' @param longitude Optional: Longitude where images were taken.
#' @return A list containing predictions for each image.
#' @export
predict_species <- function(
  model,
  image_paths,
  country = NULL,
  admin1_region = NULL,
  latitude = NULL,
  longitude = NULL
) {
  if (is.null(model)) {
    cli::cli_abort(
      "Model is NULL. Please load a model first using load_speciesnet()."
    )
  }

  # Prepare instances dict
  instances <- lapply(image_paths, function(path) {
    instance <- list(filepath = path)
    if (!is.null(country)) {
      instance$country <- country
    }
    if (!is.null(admin1_region)) {
      instance$admin1_region <- admin1_region
    }
    if (!is.null(latitude)) {
      instance$latitude <- latitude
    }
    if (!is.null(longitude)) {
      instance$longitude <- longitude
    }
    instance
  })

  # Convert to python dict format
  instances_dict <- list(instances = instances)

  cli::cli_alert_info(
    "Running predictions on {length(image_paths)} image(s)..."
  )

  # Call predict method
  predictions <- tryCatch(
    {
      model$predict(instances_dict = instances_dict)
    },
    error = function(e) {
      cli::cli_abort("Prediction failed: {e$message}")
    }
  )

  cli::cli_alert_success("Predictions complete.")

  # Convert to R object
  return(reticulate::py_to_r(predictions))
}

#' Get Top Classification from Prediction
#'
#' Extracts the top predicted species from a prediction result.
#'
#' @param prediction A single prediction result from \code{predict_species}.
#' @return A character string with the top predicted class, or "blank" if no predictions.
#' @export
get_top_species <- function(prediction) {
  # Check if there are classifications
  if (is.null(prediction$classifications)) {
    return("blank")
  }

  if (
    is.null(prediction$classifications$classes) ||
      length(prediction$classifications$classes) == 0
  ) {
    return("blank")
  }

  # Return the first (top) class
  return(prediction$classifications$classes[[1]])
}

#' Get Detections from Prediction
#'
#' Extracts detection information from a prediction result.
#'
#' @param prediction A single prediction result from \code{predict_species}.
#' @return A list of detections with bounding boxes and confidence scores.
#' @export
get_detections <- function(prediction) {
  if (is.null(prediction$detections)) {
    return(list())
  }

  return(prediction$detections)
}
