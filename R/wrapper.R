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
#' @param batch_size Number of images to process in one batch (default: 50).
#' @param show_progress Whether to show a progress bar (default: TRUE).
#' @return A list containing predictions for each image.
#' @export
predict_species <- function(
  model,
  image_paths,
  country = NULL,
  admin1_region = NULL,
  latitude = NULL,
  longitude = NULL,
  batch_size = 50,
  show_progress = TRUE
) {
  if (is.null(model)) {
    cli::cli_abort(
      "Model is NULL. Please load a model first using load_speciesnet()."
    )
  }

  total_images <- length(image_paths)
  if (total_images == 0) {
    return(list(predictions = list()))
  }

  # Initialize progress bar
  if (show_progress) {
    cli::cli_progress_bar("Predicting species", total = total_images)
  }

  all_predictions <- list()

  # Process in batches
  num_batches <- ceiling(total_images / batch_size)

  for (b in seq_len(num_batches)) {
    start_idx <- (b - 1) * batch_size + 1
    end_idx <- min(b * batch_size, total_images)
    batch_indices <- start_idx:end_idx

    # Get batch paths
    batch_paths <- image_paths[batch_indices]

    # Prepare instances dict for this batch
    instances <- lapply(seq_along(batch_paths), function(i) {
      # Map global index for coordinate lookup
      global_idx <- batch_indices[i]
      path <- batch_paths[i]
      instance <- list(filepath = unname(path))

      if (!is.null(country)) {
        instance$country <- country
      }
      if (!is.null(admin1_region)) {
        instance$admin1_region <- admin1_region
      }

      # Handle optional coordinates
      # Logic: if vector has enough elements, use specific index, else recycle first element
      if (!is.null(latitude)) {
        if (length(latitude) >= global_idx) {
          instance$latitude <- latitude[global_idx]
        } else {
          instance$latitude <- latitude[1]
        }
      }
      if (!is.null(longitude)) {
        if (length(longitude) >= global_idx) {
          instance$longitude <- longitude[global_idx]
        } else {
          instance$longitude <- longitude[1]
        }
      }
      instance
    })

    # Convert to python dict format
    instances_dict <- list(instances = unname(instances))

    # Call predict method
    predictions <- tryCatch(
      {
        model$predict(instances_dict = instances_dict)
      },
      error = function(e) {
        cli::cli_abort("Prediction failed at batch {b}: {e$message}")
      }
    )

    batch_result <- reticulate::py_to_r(predictions)

    # Accumulate results
    # Assuming batch_result is list(predictions = list(...))
    if (!is.null(batch_result$predictions)) {
      all_predictions <- c(all_predictions, batch_result$predictions)
    }

    if (show_progress) {
      cli::cli_progress_update(length(batch_indices))
    }
  }

  if (show_progress) {
    cli::cli_progress_done()
    cli::cli_alert_success("Predictions complete.")
  }

  return(list(predictions = all_predictions))
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
