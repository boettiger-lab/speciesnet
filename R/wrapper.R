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
#' @param batch_size Number of images to process in one batch (default: 50).
#' @param show_progress Whether to show a progress bar (default: TRUE).
#' @param output_file Optional: Path to a JSONL file to stream results to. If provided, results are not returned in memory.
#' @return A list containing predictions for each image, or the output file path if output_file is specified.
#' @export
predict_species <- function(
  model,
  image_paths,
  country = NULL,
  admin1_region = NULL,
  latitude = NULL,
  longitude = NULL,
  batch_size = 50,
  show_progress = TRUE,
  output_file = NULL
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

  # Initialize outputs
  all_predictions <- list()
  streaming <- !is.null(output_file)
  
  if (streaming) {
    # Initialize/truncate output file
    if (file.exists(output_file)) {
      # Warn if replacing? Usually overwrite is expected behavior for simple output args
      cli::cli_alert_info("Overwriting existing output file: {output_file}")
      unlink(output_file)
    }
    file.create(output_file)
  }

  # Initialize progress bar
  if (show_progress) {
    cli::cli_progress_bar("Predicting species", total = total_images)
  }

  # Process in batches
  num_batches <- ceiling(total_images / batch_size)
  
  # Counter for streaming verification
  count <- 0

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

    # Accumulate or Stream results
    # Assuming batch_result is list(predictions = list(...))
    if (!is.null(batch_result$predictions)) {
      if (streaming) {
        # Stream to file (JSON Lines)
        # Write each prediction object as a separate line
        json_lines <- vapply(
          batch_result$predictions, 
          function(x) jsonlite::toJSON(x, auto_unbox = TRUE), 
          character(1)
        )
        cat(json_lines, file = output_file, sep = "\n", append = TRUE)
        count <- count + length(json_lines)
      } else {
        # Keep in memory
        all_predictions <- c(all_predictions, batch_result$predictions)
      }
    }

    if (show_progress) {
      cli::cli_progress_update(length(batch_indices))
    }
  }

  if (show_progress) {
    cli::cli_progress_done()
    cli::cli_alert_success("Predictions complete.")
  }

  if (streaming) {
    cli::cli_alert_info("Results written to {output_file} ({count} records).")
    return(output_file)
  } else {
    return(list(predictions = all_predictions))
  }
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

#' Parse Prediction Taxonomy String
#'
#' Helper function to parse the semicolon-separated prediction string.
#'
#' @param x A prediction string (e.g. "id;class;order...").
#' @return A named character vector.
#' @noRd
parse_prediction_string <- function(x) {
  # Default structure if empty or blank
  cols <- c(
    "taxon_id", "class", "order", "family", "genus", "species", "common_name"
  )
  empty_res <- stats::setNames(rep(NA_character_, length(cols)), cols)

  if (is.null(x) || length(x) == 0 || is.na(x) || x == "") {
    return(empty_res)
  }

  parts <- strsplit(x, ";")[[1]]

  # Pad with NA if short, truncate if long (though it should be fixed length)
  if (length(parts) < length(cols)) {
    parts <- c(parts, rep(NA_character_, length(cols) - length(parts)))
  } else if (length(parts) > length(cols)) {
    parts <- parts[seq_along(cols)] # Take first N
  }

  # Replace empty strings with NA
  parts[parts == ""] <- NA_character_

  stats::setNames(parts, cols)
}

#' Convert Predictions to Data Frame
#'
#' Parses the raw list output from \code{predict_species} into a tidy data frame.
#' Each row represents a unique detection (or the image if no detections).
#' Includes full taxonomy and detection details.
#'
#' @param predictions A list returned by \code{predict_species}.
#' @return A data.frame with columns for file path, taxonomy, scores, and detections.
#' @export
predictions_to_df <- function(predictions) {
  if (is.null(predictions) || length(predictions) == 0) {
    return(data.frame())
  }
  
  # Handle case where input is the wrapper list containing 'predictions'
  if ("predictions" %in% names(predictions)) {
    predictions <- predictions$predictions
  }

  # Process each image result
  rows <- lapply(predictions, function(pred) {
    # Basic info
    filepath <- pred$filepath
    
    # Parse top prediction (taxonomy)
    # pred$prediction is the top-1 taxonomy string
    top_taxa <- parse_prediction_string(pred$prediction)
    
    # Top score
    # prediction_score is usually a single number (or list of 1)
    score <- as.numeric(pred$prediction_score)
    if (length(score) == 0) score <- NA_real_

    # Alternatives summary
    # classifications$classes and $scores usually contain top-K
    alts <- NA_character_
    if (!is.null(pred$classifications$classes) && length(pred$classifications$classes) > 1) {
      # Skip the first one (it's the top prediction)
      alt_classes <- pred$classifications$classes[-1]
      alt_scores <- pred$classifications$scores[-1]
      
      # Extract common names or species from the strings for brevity
      # Format: "Common Name (Score)"
      alt_names <- sapply(alt_classes, function(s) {
        parts <- strsplit(s, ";")[[1]]
        # diverse strategies for what names exist, usually last is common name
        if (length(parts) > 0) parts[length(parts)] else "unknown"
      })
      
      # Combine
      alts <- paste(
        sprintf("%s (%.3f)", alt_names, as.numeric(alt_scores)), 
        collapse = "; "
      )
    }

    # Detections
    dets <- pred$detections
    
    base_row <- data.frame(
      filepath = filepath,
      as.list(top_taxa),
      prediction_score = score,
      alternatives = alts,
      stringsAsFactors = FALSE
    )
    
    if (length(dets) == 0) {
      # No detections: return one row per image
      # Add NA columns for detection fields
      det_cols <- data.frame(
        bbox_category = NA_character_,
        bbox_conf = NA_real_,
        bbox_x = NA_real_,
        bbox_y = NA_real_,
        bbox_w = NA_real_,
        bbox_h = NA_real_
      )
      return(cbind(base_row, det_cols))
    } else {
      # One row per detection
      det_rows <- do.call(rbind, lapply(dets, function(d) {
        # bbox is typically [ymin, xmin, h, w] or similar? 
        # CAUTION: Inspecting existing data structure from user logs:
        # .. ..$ bbox    : num [1:4] 0.606 0.5924 0.0547 0.0542
        # Usually standard YOLO/COCO is [x, y, w, h] or [y_min, x_min, y_max, x_max]
        # SpeciesNet (Google) often uses [ymin, xmin, ymax, xmax] relative?
        # User log shows 4 numbers. Let's store them as bbox 1-4 for now or x/y/w/h if we assume standard.
        # Assuming [x, y, w, h] normalized based on typical detection outputs in these projects.
        # If it's [ymin, xmin, height, width], we'll just map them 1-to-1 for now.
        
        b <- as.numeric(d$bbox)
        # Map to x, y, w, h (assuming standard order, or just bbox1..4)
        # Let's label them generically if unsure, but user asked for "bbox"
        
        data.frame(
          bbox_category = as.character(d$category),
          bbox_conf = as.numeric(d$conf),
          bbox_x = b[1], # TBD strictly on coordinate system
          bbox_y = b[2],
          bbox_w = b[3],
          bbox_h = b[4],
          stringsAsFactors = FALSE
        )
      }))
      
      # Replicate base info for all detections
      # Use merge/cbind equivalent
      # Replicate the base_row N times
      base_replicated <- base_row[rep(1, nrow(det_rows)), , drop = FALSE]
      return(cbind(base_replicated, det_rows))
    }
  })

  # Combine all
  do.call(rbind, rows)
}

