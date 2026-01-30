#' Install SpeciesNet Python Package
#'
#' Installs the speciesnet python package and its dependencies.
#'
#' @param method Installation method ("auto", "virtualenv", "conda").
#' @param conda Path to conda executable (or "auto").
#' @param version Version of the package to install.
#' @param pip If TRUE, use pip for installation.
#' @param force If TRUE, force reinstallation even if already present.
#' @param ... Additional arguments passed to \code{reticulate::py_install}.
#'
#' @export
install_speciesnet <- function(
  method = "auto",
  conda = "auto",
  version = "default",
  pip = TRUE,
  force = FALSE,
  ...
) {
  # Smart check: verify both module availability AND that it can actually be imported
  if (!force) {
    is_available <- tryCatch(
      {
        reticulate::py_module_available("speciesnet") &&
          !is.null(reticulate::import(
            "speciesnet",
            delay_load = TRUE
          ))
      },
      error = function(e) FALSE
    )

    if (is_available) {
      cli::cli_alert_success(
        "SpeciesNet python package is already installed and working."
      )
      return(invisible(TRUE))
    }
  }

  cli::cli_progress_step("Installing SpeciesNet python package...")

  # Detect Python environment type to guide installation strategy
  py_config <- reticulate::py_config()
  using_conda <- grepl("conda|mamba", py_config$python, ignore.case = TRUE)
  using_virtualenv <- grepl(
    "virtualenv|venv",
    py_config$python,
    ignore.case = TRUE
  )

  # Check if we already have torch installed (e.g., in rocker/ml, base conda env)
  has_torch <- reticulate::py_module_available("torch")

  if (has_torch) {
    cli::cli_alert_info(
      "PyTorch detected in current environment ({py_config$python})."
    )
  }

  # Core dependencies that are best installed via conda if possible
  conda_deps <- c(
    "pandas",
    "requests",
    "matplotlib",
    "seaborn",
    "scikit-learn",
    "numpy",
    "pillow",
    "pyyaml",
    "tqdm",
    "humanfriendly"
  )

  # If we are using conda, try to install these first
  # This helps leverage pre-compiled binaries and avoid pip compiling from source
  # But skip if dependencies already exist (e.g., in rocker/ml base environment)
  if (
    (method == "conda" || (method == "auto" && using_conda)) &&
      !is.null(tryCatch(reticulate::conda_binary(), error = function(e) NULL))
  ) {
    # Check which dependencies are already available to avoid reinstalling
    missing_deps <- conda_deps[
      !sapply(conda_deps, reticulate::py_module_available)
    ]

    if (length(missing_deps) > 0) {
      cli::cli_alert_info(
        "Installing {length(missing_deps)} conda dependencies..."
      )

      tryCatch(
        {
          reticulate::conda_install(
            packages = missing_deps,
            conda = conda,
            pip = FALSE,
            ...
          )
        },
        error = function(e) {
          cli::cli_alert_warning(
            "Failed to install some conda dependencies: {e$message}"
          )
        }
      )
    } else {
      cli::cli_alert_success(
        "All conda dependencies already available in environment."
      )
    }
  }

  package <- "speciesnet"
  if (version != "default") {
    package <- paste0("speciesnet==", version)
  }

  # Smart pip options based on environment
  # Use --no-deps if torch/torchvision already exist to avoid reinstalling heavy deps
  pip_options <- if (has_torch) {
    cli::cli_alert_info(
      "Installing speciesnet with --no-deps to avoid duplicating PyTorch dependencies."
    )
    "--no-deps"
  } else {
    # Let pip handle dependencies if we don't have torch yet
    cli::cli_alert_info(
      "Installing speciesnet with full dependencies (PyTorch not detected)."
    )
    NULL
  }

  # Check if we're in an ephemeral environment (reticulate-managed)
  # If so, use py_require() instead of py_install() to avoid warnings
  in_ephemeral_env <- isTRUE(py_config$ephemeral)

  if (in_ephemeral_env) {
    # Ephemeral environments: use py_require which handles them properly
    tryCatch(
      {
        # py_require installs if needed and imports the module
        # This is the proper way to handle ephemeral environments
        reticulate::py_require(package)
      },
      error = function(e) {
        cli::cli_alert_danger("Installation failed: {e$message}")
        cli::cli_alert_info(
          "Try installing manually: reticulate::py_install('speciesnet')"
        )
        stop(e)
      }
    )
  } else {
    # Use standard py_install for non-ephemeral environments
    # This works with conda, virtualenv, venv, and uv environments
    tryCatch(
      {
        reticulate::py_install(
          package,
          method = method,
          conda = conda,
          pip = pip,
          pip_options = pip_options,
          ...
        )
      },
      error = function(e) {
        cli::cli_alert_danger("Installation failed: {e$message}")
        cli::cli_alert_info(
          "Try installing manually: reticulate::py_install('speciesnet')"
        )
        stop(e)
      }
    )
  }

  # Verify installation with a more robust check
  installation_ok <- tryCatch(
    {
      reticulate::py_module_available("speciesnet") &&
        !is.null(reticulate::import(
          "speciesnet",
          delay_load = TRUE
        ))
    },
    error = function(e) {
      cli::cli_alert_warning("Verification error: {e$message}")
      FALSE
    }
  )

  if (installation_ok) {
    cli::cli_progress_done()
    cli::cli_alert_success("SpeciesNet installed successfully!")
    invisible(TRUE)
  } else {
    cli::cli_progress_done()
    cli::cli_alert_danger(
      "Installation completed but verification failed. Please check your python environment."
    )
    cli::cli_alert_info(
      "Python: {py_config$python}"
    )
    invisible(FALSE)
  }
}
