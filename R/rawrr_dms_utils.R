#' Preview available trailer fields from a raw file
#'
#' @param raw_path Path to a raw file (either folder path or full .raw path)
#'
#' @return Character vector of available trailer field names
#' @export
#'
#' @examples
#' \dontrun{
#' preview_trailer_fields("\\\\proto-6\\AstralZ02\\2025_3\\dataset_name")
#' }
preview_trailer_fields <- function(raw_path) {
  
  
  # Resolve to full .raw path
  
  full_path <- resolve_raw_path(raw_path)
  
  if (!file.exists(full_path)) {
    cli::cli_abort("File does not exist: {full_path}")
  }
  
  rawrr::readTrailer(full_path)
}


#' Extract scan metadata from raw files
#'
#' @param paths Character vector of paths (folder paths or full .raw paths)
#' @param trailer_fields Character vector of trailer field names to extract,
#'   "all" to extract all available fields, or NULL (default) for no trailers.
#' @param parallel Logical, whether to use parallel processing. Default FALSE.
#' @param n_workers Number of parallel workers. Default NULL uses
#'   `future::availableCores() - 1`.
#'
#' @return A tibble with scan metadata, one row per scan, datasets stacked.
#'   First column is `dataset` (raw file name without .raw extension).
#'   Base columns from readIndex: scan, scan_type, start_time, precursor_mass,
#'   ms_order, charge, master_scan, dependency_type.
#'   Additional columns for any requested trailer fields.
#'
#' @importFrom future plan availableCores multisession
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' # Just readIndex data
#' extract_scan_metadata(dms$dataset_folder_path)
#'
#' # With specific trailer fields
#' extract_scan_metadata(dms$dataset_folder_path,
#'                       trailer_fields = c("Number of Peaks:", "Ion Injection Time (ms):"))
#'
#' # Extract all available trailer fields
#' extract_scan_metadata(dms$dataset_folder_path, trailer_fields = "all")
#'
#' # Parallel processing
#' extract_scan_metadata(dms$dataset_folder_path,
#'                       trailer_fields = "all",
#'                       parallel = TRUE,
#'                       n_workers = 8)
#' }
extract_scan_metadata <- function(paths, trailer_fields = NULL,
                                  parallel = FALSE, n_workers = NULL) {
  
  if (!requireNamespace("rawrr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg rawrr} is required.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg dplyr} is required.")
  }
  if (!requireNamespace("janitor", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg janitor} is required.")
  }
  if (parallel && !requireNamespace("furrr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg furrr} is required for parallel processing.")
  }
  
  # Resolve all paths
  full_paths <- vapply(paths, resolve_raw_path, character(1), USE.NAMES = FALSE)
  
  # Check which files exist
  exists_check <- file.exists(full_paths)
  if (!any(exists_check)) {
    cli::cli_abort("None of the provided paths point to existing files.")
  }
  
  # Warn about missing files upfront
  
  n_missing <- sum(!exists_check)
  if (n_missing > 0) {
    cli::cli_warn("{n_missing} file{?s} not found and will be skipped.")
  }
  
  # Inform about trailer fields if NULL
  if (is.null(trailer_fields)) {
    cli::cli_inform(c(
      "i" = "No trailer fields requested.",
      "*" = "Run {.fn preview_trailer_fields} on a raw file to see available fields.",
      "*" = "Pass field names to {.arg trailer_fields} or use {.val all} to extract all."
    ))
  }
  
  # If "all", get available fields from first valid file BEFORE parallelization
  if (identical(trailer_fields, "all")) {
    first_valid <- full_paths[exists_check][1]
    cli::cli_inform("Querying available trailer fields from first valid file...")
    trailer_fields <- rawrr::readTrailer(first_valid)
    cli::cli_inform("Found {length(trailer_fields)} trailer field{?s}.")
  }
  
  # Filter to existing files only
  valid_paths <- full_paths[exists_check]
  n_files <- length(valid_paths)
  
  if (parallel) {
    # Set up parallel backend
    if (is.null(n_workers)) {
      n_workers <- max(1, future::availableCores() - 1)
    }
    
    cli::cli_inform("Processing {n_files} file{?s} across {n_workers} worker{?s}...")
    
    # Set up future plan
    old_plan <- future::plan(future::multisession, workers = n_workers)
    on.exit(future::plan(old_plan), add = TRUE)
    
    start_time <- Sys.time()
    
    # Process in parallel
    results <- furrr::future_map(
      valid_paths,
      ~ process_single_file(.x, trailer_fields),
      .options = furrr::furrr_options(seed = TRUE)
    )
    
    elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
    n_success <- sum(vapply(results, function(x) !is.null(x) && nrow(x) > 0, logical(1)))
    cli::cli_inform("Completed: {n_success} of {n_files} files processed in {elapsed} minutes.")
    
  } else {
    # Sequential processing with progress bar
    results <- vector("list", n_files)
    
    cli::cli_progress_bar(
      "Extracting scan metadata",
      total = n_files,
      clear = FALSE
    )
    
    for (i in seq_along(valid_paths)) {
      cli::cli_progress_update()
      results[[i]] <- process_single_file(valid_paths[i], trailer_fields)
    }
    
    cli::cli_progress_done()
  }
  
  # Combine results
  combined <- dplyr::bind_rows(results)
  
  if (nrow(combined) == 0) {
    cli::cli_abort("No data extracted from any files.")
  }
  
  # Relocate dataset to first column and clean names
  combined <- combined |>
    dplyr::relocate(.data$dataset, .before = 1) |>
    janitor::clean_names()
  
  combined
}


# Internal helper functions -----------------------------------------------

#' Process a single raw file and extract metadata
#'
#' @param raw_path Full path to .raw file
#' @param trailer_fields Character vector of trailer field names or NULL
#' @return A data.frame with scan metadata, or NULL on failure
#' @keywords internal
process_single_file <- function(raw_path, trailer_fields) {
  
  dataset_name <- extract_dataset_name(raw_path)
  
  # Try to read index
  idx <- tryCatch(
    {
      rawrr::readIndex(raw_path)
    },
    error = function(e) {
      warning(sprintf("Failed to read index from %s: %s", dataset_name, e$message))
      return(NULL)
    }
  )
  
  if (is.null(idx)) return(NULL)
  
  # Add dataset column
  idx$dataset <- dataset_name
  
  # Extract trailer fields if requested
  if (!is.null(trailer_fields) && length(trailer_fields) > 0) {
    
    for (field in trailer_fields) {
      trailer_values <- tryCatch(
        {
          rawrr::readTrailer(raw_path, field)
        },
        error = function(e) {
          warning(sprintf("Failed to extract '%s' from %s: %s", field, dataset_name, e$message))
          return(NULL)
        }
      )
      
      if (!is.null(trailer_values)) {
        # rawrr returns character, keep as-is for now
        # Length should match number of scans
        if (length(trailer_values) == nrow(idx)) {
          idx[[field]] <- trailer_values
        } else {
          warning(sprintf(
            "Trailer field '%s' has %d values but index has %d scans in %s. Skipping field.",
            field, length(trailer_values), nrow(idx), dataset_name
          ))
        }
      }
    }
  }
  
  idx
}


#' Resolve a path to a full .raw file path
#'
#' @param path Folder path or full .raw path
#' @return Full path to .raw file
#' @keywords internal
resolve_raw_path <- function(path) {
  # If already ends in .raw, use as-is
  if (grepl("\\.raw$", path, ignore.case = TRUE)) {
    return(path)
  }
  
  # Otherwise, construct path: folder/basename/basename.raw
  base <- basename(path)
  file.path(path, paste0(base, ".raw"))
}


#' Extract dataset name from raw file path
#'
#' @param raw_path Full path to .raw file
#' @return Dataset name (filename without .raw extension)
#' @keywords internal
extract_dataset_name <- function(raw_path) {
  fname <- basename(raw_path)
  sub("\\.raw$", "", fname, ignore.case = TRUE)
}