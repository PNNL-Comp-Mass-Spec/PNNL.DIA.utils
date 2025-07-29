#' Global variables
#'
#' @keywords internal
#' @noRd
NULL

# Suppress R CMD check notes for variables used in dplyr operations
utils::globalVariables(c("success", "output_size_MB", "input_size_MB", 
                         "size_ratio", "error_message", "input_file"))