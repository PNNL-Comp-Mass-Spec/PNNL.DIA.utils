#' Parse DIA-NN Command Line Arguments
#'
#' @description
#' Extracts command line arguments from a DIA-NN log file and returns them as a data frame.
#' Can optionally suppress all `--f` calls..
#'
#' @param file_path character String containing the path to the DIA-NN log file
#' @param suppress_files logical If TRUE, suppresses file input parameters (--f).
#' Default is FALSE
#'
#' @return A data frame with two columns:
#' \itemize{
#'   \item parameter: The command line parameter (e.g., "--min-pr-mz")
#'   \item value: The corresponding value (e.g., "400")
#' }
#'
#' @details
#' The function searches for lines beginning with "diann.exe" and parses all command line
#' arguments that follow. Parameters starting with "--" are extracted along with their values.
#' If a parameter has no value, NA is assigned. When suppress_files is TRUE, the input
#' file parameters (--f) are removed from the final output.
#'
#' @examples
#' \dontrun{
#' # Get all parameters
#' result_all <- parse_diann_commands("path/to/diann_log.txt")
#'
#' # Get parameters excluding input files
#' result_filtered <- parse_diann_commands("path/to/diann_log.txt", suppress_files = TRUE)
#' }
#'
#' @export
#' @importFrom utils read.table
parse_diann_commands <- function(file_path, suppress_files = FALSE) {
  # Read the file
  lines <- readLines(file_path)
  
  # Find the line with the command (starts with "diann.exe")
  cmd_line <- lines[grep("^diann.exe", lines)]
  
  # Split the command line by spaces, but keep quoted strings together
  args <- strsplit(cmd_line, "(?<=\\s)(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", perl = TRUE)[[1]]
  
  # Remove "diann.exe" from the beginning and trim whitespace
  args <- trimws(args[args != "diann.exe"])
  
  # Create vectors to store parameters and values
  params <- c()
  values <- c()
  
  i <- 1
  while (i <= length(args)) {
    if (startsWith(args[i], "--")) {
      param <- trimws(args[i])  # Trim parameter
      
      # Check if next argument exists and doesn't start with --
      if (i + 1 <= length(args) && !startsWith(args[i + 1], "--")) {
        value <- trimws(args[i + 1])  # Trim value
        i <- i + 2
      } else {
        value <- NA
        i <- i + 1
      }
      
      params <- c(params, param)
      values <- c(values, value)
      
    } else {
      i <- i + 1
    }
  }
  
  # Create initial data frame with all parameters
  result <- data.frame(
    parameter = params,
    value = values,
    stringsAsFactors = FALSE
  )
  
  # Filter out file inputs if requested
  if (suppress_files) {
    result <- result[result$parameter != "--f", ]
  }
  
  return(result)
}