# wellplate_utils.R

#' Convert wellplate location to numeric value
#'
#' This function converts wellplate locations in the format "A1", "B2", etc. 
#' to their corresponding numeric values where A1 = 1, A2 = 2, B1 = 13, etc.
#' This is used for preparing the Evosep loading queue.
#'
#' @param well A character vector of wellplate locations (e.g., "A1", "B2", "H12").
#' 
#' @return An integer vector containing the numeric values corresponding to the wellplate locations.
#' 
#' @examples
#' wellplate_to_num(c("A1", "A2", "B1", "B2", "H12"))
#' # [1]  1  2 13 14 96
#'
#' @export
wellplate_to_num <- function(well) {
  convert_well <- function(well) {
    row_letter <- substr(well, 1, 1)
    column_number <- as.numeric(substr(well, 2, nchar(well)))
    row_value <- as.numeric(charToRaw(row_letter)) - 64
    num_value <- (row_value - 1) * 12 + column_number
    return(num_value)
  }
  sapply(well, convert_well)
}

#' Convert numeric value to wellplate location
#'
#' This function converts numeric values back to wellplate locations in the format "A1", "B2", etc.
#' where 1 = A1, 2 = A2, 13 = B1, etc.
#'
#' @param num An integer vector containing numeric values to be converted to wellplate locations.
#' 
#' @return A character vector of wellplate locations corresponding to the numeric values.
#' 
#' @examples
#' num_to_wellplate(c(1, 2, 13, 14, 96))
#' # [1] "A1" "A2" "B1" "B2" "H12"
#'
#' @export
num_to_wellplate <- function(num) {
  convert_num <- function(num) {
    row_num <- (num - 1) %/% 12 + 1
    column_num <- (num - 1) %% 12 + 1
    row_letter <- rawToChar(as.raw(row_num + 64))
    return(paste0(row_letter, column_num))
  }
  sapply(num, convert_num)
}
