# wellplate_utils.R

#' Convert wellplate location to numeric value (by row)
#'
#' This function converts wellplate locations to numeric values reading across each row.
#' Format: A1 = 1, A2 = 2, A12 = 12, B1 = 13, etc.
#' This is used for preparing the Evosep loading queue.
#'
#' @param well A character vector of wellplate locations (e.g., "A1", "B2", "H12").
#' 
#' @return An integer vector containing the numeric values corresponding to the wellplate locations.
#' 
#' @examples
#' wellplate_to_num_by_row(c("A1", "A2", "B1", "H12"))
#' # [1]  1  2 13 96
#'
#' @export
wellplate_to_num_by_row <- function(well) {
  convert_well <- function(well) {
    row_letter <- substr(well, 1, 1)
    col_number <- as.numeric(substr(well, 2, nchar(well)))
    row_value <- as.numeric(charToRaw(row_letter)) - 64
    num_value <- (row_value - 1) * 12 + col_number
    return(num_value)
  }
  sapply(well, convert_well)
}

#' Convert numeric value to wellplate location (by row)
#'
#' This function converts the rowwise numeric values back to wellplate locations in the format "A1", "B2", etc.
#' where 1 = A1, 2 = A2, 13 = B1, etc. Basically Evosep number back to well.
#'
#' @param num An integer vector containing numeric values to be converted to wellplate locations.
#' 
#' @return A character vector of wellplate locations corresponding to the numeric values.
#' 
#' @examples
#' num_to_wellplate_by_row(c(1, 2, 13, 96))
#' # [1] "A1" "A2" "B1" "H12"
#'
#' @export
num_to_wellplate_by_row <- function(num) {
  convert_num <- function(num) {
    row_num <- (num - 1) %/% 12 + 1
    column_num <- (num - 1) %% 12 + 1
    row_letter <- rawToChar(as.raw(row_num + 64))
    return(paste0(row_letter, column_num))
  }
  sapply(num, convert_num)
}

#' Convert wellplate location to numeric value (by column)
#'
#' This function converts wellplate locations to numeric values reading down each column.
#' Format: A1 = 1, B1 = 2, H1 = 8, A2 = 9, etc. Assumes 8 rows (A-H).
#'
#' @param well A character vector of wellplate locations (e.g., "A1", "B1", "A2").
#' 
#' @return An integer vector containing the numeric values corresponding to the wellplate locations.
#' 
#' @examples
#' wellplate_to_num_by_column(c("A1", "B1", "A2", "H12"))
#' # [1]  1  2  9 96
#'
#' @export
wellplate_to_num_by_column <- function(well) {
  convert_well <- function(well) {
    row_letter <- substr(well, 1, 1)
    col_number <- as.numeric(substr(well, 2, nchar(well)))
    row_number <- which(LETTERS == row_letter)
    return((col_number - 1) * 8 + row_number)
  }
  sapply(well, convert_well)
}

#' Convert numeric value to wellplate location (by column)
#'
#' This function converts numeric values to wellplate locations reading down each column.
#' Format: 1 = A1, 2 = B1, 8 = H1, 9 = A2, etc. Assumes 8 rows (A-H).
#'
#' @param num An integer vector containing numeric values to be converted to wellplate locations.
#' 
#' @return A character vector of wellplate locations corresponding to the numeric values.
#' 
#' @examples
#' num_to_wellplate_by_column(c(1, 2, 9, 96))
#' # [1] "A1" "B1" "A2" "H12"
#'
#' @export
num_to_wellplate_by_column <- function(num) {
  convert_num <- function(num) {
    col_num <- ceiling(num / 8)
    row_num <- ((num - 1) %% 8) + 1
    row_letter <- LETTERS[row_num]
    return(paste0(row_letter, col_num))
  }
  sapply(num, convert_num)
}