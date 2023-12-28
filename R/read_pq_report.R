
#' Reads DIA-NN Precursor report
#'
#' @param path Filepath to DIA-NN `report.tsv`
#'
#' @return A data frame 
#' @export
#'
#' @examples
#' x <- system.file("extdata", "diann_report.tsv", package = "pnnl.diann.utils")
#' read_pq_report(x)
read_pq_report <- function(path){
  
  df <- data.table::fread(path) |>
    as.data.frame()
  df
  
}