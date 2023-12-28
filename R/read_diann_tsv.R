
#' Reads DIA-NN `.tsv` output files
#'
#' @param path Filepath to DIA-NN `.tsv` output file 
#'
#' @return A data frame 
#' @export
#'
#' @examples
#' x <- system.file("extdata", "diann_report.tsv", package = "pnnl.diann.utils")
#' read_diann_tsv(x)
read_diann_tsv <- function(path){
  
  df <- data.table::fread(path) |>
    as.data.frame()
  df
  
}