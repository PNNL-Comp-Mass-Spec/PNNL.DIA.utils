#' Get Basename from Sample LC-MS file path
#'
#' @param x Absolute path to LC-MS file
#' @param drop_ext Options to drop file extensions: "no", "last", "all", see Details
#' 
#' @return basename of LC-MS file path, drops file extension
#' @export
#' @details
#' `drop_ext` specifies whether to drop file extensions. Default option is `"no"`, which keeps file extensions; 
#' `"last"` removes only the last extension, and `"all"` removes  everything after the first period (`.`); this isn't a problem 
#' if using data from DMS as periods are not allowed in dataset names 
#' (see [DMS Dataset Naming Conventions](https://prismwiki.pnl.gov/wiki/Dataset#DMS_Dataset_Naming_Conventions)); 
#'
#' @examples
#' x <- "PATH/TO/MSFile.raw.dia"
#' sample_basename(x, drop_ext = "all")
#' sample_basename(x, drop_ext = "last")
#' sample_basename(x, drop_ext = "no")
sample_basename <- function(x, drop_ext = c("no", "last", "all")){
  
  drop_ext <- match.arg(drop_ext)
  x <- basename(x)
  
  if(drop_ext == "all"){
    x <- gsub("\\..*", "", x)
  } else if (drop_ext == "last"){
    x <- fs::path_ext_remove(x)
  } else if (drop_ext == "no"){
    x
  }
  x
}

#' Reads DIA-NN `.tsv` output files
#'
#' @param path Filepath to DIA-NN `.tsv` output file 
#' @param basename `T` or `F` to apply `sample_basename()`
#' @param ... optional arguments to pass to `sample_basename()`
#' @inheritDotParams sample_basename drop_ext 
#'
#' @return A data frame 
#' 
#' @details
#' DIA-NN, whether a *pq_report* or *pg*/*gg* matrix applies full path names to samples.
#' The option `basename` applies the function `sample_basename` to either the `File.Names` 
#' column for *pq_reports* or the headers for *pg*/*gg* matrices. 
#' 
#' 
#' @export
#'
#' @examples
#' x <- system.file("extdata", "diann_report.tsv", package = "PNNL.DIA.utils")
#' read_diann_tsv(x, basename = TRUE, drop_ext = "all")
#' 
read_diann_tsv <- function(path, basename = FALSE, ...){
  
  # to pass R CMD CHECK
  File.Name <- NULL
  
  df <- data.table::fread(path, stringsAsFactors = FALSE) |>
    as.data.frame()
  
  if(basename){
    
    # basename for diann precursorsor report
    if("File.Name" %in% colnames(df)){
      message("File.Name values renamed using pnnl.diann.utils::sample_basename()")
      
      df <- df |>
        dplyr::mutate(File.Name = sample_basename(File.Name, ...))
      df
      
      # basename for diann pg or gg matrix
    } else {
      message("Headers renamed using pnnl.diann.utils::sample_basename()")
      
      args <- list(...)
      
      # workaround to default to "no" if no arguments applied
      if(!length(args)){args <- list(drop_ext = "no")}
      
      df <- df |>
        dplyr::rename_with(.fn = ~ sample_basename(.x, drop_ext = args[[1]]),
                           .cols = dplyr::contains("\\"))
      
    }
  }
  
  df
}

#' Reads DIA-NN parquet report files
#'
#' @param path Filepath to DIA-NN parquet report file
#' @param basename `T` or `F` to apply `sample_basename()`
#' @param ... optional arguments to pass to `sample_basename()`
#' @inheritDotParams sample_basename drop_ext 
#'
#' @return A data frame
#' 
#' @details
#' DIA-NN parquet report files contain full path names in the `Run` column.
#' The option `basename` applies the function `sample_basename` to the `Run` column.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' x <- "path/to/your/diann_report.parquet"
#' read_diann_parquet(x, basename = TRUE, drop_ext = "all")
#' }
read_diann_parquet <- function(path, basename = FALSE, ...){
  
  # to pass R CMD CHECK
  Run <- NULL
  
  df <- arrow::read_parquet(path) |>
    as.data.frame()
  
  if(basename){
    message("Run values renamed using pnnl.diann.utils::sample_basename()")
    
    df <- df |>
      dplyr::mutate(Run = sample_basename(Run, ...))
  }
  
  df
}