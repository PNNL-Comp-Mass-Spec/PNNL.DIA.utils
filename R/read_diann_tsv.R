




#' Get Basename from Sample LC-MS file path
#'
#' @param x Absolute path to LC-MS file
#' @param drop_ext Options to drop file extensions, see Details
#' 
#' @return basename of LC-MS file path, drops file extension
#' 
#' @details
#' `drop_ext` specifies whether to drop file extensions. `"no"` keeps file exteions; 
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
#'
#' @return A data frame 
#' @export
#'
#' @examples
#' x <- system.file("extdata", "diann_report.tsv", package = "pnnl.diann.utils")
#' read_diann_tsv(x)
read_diann_tsv <- function(path, basename = TRUE){
  
  df <- data.table::fread(path, stringsAsFactors = FALSE) |>
    as.data.frame()
  
  if(basename){
    message("File Names renamed to basename; i.e. PATH/TO/MSFILE.raw.dia becomes MSFILE")
    if("File.Name" %in% colnames(df)){
      
      df <- df |>
        dplyr::mutate(File.Name = basename(File.Name))
      
    } else{
      df <- df |>
        dplyr::rename_with(basename) 
    }
  }
  
  df
  
}