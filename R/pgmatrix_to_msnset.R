

#' Converts DIA-NN matrix report to msnset object
#'
#' @param x protein groups matrix read  into R
#' @param sample_tag Either a substring matching sample names, 
#' can be a vector of multiple substrings or vector of sample names.
#' For now the default is "raw" meaning that the datasets are Thermo raw files.
#' @param id_header Which matrix column to be used in fData slot rowname
#'
#' @importFrom dplyr select any_of rename_with
#' @importFrom tibble column_to_rownames
#' @importFrom stringr str_remove_all
#' @importFrom MSnbase MSnSet
#' 
#' @return MSnSet object 
#' 
#' @export
#'
#' @examples
#' 
#' pg_path <- system.file("extdata",
#'   "QC_Mam_19_01_b_DIA_report.pg_matrix.tsv", 
#'    package = "PNNL.DIA.utils")
#'    
#' pg <- read_diann_tsv(pg_path) 
#'    
#' pgmatrix_to_msnset(pg)
pgmatrix_to_msnset <- function(x, sample_tag = "raw", id_header = "Protein.Group"){
  
  df <- as.data.frame(x)
  
  sample_idx <- grepl(paste(sample_tag, collapse = "|"), colnames(df))
  
  # f_data 
  # f_data_cols <- c("Protein.Group",
  #                  "Protein.Ids",
  #                  "Protein.Names",
  #                  "Genes",
  #                  "First.Protein.Description")
  f_data_cols <- colnames(df)[!sample_idx]
  
  f_data <- df |>
    dplyr::select(dplyr::any_of(f_data_cols)) |>
    tibble::column_to_rownames(var = id_header)
  
  # exprs data 
  
  exprs <- df |>
    dplyr::select(-dplyr::any_of(f_data_cols)) |>
    dplyr::rename_with(
      ~ stringr::str_remove_all(
        basename(.x), "\\..*")
      ) |>
    as.matrix()
    
  # p_data
  
  p_data <- data.frame(dataset = colnames(exprs)) |>
    `rownames<-`(colnames(exprs))
    
  
  # assemble msnset
  
  m <- MSnbase::MSnSet(
    exprs = exprs, 
    fData = f_data,
    pData = p_data
  )
  
  m

}