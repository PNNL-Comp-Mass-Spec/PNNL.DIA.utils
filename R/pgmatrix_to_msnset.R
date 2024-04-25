

#' Converts DIA-NN matrix report to msnset object
#'
#' @param x protein groups matrix read  into R
#' @param id_header Which matrix column to be used in fData slot rowname
#'
#' @return MSnSet object 
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
pgmatrix_to_msnset <- function(x, id_header = "Protein.Group"){
  
  df <- as.data.frame(x)
  
  # f_data 
  
  f_data_cols <- c("Protein.Group",
                   "Protein.Ids",
                   "Protein.Names",
                   "Genes",
                   "First.Protein.Description")
  
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