

#' Converts DIA-NN matrix report to msnset object
#'
#' @param path Filepath to DIA-NN protein or gene group output file
#' @param id_header Which matrix column to be used in fData slot rowname
#'
#' @return MSnSet object 
#' @export
#'
#' @examples
#' 
#' x <- system.file("extdata",
#'   "QC_Mam_19_01_b_DIA_report.pg_matrix.tsv", 
#'    package = "pnnl.diann.utils")
#'    
#' pgmatrix_to_msnset(x)
pgmatrix_to_msnset <- function(path, id_header = "Protein.Group"){
  
  df <- read_diann_tsv(path)
  
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