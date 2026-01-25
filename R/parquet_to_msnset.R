
#' Converts Parquet table to MSnSet
#'
#' @param data.frame output from `read_diann_parquet()`
#'
#' @return MSnSet object
#' 
#' @details
#' At this point it return only precurso (pq) type MSnSet. The only additional
#' piece of data is iRT - normalized retention time of a peptide/precursor.
#'
#' @importFrom tidyr pivot_wider
#' @importFrom Biobase exprs fData `fData<-`
#' @importFrom purrr map map_lgl
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fpath <- system.file("extdata", "report.parquet", package = "PNNL.DIA.utils")
#' pqt <- read_diann_parquet(fpath)
#' m <- parquet_to_msnset(pqt) }
#' 

parquet_to_msnset <- function(pqt){
  
  f_data_cols <- c("Protein.Group",
                   "Protein.Ids",
                   "Protein.Names",
                   "Genes",
                   "Proteotypic",
                   "Stripped.Sequence",
                   "Modified.Sequence",
                   "Precursor.Charge",
                   "Precursor.Id",
                   "iRT")
  
  x <- pivot_wider(pqt, names_from = Run, values_from = Precursor.Normalised,
                   id_cols = any_of(f_data_cols))
  
  f_data <- as.data.frame(x[,f_data_cols])
  # temporarily we'll use Precursor.Id as feature names
  rownames(f_data) <- f_data$Precursor.Id
  
  
  x_data <- x[,setdiff(colnames(x), f_data_cols)]
  x_data[x_data == 0] <- NA
  # clean the sample names
  x_data_colnames <- colnames(x_data)
  x_data_colnames <- basename(x_data_colnames)
  colnames(x_data) <- x_data_colnames
  x_data <- as.matrix(x_data)
  rownames(x_data) <- rownames(f_data)
  
  p_data <- data.frame(Dataset = colnames(x_data), 
                       row.names = colnames(x_data))
  
  m <- MSnSet(exprs = x_data, fData = f_data, pData = p_data)
  
  fData(m)$total_int <- rowSums(exprs(m), na.rm = TRUE)
  
  # filter out columns from fData that are complete NAs
  idx <- fData(m) %>% map(is.na) %>% map(all) %>% map_lgl(`!`)
  fData(m) <- fData(m)[,idx]
  
  return(m)
  
}


