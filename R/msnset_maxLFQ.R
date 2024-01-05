


#' MaxLFQ quantites of precursors in msnset
#'
#' @param m DIA-NN precursors report as MSnSet from `pqreport_to_msnset()`
#'
#' @return list of maxLFQ intensities
#' @export
#'
#' 
msnset_maxLFQ <- function(m){
  # To DO 
  
  # output msnset object at protein level
  # make function good for gene level roll up as well
  # check for missing protein group info (could be general msnset function)
  # pipeline for msnset normalization 
  
  
  ls <- MSnbase::exprs(m) |>
    as.data.frame() |>
    tibble::rownames_to_column(var= "id") |>
    cbind(MSnbase::fData(m)[, "Protein.Group"]) |>
    dplyr::rename("protein_list" = `MSnbase::fData(m)[, "Protein.Group"]`) |>
    dplyr::filter(protein_list != "") |>
    tidyr::pivot_longer(cols = -c(protein_list, id), 
                        names_to = "sample_list", 
                        values_to = "quant") |>
    dplyr::filter(!is.na(quant)) |>
    dplyr::mutate(quant = log2(quant)) |>
    as.list()
  
  ls_maxlfq <- iq::fast_MaxLFQ(ls)
  
  ls
  
}

