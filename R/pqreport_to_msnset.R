

#' Helper function, makes matrix of expression values 
#'
#' @param df data.frame of DIA-NN pq_report
#' @param id_header Precursor ID column header
#' @param quantity_header Precursors Quantity Column
#' @param sample_header Run/Sample header column
#'
#' @return Matrix of quantity_header with col_names = sample_header; row_names = precursor_id, 
#'
make_esprs <- function(df, id_header, quantity_header, sample_header = "File.Name"){
  
  m <- df |>
    dplyr::mutate({{quantity_header}} := dplyr::na_if(.data[[quantity_header]], 0),
                  {{quantity_header}} := dplyr::na_if(.data[[quantity_header]], Inf)) |>
    dplyr::select(dplyr::all_of( c(id_header, quantity_header, sample_header))) |>
    tidyr::pivot_wider(names_from = .data[[sample_header]], 
                       values_from = .data[[quantity_header]]) |>
    tibble::column_to_rownames(var = id_header) |>
    as.matrix()
  
  m
  
}


#' Converts DIA-NN precursor report to msnset object
#'
#' @param x precursor report read into R
#' @param id_header protein/peptide/precursor Id column name
#' @param quantity_header quantity column name
#' @param proteotypic_only only proteotypic peptides and the respective proteins should be considered
#' @param q q-value threshold
#' @param protein_q uniquely identified protein q-value threshold
#' @param pg_q protein group q-value threshold
#' @param gg_q gene group q-value threshold
#' @param strip_fData TRUE will remove all fData except for id_header, FALSE will attempt to keep protein meta info from DIA-NN. 
#' 
#' @importFrom rlang .data 
#' 
#' @description
#' Mirrors functionality of [`diann::diann_matrix()`](https://github.com/vdemichev/diann-rpackage), but outputs an MSnSet object.
#' 
#' If you have multiple DIA-NN precursor reports that you've combined into one 
#' i.e. `cbind (pq_report1, pq_report2)`) and you want to convert that into one `msnset` object, 
#' set `strip_fData = TRUE` to remove protein and gene group info columns from the fData slot to avoid errors
#' when going from the *long* precursor report to the *wide* msnset. Errors emanate from disparities in
#' protein group assignment for a given precursor in the report. For ex. *precA* in *run 1* could be assigned
#' to *protein group X*, whereas the same *precA* in *run 2* could be assigned to *protein groups X; Y*. 
#' When comparing these types of samples, bet set `strip_fData = FALSE` and compare at the precursor level only. 
#'
#' @return MSnSet object
#' @export
#'
#' @examples
#' 
#' x_path <- system.file("extdata",
#'   "diann_report.tsv",
#'    package = "pnnl.diann.utils")
#' 
#' x <- read_diann_tsv(x_path)
#' 
#' pqreport_to_msnset(x)
pqreport_to_msnset <- function(x,
                               id_header = "Precursor.Id",
                               quantity_header = "Precursor.Normalised",
                               proteotypic_only = F,
                               q = 0.01,
                               protein_q = 1.0,
                               pg_q = 1.0,
                               gg_q = 1.0,
                               strip_fData = FALSE) {
  # to avoid notes on check()
  Q.Value <- Protein.Q.Value <- PG.Q.Value <- GG.Q.Value <- File.Name <- Proteotypic <-  NULL

  df <- as.data.frame(x)

  # Filtering df based on provided cut-offs

  if (proteotypic_only) {
    df <- df |>
      dplyr::filter(Proteotypic != 0)
  }

  df <- df |>
    dplyr::filter(.data[[id_header]] != "") |>
    dplyr::filter(.data[[quantity_header]] > 0) |>
    dplyr::filter(Q.Value <= q) |>
    dplyr::filter(Protein.Q.Value <= protein_q) |>
    dplyr::filter(PG.Q.Value <= pg_q) |>
    dplyr::filter(GG.Q.Value <= gg_q)

  # checking for duplicate precursors in ind. runs.
  is_duplicated <- any(duplicated(paste0(df[["File.Name"]], ":", df[[id_header]])))

  if (is_duplicated) {
    warning("Multiple quantities per id: the maximum of these will be calculated")

    df <- df |>
      dplyr::group_by(File.Name, .data[[id_header]]) |>
      dplyr::slice_max(.data[[quantity_header]], n = 1) |>
      dplyr::ungroup()
  }

  # Creating expression matrix for MSnSet
  exprs <- df |>
    make_esprs(
      id_header = {{ id_header }},
      quantity_header = {{ quantity_header }},
      sample_header = "File.Name"
    )

  # fData

  if(strip_fData == TRUE){
    
    # fData empty except for rownames matching those in exprs
    
    f_data <- data.frame(id_header = rownames(exprs)) |>
      tibble::column_to_rownames(var = "id_header")
    
  } else {
    
    # fData will contain protein group info from DIA-NN report. 
    
    f_data_cols <- c(
      "Protein.Group",
      #"Protein.Ids", # Dropping b/c not useful and messes order of protein.group
      "Protein.Names",
      "Genes",
      "First.Protein.Description",
      id_header
      )
    
    f_data_temp <- df |>
      dplyr::select(dplyr::any_of(f_data_cols)) |>
      dplyr::distinct()
    
    f_data <- data.frame(id_header = rownames(exprs)) |>
      dplyr::left_join(f_data_temp,
      by = c(id_header = id_header)
      ) |>
      tibble::column_to_rownames(var = "id_header")
  
    
    }  

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
