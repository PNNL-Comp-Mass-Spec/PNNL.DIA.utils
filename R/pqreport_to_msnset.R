

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
    dplyr::select(all_of( c(id_header, quantity_header, sample_header))) |>
    tidyr::pivot_wider(names_from = .data[[sample_header]], 
                       values_from = .data[[quantity_header]]) |>
    tibble::column_to_rownames(var = id_header) |>
    as.matrix()
  
  m
  
}


pqreport_to_msnset <- function(x,
                               id_header = "Precursor.Id",
                               quantity_header = "Precursor.Normalised",
                               proteotypic_only = F,
                               q = 0.01,
                               protein_q = 1.0,
                               pg_q = 1.0,
                               gg_q = 1.0) {

  # to avoid notes on check()
  Q.Value <- Protein.Q.Value <- PG.Q.Value <- GG.Q.Value <- File.Name <- NULL
  
  df <- as.data.frame(x)

  # Filtering df based on provided cut-offs

  if(proteotypic_only){
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
  is_duplicated = any(duplicated(paste0(df[["File.Name"]],":",df[[id_header]])))

  if (is_duplicated){
    
    warning("Multiple quantities per id: the maximum of these will be calculated")

    df <- df |>
      dplyr::group_by(File.Name, .data[[id_header]]) |>
      dplyr::slice_max(.data[[quantity_header]], n = 1) |>
      dplyr::ungroup()
    }
    
    m <- df |>
      make_esprs(id_header = {{id_header}},
                 quantity_header = {{quantity_header}},
                 sample_header = "File.Name")
    
  m
  
}
