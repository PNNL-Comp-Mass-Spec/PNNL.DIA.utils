

pqreport_to_msnset <- function(x,
                               id_header = "Precursor.Id",
                               quantity_header = "Precursor.Normalised",
                               proteotypic_only = F,
                               q = 0.01,
                               protein_q = 1.0,
                               pg_q = 1.0,
                               gg_q = 1.0) {

  df <- as.data.frame(x)

  # Filtering df based on provided cut-offs

  if(proteotypic_only){
    df <- df |>
      dplyr::filter(Protetypic != 0)
  }

  df <- df |>
    dplyr::filter({{id_header}} != "") |>
    dplyr::filter({{quantity_header}} > 0) |>
    dplyr::filter("Q.Value" < q) |>
    dplyr::filter("Protein.Q.Value" < protein_q) |>
    dplyr::filter("PG.Q.Value" < pg_q) |>
    dplyr::filter("GG.Q.Value" < gg_q)

  # checking for duplicate precursors in ind. runs.
  is_duplicated = any(duplicated(paste0(df[["File.Name"]],":",df[[id_header]])))

  if (is_duplicated){
    warning("Multiple quantities per id: the maximum of these will be calculated")

  }

  
  
}
