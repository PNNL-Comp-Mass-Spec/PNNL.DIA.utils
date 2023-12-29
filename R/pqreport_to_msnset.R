
# 
# pqreport_to_msnset <- function(x, 
#                                id_header = "Precursor.Id", 
#                                quantity_header = "Precursor.Normalised",
#                                proteotypic_only = F, 
#                                q = 0.01,
#                                protein_q = 1.0,
#                                pg_q = 1.0, 
#                                gg_q = 1.0) {
#   
#   df <- as.data.table(x)
#   
#   if(proteotypic_only){
#     df <- df |>
#       filter(Protetypic != )
#   }
#   
# }
#   