x_path <- system.file("extdata",
                      "diann_report.tsv",
                      package = "pnnl.diann.utils")

x <- read_diann_tsv(x_path)


View(x)


iq::process_long_format(x_path, output_filename = "report-pg-annotated.tsv",
                        annotation_col = c("Protein.Names", "Genes"))

fun <- utils::getFromNamespace("fast_MaxLFQ", "iq")