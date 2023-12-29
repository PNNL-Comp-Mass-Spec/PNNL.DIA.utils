#' Console command to load dataset files from DMS to DIA-NN
#'
#' @param file_type LC-MS file extension, default is ".raw"
#' @param diann_console Option to modify file paths as console command for DIA-NN
#' @inheritParams PNNL.DMS.utils::get_datasets_by_data_package
#' 
#' @description
#' For saving output for pasting into DIA-NN console, use `readr::write_tsv()` 
#' with `col_names = FALSE` and `escape = "none"` for a `.tsv` that can be
#' pasted directly into DIA-NN
#' 
#' Not useful outside of PNNL unless connected through VPN. Works on windows out of the box.
#'
#' @details
#' Additional details...
#' 
#'
#' @return one column data frame. 
#' @export
#' 
#' @examples
#' diann_raw_paths <- dms_dataset_path(5293 , file_type = ".raw")
#'
dms_dataset_path <- function(data_package_num, file_type = ".raw", diann_console = TRUE) {
  
  # to avoid notes on check()
  Folder <- raw_file_path <- NULL
  
  
  df <- PNNL.DMS.utils::get_datasets_by_data_package(data_package_num) |>
    dplyr::select(Folder) |>
    dplyr::mutate(raw_file_path = paste0(Folder, "\\", basename(Folder), file_type))

  if (diann_console == TRUE) {
    df <- df |>
      dplyr::mutate(raw_file_path = paste0("--f \"", raw_file_path, "\"")) |>
      dplyr::select(raw_file_path)

    df
  }

  df
}
