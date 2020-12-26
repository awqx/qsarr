#' Combine SDF files
#'
#' `combine_sdf` combines a directory of SDFs into a single SDF.
#' For each molecule in the single SDF, the molecule name is the filename.
#'
#' @param mol_dir The directory containing the SDFs to combine.
#' @return An SDF as a data frame with one column.
#' @import dplyr
#' @importFrom stringr str_detect str_remove
#' @export

combine_sdf <- function(mol_dir) {
  mol_files <- list.files(mol_dir, full.names = T) %>%
    .[stringr::str_detect(., "(?i)sdf")]
  mol_names <- list.files(mol_dir) %>%
    stringr::str_remove("(?i)\\.sdf")

  # Removing empty files
  mol_info  <- lapply(mol_files, file.info) %>%
    lapply(function(x) x$size > 30) %>%
    unlist()
  mol_files <- mol_files[mol_info]
  mol_names <- mol_names[mol_info]

  # Reading into a list
  sdf_list <- lapply(
    mol_files,
    read.csv,
    header = F,
    stringsAsFactors = F
    )

  # Assigning the correct names
  for(i in 1:length(sdf_list)) sdf_list[[i]][1, ] <- mol_names[i]

  # return combined list
  do.call(rbind, sdf_list)
}
