#' Combine SDF files
#'
#' `combine_sdf` combines a directory of SDFs into a single SDF.
#' For each molecule in the single SDF, the molecule name is the filename.
#' If the directory is empty, the function returns a 0.
#' The function will message if a directory is processed. This
#' can be suppressed with `quiet = T`.
#'
#' @param mol_dir The directory containing the SDFs to combine.
#' @param quiet Whether or not the function will message when a directory
#'   is processed. The default is `quiet = FALSE`.
#' @return An SDF as a data frame with one column.
#' @import dplyr
#' @importFrom stringr str_detect str_remove
#' @export

combine_sdf <- function(mol_dir, quiet = F) {
  # Exit if directory is empty
  if (!length(list.files(mol_dir))) {
    if (!quiet) {
      message(
        "The directory ", mol_dir, " is empty.", "\n",
        "Skipping ", mol_dir, " and returning 0."
      )
    }
    return(0)
  }

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

  # Assign the correct names
  for(i in 1:length(sdf_list)) sdf_list[[i]][1, ] <- mol_names[i]

  # Message that directory is processed
  if (!quiet) message("The directory", mol_dir, " was processed.")

  # Return combined list
  do.call(rbind, sdf_list)
}
