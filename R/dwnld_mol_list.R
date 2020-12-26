#' Download multiple molecule structures
#'
#' Queries the NCI Chemical Identifier Resolver for structure
#' files corresponding to the names given in a vector.
#' File saved in the directory provided as the path.
#'
#' @param mol_list Character vector of molecule names
#' @param path Name of directory to save file
#' @param file_format Type of file to download. Default is "SDF".
#' @return Data frame corresponding to whether a file downloaded, received a warning, or received an error.

dwnld_mol_list <- function(mol_list, path, file_format = "SDF") {
  do.call(
    rbind,
    lapply(
      mol_list,
      dwnld_mol,
      path = path,
      file_format = file_format
    )
  )
}
