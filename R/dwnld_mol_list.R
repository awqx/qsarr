#' Download multiple molecule structures
#'
#' `dwnld_mol_list` is a wrapper for `lapply` and `[dwnld_mol()]`. It queries
#' the NCI Chemical Identifier Resolver for structure files corresponding to
#' the names given in a vector.
#' Files are saved in the directory provided as the path.
#' The output is a single data frame of the results.
#'
#' @param mol_list Character vector or list of molecule names
#' @param path Name of directory to save the files
#' @param file_format Type of file to download, default of "SDF". The full list
#'   of possible formats can be found in the Chemical Identifier Resolver.
#' @return Data frame corresponding to whether a file downloaded, received a
#'   warning, or received an error.
#' @export

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
