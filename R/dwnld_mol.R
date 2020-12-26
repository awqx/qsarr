#' Download molecule structure
#'
#' Queries the NCI Chemical Identifier Resolver for a structure file corresponding to the name given.
#' File saved in the directory provided as the path.
#'
#' @param mol Name of molecule
#' @param path Name of directory to save file
#' @param file_format Type of file to download. E.g., "SDF"
#' @return Data frame corresponding to whether a file downloaded, received a warning, or received an error.

dwnld_mol <- function(mol, path, file_format) {
  destfile   <- paste0(path, "/", mol, ".", file_format)
  mol_url  <- unlist(lapply(mol, URLencode, reserved = T))
  cactus_url <- paste0(
    "https://cactus.nci.nih.gov/chemical/structure/",
    mol_url, "/", file_format
  )
  report <- tryCatch({
    GET(cactus_url, write_disk(destfile, overwrite = T))
    data.frame(
      mol = mol,
      downloaded = 1,
      warning = 0,
      error = 0)
  },
  warning = function(warn) {
    message("Warning: URL error or attempt to overwrite existing directory.")
    GET(cactus_url, write_disk(destfile, overwrite = T))
    data.frame(
      mol = mol,
      downloaded = 1,
      warning = 1,
      error = 0)
  },
  error = function(err) {
    message("An error occurred:")
    message(err)
    data.frame(
      mol = mol,
      downloaded = 0,
      warning = 0,
      error = 1)
  },
  finally = {
    message(paste0("Processed ", mol))
  })
  return(report)
}
