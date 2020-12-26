#' Download molecule structure
#'
#' `dwnld_mol` queries the NCI Chemical Identifier Resolver for a structure
#' file corresponding to the name given. The file is saved in the directory
#' provided as the path. The Chemical Identifier Resolver can be found at
#' <https://cactus.nci.nih.gov/chemical/structure>.
#'
#' @param mol Name of molecule
#' @param path Name of directory to save file
#' @param file_format Type of file to download, default of "SDF". The full list
#'   of possible formats can be found in the Chemical Identifier Resolver.
#' @return The function returns a data frame row corresponding to whether a
#'   file downloaded, received a warning, or received an error.
#' @importFrom httr GET
#' @export

dwnld_mol <- function(mol, path, file_format = "SDF") {
  destfile   <- paste0(path, "/", mol, ".", file_format)
  mol_url  <- unlist(lapply(mol, URLencode, reserved = T))
  cactus_url <- paste0(
    "https://cactus.nci.nih.gov/chemical/structure/",
    mol_url, "/", file_format
  )
  report <- tryCatch({
    httr::GET(cactus_url, write_disk(destfile, overwrite = T))
    data.frame(
      mol = mol,
      downloaded = 1,
      warning = 0,
      error = 0)
  },
  warning = function(warn) {
    message("Warning: URL error or attempt to overwrite existing directory.")
    httr::GET(cactus_url, write_disk(destfile, overwrite = T))
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
