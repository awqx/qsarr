#' Write an SDF
#'
#' `write_sdf` can write the output of `combine_sdf` into an SDF file.
#' The function is a wrapper for `write.table` with settings adjusted
#' for valid SDF files.
#' The path passed to the function does not have to be suffixed with
#' ".SDF". The function will append the correct file suffix.
#'
#' @param sdf The SDF as a data frame. Accepts the output data frame
#'   from `combine_sdf`
#' @param path The file path for the SDF. If it does not end with
#'   ".SDF", the appropriate file suffix will be applied.
#' @export

write_sdf <- function(sdf, path) {
  # If the path doesn't end with ".SDF", append the filetype
  if (!str_detect(path, "\\.(?i)sdf$")) {
    path <- paste0(path, ".SDF")
  }
  write.table(
    sdf,
    path,
    quote = F,
    row.names = F,
    col.names = F
  )
}
