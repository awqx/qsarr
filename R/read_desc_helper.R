#' Read a file of chemical descriptors (helper)
#'
#' `read_desc_helper` is a wrapper for `[read_desc()]` that runs the
#' function with error handling. It is used as the function call
#' in `[read_desc_list()]` to prevent the function from exiting
#' if a file cannot be read.
#'
#' @param path The location of the file to be read
#' @param quiet Whether the function will produce messages.
#'   The default is `FALSE`.
#' @param shorten Whether to message the shortened file name.
#'   The default is `TRUE`.

read_desc_helper <- function(path, quiet = F, shorten = T) {
  if (shorten) file_name <- str_remove(path, "^[[:print:]]+/")
  tryCatch({
    df <- read_desc(path)
    if (!quiet)
      message(file_name, " was successfully read")
    return(df)
  },
  warning = function(w) {
    if (!quiet) {
      message("A warning occurred:")
      message(w)
    }
  },
  error = function(e) {
    if (!quiet) {
      message("An error occurred:")
      message(e)
      message(
        "Is the file of the correct type? ",
        "The function accepts CSV, RDS, or Excel files."
        )
    }
  },
  finally = {
    if (!quiet)
      message("Processed ", file_name)
  })
}
