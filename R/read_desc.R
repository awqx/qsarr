#' Read a file of chemical descriptors
#'
#' `read_desc` can read an RDS, CSV, or Excel file with chemical
#' descriptors. The function is a helpful wrapper for `readRDS`,
#' `read.csv` and `read_excel` from the package `readxl`.
#'
#' The path passed to the function must include the file extension.
#'
#' @param path The path to the file to be read
#' @importFrom stringr str_extract str_remove
#' @importFrom readxl excel_sheets read_excel
#' @export

read_desc <- function(path) {
  file_type <- str_extract(path, "\\.[[:alnum:]]+$") %>%
    str_remove("\\.") %>%
    tolower()

  # Solution to reading sheets in Excel file inspired by
  # Jeromy Anglim on Stack Overflow
  # https://stackoverflow.com/questions/12945687/
  df <- switch(
    file_type,
    "rds" = readRDS(path),
    "csv" = read.csv(path),
    "xls" =,
    "xlsx" = {
      sheets <- excel_sheets(path)
      do.call(
        cbind,
        lapply(
          sheets,
          function(x) read_excel(path, sheet = x)
        )
      )
    }
  )

  as.data.frame(df)
}
