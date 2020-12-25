#' Convert to regex
#'
#' Escapes special characters in a string for compatability with regex
#'
#' @param s The string to convert to regex
#' @return A string with special characters escaped

make_regex <- function(s) {
  chr_pattern <- paste0(
    "\\",
    c(".", "-", "(", ")", "?", "*", "+"))
  chr_replace <- paste0("\\", chr_pattern)
  mapply(
    str_replace,
    pattern = chr_pattern,
    replacement = chr_replace,
    MoreArgs = list(string = s))
}
