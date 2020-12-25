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
  for (i in 1:length(chr_pattern)) {
    s <- str_replace(
      pattern = chr_pattern[i],
      replacement = chr_replace[i]
    )
  }
  s
}
