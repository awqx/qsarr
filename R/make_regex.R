#' Convert to regex
#'
#' `make_regex` escapes special regex characters in a string. The function
#' addresses the grouping characters (parentheses, brackets, and hyphens),
#' matching for the beginning and end of a string (`"^"` and `"$"`), regex
#' for number of repetitions, as well as the dot `"."` character.
#'
#' The function assumes that the string passed to the function needs to
#' found as-is. That is, the string is not indicative of any regex pattern
#' matching.
#'
#' @param s The string to convert to regex
#' @return A string with special characters escaped
#' @importFrom stringr str_replace_all
#' @export

make_regex <- function(s) {
  chr_pattern <- paste0(
    "\\",
    c(".", "-", "(", ")", "?", "*", "+", "[", "]", "^", "$", "{", "}"))
  chr_replace <- paste0("\\", chr_pattern)
  for (i in 1:length(chr_pattern)) {
    s <- stringr::str_replace_all(
      s,
      pattern = chr_pattern[i],
      replacement = chr_replace[i]
    )
  }
  return(s)
}
