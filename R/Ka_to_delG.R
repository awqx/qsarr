#' Ka to dG conversion
#'
#' Convert a value from Ka (constant of association) to Gibbs free energy (kJ/mol)
#'
#' @param ka The Ka value
#' @return Gibbs free energy, in kJ/mol

Ka_to_delG <- function(ka) {
  -8.314*298*log(ka)/1000
}
