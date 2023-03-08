
#' Read Input File
#'
#' Reads AGEPRO Input File
#'
#' @param file Filename
#'
#' @importFrom jsonlite read_json
#'
#' @export
read_inpfile <- function(file) {
  return(jsonlite::read_json(file, simplifyVector = TRUE))
}


#' Print json string
#'
#' Pretty Print R objects as json string to console.
#'
#' @param x object
#'
#' @importFrom jsonlite toJSON
#'
#' @export
print_json <- function(x) {
  return(jsonlite::toJSON(x, pretty = TRUE))
}
