

#' Asserts if all substrings of AGEPRO's input data file line can be numeric.
#'
#' Validates the string vector via `grepl` if all values match the digit
#' character class. Function will throw an exception if non digit characters
#' were found.
#'
#' @return Converts the input data line string vector as numeric.
#'
#' @template inp_line
#'
#' @keywords internal
#'
assert_numeric_substrings <- function(inp_line) {

  if(!all(grepl("^[[:digit:]]",inp_line))) {

    non_numerics <- inp_line[!grepl("^[[:digit:]]",inp_line)]
    stop("Line contains a Non Numeric Substring",
         paste(non_numerics, collapse = ", "))
  }

  invisible(as.numeric(inp_line))

}
