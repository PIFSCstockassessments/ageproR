

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


#' Reads a line of numeric strings from the AGEPRO input file connection.
#'
#' Reads in a line from the open file connection, splits the string
#' into substrings by whitespace, validates for numerical strings, and
#' then converts to numerical vector.
#'
#' @template inp_con
#'
#' @keywords internal
#'
read_inp_numeric_line <- function(inp_con) {

  if(!isOpen(inp_con)){
    stop("No open file Connection to AGEPRO input file")
  }

  inp_line <-
    unlist(strsplit(readLines(inp_con, n = 1, warn = FALSE), " +"))

  return(assert_numeric_substrings(inp_line))
}



#' Command-line interface header for AGEPRO Keyword parameters
#'
#' Creates an custom header with double-lines (colored in cyan by default), via
#' cli library.
#'
#' @param keyword Text of the header
#' @param header_color Text Color, R color, or HTML hexidecimal color.
#'
#' @keywords internal
#'
cli_keyword_heading <- function(keyword, heading_color = "cyan") {
  d <- cli_div(theme = list(rule = list(
    color = heading_color,
    "line-type" = "double")))
  cli_rule(keyword)
  cli_end(d)
}



#' Open file dialog or interface to interactively return file path.
#'
#' If Rconsole is currently running in Rstudio, it will use the rstudioapi
#' to show the file dialog window over the IDE.
#'
#' @keywords internal
#'
open_file_dialog <- function(){

  #Open file dialog
  #Use rstudioapi to show file dialog in front of RStudio.
  filename <- ifelse(rstudioapi::isAvailable(),
                    rstudioapi::selectFile(),
                    file.choose(new=TRUE))


  return(path.expand(filename))
}
