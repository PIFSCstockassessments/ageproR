

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
div_keyword_header <- function(keyword, heading_color = "cyan") {
  d <- cli_div(theme = list(rule = list(
    color = heading_color,
    "line-type" = "double")))
  cli_rule(keyword)
  cli_end(d)
}


#' Print AGEPRO table-like values to the console
#'
#' Helper function to print out AGEPRO keyword parameter's table-like matrix or
#' vector variables to console. Includes an option to show the first few rows
#' to the console.
#'
#' @param tbl AGEPRO Keyword Parameter matrix/vector variable
#' @param omit_rows Logical flag, if `TRUE`, will print the first six rows,
#' via [`head()`][utils::head], to R console. In addition, the total number
#' of rows and rows omitted will be displayed. If `FALSE`, by default,
#' the matrix or vector prints normally.
#'
#' @importFrom utils head
#'
print_parameter_table = function (tbl, omit_rows=FALSE) {

  if(omit_rows) {

    omitted_num_rows <- pmax(0, nrow(tbl)-6)

    cli::cat_print(head(tbl)) #first 6 rows
    cli::cli_text(
      paste0("{symbol$info} ","Total of {nrow(tbl)} row{?s}; ",
             "{no(omitted_num_rows)} row{?s} omitted"))
  }else{
    cli::cat_print(tbl)
  }

}


#' Creates a table-like matrix with `NA` values
#'
#' Wrapper to [`Matrix`][base::matrix] function, that returns the object with
#' `NA` values. Uses the matrix dimnames argument to set row and column names.
#' See [`Matrix`][base::matrix] for more information.
#'
#' @param num_rows the desired number of rows
#' @param num_cols the desired number of columns
#' @param dimnames Matrix `dimnames`. See [`Matrix`][base::matrix] argument
#' for more detail.
#'
create_blank_parameter_table = function(num_rows, num_cols,
                                        dimnames = NULL) {

  return(matrix(rep(NA, (num_rows * num_cols) ) ,
                nrow = num_rows,
                ncol = num_cols,
                dimnames = dimnames))

}


#' @title
#' Validates the usage of the 'projection years' parameter.
#'
#' @description
#' If `proj_years` parameter is a
#' [projection_years class][ageproR::projection_years], then it will return
#' that value. Otherwise, it will create a new `projection_years` class based
#' on the param value passed.
#'
#' @param proj_years Projection year parameter. May be a numeric vector or a
#' [`projection_years`][ageproR::projection_years]
#'
validate_proj_years_parameter <- function (proj_years) {

  #Validate parameters
  if (checkmate::test_r6(proj_years, public = c("count","sequence") )) {
    proj_years_class <- proj_years
  } else {
    proj_years_class <- ageproR::projection_years$new(proj_years)
  }

  return(proj_years_class)
}

