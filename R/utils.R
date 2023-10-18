

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
