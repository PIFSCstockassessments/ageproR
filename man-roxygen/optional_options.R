#' @details
#' Associated with AGEPRO's output options (OPTIONS) are additional optional
#' options:
#'
#' \describe{
#'   \item{PERC}{percentile_summary}
#'   \item{REFPOINT}{reference_points}
#'   \item{SCALE}{scaling_factors}
#'   \item{BOUNDS}{max_bounds}
#'   \item{RETROADJUST}{retrospective_adjustment}
#' }
#'
#' The AGEPRO input file format recognizes these optional keyword parameters.
#' At initialization, all option flags will be set to FALSE. To "enable" an
#' optional option or set it to TRUE, assign a value to the optional option's
#' field. For example, if the flag to enable for percentile summary
#' is FALSE: set value `report_percentile`,
#' Then it will be TRUE
