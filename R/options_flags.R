
#' @title
#' Class container that encapsulates logical flags to enable AGEPRO
#' user-defined options
#'
#' @description
#' Encapsulates logical flags indicating that an optional AGEPRO's option can
#' be used: these options are percentile summary (PERC), scaling factors
#' (SCALE), biological reference points (REFPOINT), maximum bounds (BOUNDS), and
#' retrospective adjustment (RETROADJUST).
#'
#' @details
#' Associated with AGEPRO's output options (OPTIONS) are additional optional
#' options:
#'
#' * `PERC` :  percentile_summary
#' * `REFPOINT` :  reference_points
#' * `SCALE` :  scaling_factors
#' * `BOUNDS` :  max_bounds
#' * `RETROADJUST` :  retrospective_adjustment
#'
#' The AGEPRO input file format recognizes these optional keyword parameters.
#' At initialization, all option flags will be set to FALSE. To "enable" an
#' optional option or set it to TRUE, assign a value to the optional option's
#' field. For example, if the flag to enable for percentile summary
#' is FALSE: set value `report_percentile`,
#' Then it will be TRUE
#'
#'
options_flag_container <- R6Class(
  "options_flag_container",
  public = list(

    #' @field enable_percentile_summary
    #' Enables output summary report of specific Percentile
    enable_percentile_summary = NULL,

    #' @field enable_reference_points
    #' Enables biological reference points threshold report
    enable_reference_points = NULL,

    #' @field enable_scaling_factors
    #' Enables Scaling Factors
    enable_scaling_factors = NULL,

    #' @field enable_max_bounds
    #' Sets maximum bounds of Weight(MT) and natural mortality
    enable_max_bounds = NULL,

    #' @field enable_retrospective_adjustments
    #' Allows use of Retrospective Adjustment Factors by Age
    enable_retrospective_adjustments = NULL
  )
)

#' @rdname options_flag_container
#'
#' @export
#'
options_flags <- R6Class(
  "options_flags",
  public = list(

    #' @field op
    #' Class container that encapsulates logical flags to enable AGEPRO
    #' user-defined options
    op = options_flag_container$new()
  )
)

