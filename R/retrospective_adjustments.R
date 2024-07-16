
#' @title
#' Vector of retrospective bias-correction coefficients to adjust
#' to the initial population of numbers of age.
#'
#' @description
#' This is the vector of age-specific numbers at age multipliers for an
#' initial population size at age vector if retrospective bias adjustment
#' is applied.
#'
#' The logical flag `enable_retrospective_adjustments` allows the user to
#' set values to this class fields. The flag will also notify `agepro_model`
#' if this keyword parameter is allowed to be written to input file.
#'
#' If this class is initialized with default values, it is presumed that this
#' keyword parameter is not used in the agepro_model. Therefore,
#' `enable_retrospective_adjustments` is flagged as FALSE. Valid non-default
#' values will set this flag to TRUE.
#'
#' @details
#' The retrospective_adjustments class (`RETROADJUST`) is recognized as a keyword
#' parameter used in the AGEPRO input file format, but it is optional.
#'
#' @include optional_options_flags.R
#'
#' @export
#' @importFrom R6 R6Class
#'
retrospective_adjustments <- R6Class(
  "retrospective_adjustments",
  public = list(

  ),
  active = list(

  ),
  private = list(

  )
)
