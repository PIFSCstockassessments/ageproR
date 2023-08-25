
#' Input information for the natural mortality rate (M) at Age
#'
#' @description
#' Generalized Class Structure for Natural Mortality rate of age
#' AGEPRO Keyword parameter.
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @export
natural_mortality <- R6Class(
  "natural_mortality",
  inherit = ageproR::stochastic,
  private = list(

  ),
  public = list(

    #' @description
    #' Initializes the stochastic class
    #'
    #' @param num_projection_years Numbers of years in from first to last
    #' year of projection.
    #' @param num_ages Number of Age classes
    #' @param input_option Option to indicate Natural Mortality at age will be:
    #' \itemize{
    #'  \item `0` By default, done interactively via interface.
    #'  \item `1` Imported from a the location of an existing data file
    #' }
    #' @param time_varying Logical flag that enables  Natural Mortality at age
    #' to use as a time-varying array if TRUE (or 1). Otherwise, FALSE the
    #' vector will cover "all years" of the projection. Default is TRUE.
    #'
    #'
    initialize = function(num_projection_years,
                         num_ages,
                         input_option = 0,
                         time_varying = TRUE) {

      super$initialize(num_projection_years,
                      num_ages,
                      1, #Single, non-Fleet dependent parameter.
                      input_option,
                      time_varying)

      super$parameter_name <- "Natural mortality Of Age"
      private$.inp_keyword <- "[NATMORT]"


    }

  )

)
