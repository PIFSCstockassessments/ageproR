
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
#' @template stochastic_years_ages
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
