

#' @title Scale Factors
#'
#' @description
#' Input information on scaling factors for biomass, recruitment, and stock size
#'
#' @include optional_options_flags.R
#'
#' @export
#' @importFrom R6 R6Class
#'
scale_factors <-R6Class(
  "scale_factors",
  public = list(

  ),
  active = list(

    #' @field keyword_name
    #' AGEPRO keyword parameter name
    keyword_name = function() {
      private$.keyword_name
    },

    #' @field inp_keyword
    #' Returns AGEPRO input-file formatted Parameter
    inp_keyword = function() {
      paste0("[",toupper(private$.keyword_name),"]")
    }

  ),
  private = list(

    .keyword_name = "scale"


  )
)
