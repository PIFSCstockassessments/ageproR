

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

    #' @field biomass_scale
    #' Output units of biomass expressed in thousand metric tons
    biomass_scale = function(value){
      if(missing(value)){
        return(private$.biomass_scale)
      }
    },

    #' @field recruitment_scale
    #' Output units of recruitment numbers
    recruitment_scale = function(value){
      if(missing(value)){
        return(private$.recruitment_scale)
      }
    },

    #' @field stock_size_scale
    #' Output Units of stock size numbers
    stock_size_scale = function(value){
      if(missing(value)){
        return(private$.stock_size_scale)
      }
    },


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

    .keyword_name = "scale",

    .biomass_scale = NULL,
    .recruitment_scale = NULL,
    .stock_size_scale = NULL


  )
)
