
#' @title
#' Harvest intensity (of fishing mortality or landings quota) by fleet
#'
#' @description
#' Class Structure containing the Harvest values and Harvest Specifications
#'
#'
#' @param proj_years [Projection years][ageproR::projection_years]:
#' Input can be Sequence of years in from first to last year of
#' projection or the number of years in the time projection.
#' @param num_fleets Number of Fleets. Default is 1
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @export
#'
harvest_scenario <- R6Class(
  "harvest_scenario",
  private = list(

    .keyword_name = "harvest",

    .harvest_specification = NULL,
    .harvest_value = NULL,

    #setup variables at initialization
    .count_projection_years = NULL,
    .num_fleets = NULL

  ),
  public = list(

    #' @description
    #' Initializes Class
    #'
    #' @param count_projection_years Count or length of
    #' [Projection years][ageproR::projection_years]
    #' @param num_fleets Number of Fleets. Defaults to 1
    #'
    initialize = function(count_projection_years,
                          num_fleets = 1) {

      #Validate count_projection_years

      #Initialize private variables
      private$.count_projection_years <- count_projection_years
      private$.num_fleets <- num_fleets

      #harvest_specification

      if(private$.num_fleets == 1) {
        #harvest_value ("Harvest Value")

      }else{
        #harvest_value (Multi-fleet "FLEET-")

      }
      #cbind harvest_specification and harvest_value


    }

  ),
  active = list(

    #' @field harvest_specification
    #' Contains values the Harvest Specification per projection year
    #' \itemize{
    #'  \item{"0"}{F-MULT}
    #'  \item{"1"}{LANDINGS}
    #'  \item{"2"}{REMOVALS}
    #' }
    #'
    harvest_specification = function(value){
      if(missing(value)){
        private$.harvest_specification
      }else{
        checkmate::assert_matrix(value, ncols = 1, min.rows = 1,
                                 .var.name = "harvest_specification")
        #TODO: Validation
        private$.harvest_specification <- value
      }
    },

    #' @field harvest_value
    #' Contains the harvest Amount per projection year. Can be fleet specific
    #' if more than 2 fleets are specified.
    harvest_value = function(value) {
      if(missing(value)){
        private$.harvest_value
      }else{
        checkmate::assert_matrix(value, min.cols = 1, min.rows = 1,
                                 .var.name = "harvest_val")
        private$.harvest_value <- value
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
  )
)
