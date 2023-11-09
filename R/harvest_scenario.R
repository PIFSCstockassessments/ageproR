
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
    .harvest_scenario_table = NULL,

    #setup variables at initialization
    .projection_years = NULL,
    .num_fleets = NULL

  ),
  public = list(

    #' @description
    #' Initializes Class
    #'
    #' @param projection_years [Projection years][ageproR::projection_years]:
    #' Input can be Sequence of years in from first to last year of
    #' projection or the number of years in the time projection.
    #' @param num_fleets Number of Fleets. Default is 1
    #'
    #'
    initialize = function(projection_years,
                          num_fleets = 1) {

      #Validate count_projection_years

      #Initialize private variables
      private$.projection_years <- projection_years
      private$.num_fleets <- num_fleets

      #harvest_specification
      self$harvest_specification <-
        matrix(rep(1, (private$.projection_years$count)),
               nrow = private$.projection_years$count,
               ncol = 1,
               dimnames = list(private$.projection_years$sequence,
                               "harvest_specificaton"))


      #Harvest Value
      #Check if Single or Multi Fleet
      if(isTRUE(identical(private$.num_fleets, 1))){
        harvest_value_colnames <- "harvest_value"
      }else {
        harvest_value_colnames <- paste0("FLEET-", 1:private$.num_fleets)
      }

      self$harvest_value <-
        matrix(rep(NA, (private$.projection_years$count)),
               nrow = private$.projection_years$count,
               ncol = private$.num_fleets,
               dimnames = list(private$.projection_years$sequence,
                               harvest_value_colnames))


      #cbind harvest_specification and harvest_value
      private$.harvest_scenario_table <- cbind(self$harvest_specification,
                                   self$harvest_value)

    },


    #' @description
    #' Formatted to print out the Harvest Scenario Table
    #'
    print = function(enable_cat_print = TRUE, ...){
      cli::cli_alert_info("Harvest Scenario")
      #Verbose flag check
      if(enable_cat_print){
        #Allow `cli::cat_print` message
        print_parameter_table(self$harvest_scenario_table, ...)
      }else {
        #Suppress `cli::cat_print` message
        capture.output(
          x <- print_parameter_table(self$harvest_scenario_table, ...))
      }
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
        checkmate::assert_matrix(value, mode = "numeric",
                                 min.cols = 1, min.rows = 1,
                                 .var.name = "harvest_value")
        private$.harvest_value <- value
      }
    },

    #' @field harvest_scenario_table
    #' Combines the Harvest specification and (fleet) harvest amount per
    #' projection year.
    harvest_scenario_table = function(value) {
      #TODO: Show Print harvest_specification column as
      #F-MULT, LANDINGS, REMOVALS

        private$.harvest_scenario_table

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
