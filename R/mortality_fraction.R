

#' @title
#' Proportion of total mortality occurring prior to spawn in year t
#'
#' @description
#' Class Structure that includes proportion seasonal timing for fishing mortality & natural mortality
#' prior to spawning season
#'
#' @template proj_years_vector
#'
#' @param time_varying
#' Logical flag that enables the stochastic parameter
#' to use as a time-varying array if TRUE (or 1). Otherwise, FALSE the
#' vector will cover "all years" of the projection. Default is FALSE
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @export
mortality_fraction_before_spawn <- R6Class(
  "mortality_fraction_before_spawning",
  private = list(

    .proj_years = NULL,
    .time_varying = NULL,
    .natural_mortality_before_spawn = NULL,
    .fishing_mortality_before_spawn = NULL,

    # Handle proj_years that may be a single int or sequential numeric vector
    setup_projection_years = function(value){

      # Handle instances where value is passed as projection_years class
      if(checkmate::test_r6(value, public = c("count","sequence"))){
        private$.projection_years <- value
        invisible(value)
      }

      checkmate::assert_numeric(value)
      private$.projection_years <-
        ageproR::projection_years$new(as.numeric(value))


    },


    # Creates matrix object vector for natural_mortality_before_spawn and
    # fishing_mortality_before_spawn
    setup_fraction_mortality_matrix = function(time_varying) {

      default_proportion <- 0.5

      if(time_varying){
        # Default values of fishing and natural mortality are co
        return(
          matrix(rep(default_proportion/private$.proj_years$count,
                     private$.proj_years$count),
                 nrow = 1,
                 ncol = private$.proj_years$count,
                 dimnames = list(NULL,
                                 private$.proj_years$sequence))
        )

      }else{
        return(
          matrix(default_proportion,
                 nrow = 1,
                 ncol = 1,
                 dimnames = list(NULL, "All Years"))
        )

      }

    }


  ),
  public = list(


    #' @description
    #' Initializes the class
    #'
    #'
    #'
    initialize = function(proj_years_vector, time_varying = FALSE) {

      checkmate::assert_logical(time_varying,
                                any.missing = FALSE, all.missing=FALSE,
                                len = 1)


      private$setup_projection_years(proj_years_vector)
      private$.time_varying <- time_varying

      #Setup
      private$.natural_mortality_before_spawn <-
        setup_fraction_mortality_matrix(private$.time_varying)

      private$.fishing_mortality_before_spawn <-
        setup_fraction_mortality_matrix(private$.time_varying)

    }



  )
)
