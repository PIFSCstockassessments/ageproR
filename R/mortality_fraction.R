

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
mortality_fraction_prior_spawn <- R6Class(
  "mortality_fraction_prior_spawn",
  private = list(

    .keyword_name = "biological",

    .projection_years = NULL,
    .time_varying = NULL,
    .natural_mortality_prior_spawn = NULL,
    .fishing_mortality_prior_spawn = NULL,

    # Handle proj_years that may be a single int or sequential numeric vector
    set_projection_years = function(value){

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
    set_fraction_mortality_matrix = function(time_varying,
                                             row_names = NULL) {

      default_proportion <- 0.5

      if(time_varying){
        return(
          matrix(rep(default_proportion/private$.projection_years$count,
                     private$.projection_years$count),
                 nrow = 1,
                 ncol = private$.projection_years$count,
                 dimnames = list(row_names,
                                 private$.projection_years$sequence))
        )

      }else{
        return(
          matrix(default_proportion,
                 nrow = 1,
                 ncol = 1,
                 dimnames = list(row_names, "All Years"))
        )

      }

    }


  ),
  public = list(


    #' @description
    #' Initializes the class
    #'
    initialize = function(proj_years_vector, time_varying = FALSE) {

      checkmate::assert_logical(time_varying,
                                any.missing = FALSE, all.missing=FALSE,
                                len = 1)

      #setup
      private$set_projection_years(proj_years_vector)
      private$.time_varying <- time_varying

      private$.natural_mortality_prior_spawn <-
        private$set_fraction_mortality_matrix(private$.time_varying,
                                              row_names = "natural_mortality_prior_spawn")

      private$.fishing_mortality_prior_spawn <-
        private$set_fraction_mortality_matrix(private$.time_varying,
                                              row_names = "fishing_mortality_prior_spawn")

    }

  ),
  active = list(

    #' @field time_varying
    #' [Logical][base::logical] flag to list fishing and natural mortality per
    #' observation year if TRUE or representative of the
    time_varying = function(value) {
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      private$.time_varying
    },

    #' @field proportion_total_mortality
    #' Proportion of total mortality occurring prior to spawning
    proportion_total_mortality = function(value) {
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      rbind(private$.natural_mortality_prior_spawn,
            private$.fishing_mortality_prior_spawn)
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
