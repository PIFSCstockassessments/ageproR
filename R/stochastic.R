

#' @title Stochastic AGEPRO Keyword Parameter Structure.
#'
#' @description
#' Generalized Class Structure for Stochastic AGEPRO Keyword parameters.
#'
#' @param num_obs_yrs Numbers of years in from first to last year of
#' projection.
#' @param num_ages Number of Age classes
#' @param num_fleets Number of Fleets. Default is 1
#' @param input_option Option to indicate stochastic parameter will be:
#' \itemize{
#'  \item `0` By default, interactively via interface.
#'  \item `1` Imported from a the location of an existing data file
#' }
#' @param time_flag Logical flag that enables the stochastic parameter to use
#' as a time-varying array if TRUE (or 1). Otherwise, FALSE the vector will
#' cover "all years" of the projection. Default is TRUE.
#'
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
stochastic <- R6Class(
  "stochastic",
  private = list(

    .count_observation_years = NULL,
    .count_ages = NULL,
    .input_option = NULL,
    .time_varying = NULL,
    .stochastic_datafile = NULL,
    .stochastic_table = NULL,
    .cv_table = NULL,

    .valid_input_options = c(0,1)



  ), public = list (

    #' @description
    #' Initializes the stochastic class
    #'
    initialize = function(num_obs_yrs,
                          num_ages,
                          num_fleets = 1,
                          input_option = 0,
                          time_flag = TRUE){

      #initialize tables
      private$.stochastic_table <- vector("list", 1)
      private$.cv_table <- vector("list", 1)

      self$input_option <- input_option

      if(self$time_varying){

        self$stochastastic_table <-
          self$create_stochastic_table(num_obs_yrs, num_ages, num_fleets)

        self$cv_table <-
          self$create_stochastic_table(num_obs_yrs, num_ages, num_fleets)
      }else{
        #All Years
        self$stochastic_table <-
          self$create_stochastic_table(1, num_ages, num_fleets)
        self$cv_table <- self$create_stochastic_table(1, num_ages, num_fleets)


      }



    },

    #' @description
    #' Creates an stochastic table
    #'
    create_stochastic_table = function(num_obs_yrs,
                                      num_ages,
                                      num_fleets = 1) {


      fleet_yrs_rows <- num_obs_yrs * num_fleets
      ages_cols <- num_ages

      return(matrix(rep(NA, (fleet_yrs_rows * ages_cols) ) ,
                    nrow = fleet_yrs_rows,
                    ncol = ages_cols))

    }







  ), active = list (

    #' @field input_option Stochastic Input option
    input_option = function(value) {
      if(missing(value)){
        private$.input_option
      } else {
        checkmate::assert_integer(value, lower = 0)
        checkmate::assert_subset(value, private$.valid_input_options)
        private$.input_option <- value
      }
    },

    #' @field time_varying Logical flag to list stochastic data by observation
    #' year
    time_varying = function(value) {
      if(missing(value)){
        private$.time_varying
      } else {
        checkmate::assert_logical(value)
        private$.time_varying <- value
      }
    },

    #' @field stochastic_table This is the logic for the average stochastic
    #' AGEPRO keyword parameter's at age (and by fleet if fleets are a
    #' factor).
    stochastic_table = function(value) {
      if(missing(value)){
        private$.stochastic_table
      } else {
        checkmate::assert_matrix(value, min.cols = 1, min.rows = 1)
        private$.stochastic_table <- value
      }
    },

    #' @field cv_table Matrix containing the vector of of age-specific CVs for
    #' sampling the average stochastic AGEPRO keyword parameter's at age
    #' (and by fleet if fleets are a factor) with lognormal process error.
    cv_table = function(value) {
      if(missing(value)) {
        private$.cv_table
      } else {
        checkmate::assert_matrix(value, min.cols = 1, min.rows = 1)
        private$.cv_table <- value
      }
    }


  )

)
