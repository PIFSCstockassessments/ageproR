

#' @title Stochastic AGEPRO Keyword Parameter Structure.
#'
#' @description
#' Generalized Class Structure for Stochastic AGEPRO Keyword parameters.
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
    .cv_table = NULL

  ), public = list (

    #' @description
    #' Initializes the stochastic class
    #'
    initialize = function(num_obs_yrs, num_ages, num_fleets = 1){



    }
  ), active = list (

    #' @field input_option Stochastic Input option
    input_option = function(value) {
      if(missing(value)){
        private$.input_option
      } else {
        checkmate::assert_logical(value)
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

    #' @field stochastic_table Stochastic Table
    stocastic_table = function(value) {
      if(missing(value)){
        private$.stochastic_table
      } else {
        checkmate::assert_matrix(value, min.cols = 1, min.rows = 1)
        private$.stochastic_table <- value
      }
    }


  )

)
