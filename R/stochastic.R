

#' @title Stochastic AGEPRO Keyword Parameter Structure.
#'
#' @description
#' Generalized Class Structure for Stochastic AGEPRO Keyword parameters.
#'
#' @param num_projection_years Numbers of years in from first to last year of
#' projection.
#' @param num_ages Number of Age classes
#' @param num_fleets Number of Fleets. Default is 1
#' @param input_option Option to indicate stochastic parameter will be:
#' \itemize{
#'  \item `0` By default, interactively via interface.
#'  \item `1` Imported from a the location of an existing data file
#' }
#' @param time_varying Logical flag that enables the stochastic parameter to use
#' as a time-varying array if TRUE (or 1). Otherwise, FALSE the vector will
#' cover "all years" of the projection. Default is TRUE.
#'
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @export
stochastic <- R6Class(
  "stochastic",
  private = list(

    .input_option = NULL,
    .time_varying = NULL,
    .stochastic_datafile = NULL,
    .stochastic_table = NULL,
    .cv_table = NULL,
    .upper_bounds = NULL,

    .valid_input_options = c(0,1),
    .parameter_name = NULL,
    .discards = NULL,

    #Private method to initialize Stochastic and CV tables
    setup_stochastic_tables = function (num_projection_years,
                                        num_ages,
                                        num_fleets = 1) {
      #initialize tables
      private$.stochastic_table <- vector("list", 1)
      private$.cv_table <- vector("list", 1)

      if(self$time_varying){

        self$stochastic_table <- self$create_stochastic_table(
          (num_projection_years * num_fleets), num_ages)

        self$cv_table <- self$create_stochastic_table(
          (num_projection_years * num_fleets), num_ages)

      }else{
        #All Years
        self$stochastic_table <-
          self$create_stochastic_table((1 * num_fleets), num_ages)
        self$cv_table <-
          self$create_stochastic_table((1 * num_fleets), num_ages)

      }

    }

  ), public = list (

    #' @description
    #' Initializes the stochastic class
    #'
    initialize = function(num_projection_years,
                          num_ages,
                          num_fleets = 1,
                          input_option = 0,
                          time_varying = TRUE){

      #set and validate input_option value
      self$input_option <- input_option

      #Time Varying
      self$time_varying <- time_varying

      #Initialize Stochastic and CV tables
      private$setup_stochastic_tables(num_projection_years,
                                      num_ages,
                                      num_fleets)


    },

    #' @description
    #' Creates an stochastic table
    #'
    #' @param fleet_yr_rows (Fleet-)Year Row
    #' @param ages_cols Age Columns
    #'
    create_stochastic_table = function(fleet_yr_rows, ages_cols) {


      return(matrix(rep(NA, (fleet_yr_rows * ages_cols) ) ,
                    nrow = fleet_yr_rows,
                    ncol = ages_cols))

    }

    #TODO: print function

    #TODO: read_inp_lines


    #TODO: inplines_stochastic

  ), active = list (

    #' @field input_option Stochastic Input option
    input_option = function(value) {
      if(missing(value)){
        private$.input_option
      } else {
        checkmate::assert_integerish(value, lower = 0)
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
        #TODO: Create stochastic_table value Validation for upper_bound
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

    #TODO: json_list_stochastic

  )

)
