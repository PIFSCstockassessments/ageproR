

#' @title Stochastic AGEPRO Keyword Parameter Structure.
#'
#' @description
#' Generalized Class Structure for Stochastic AGEPRO Keyword parameters.
#'
#' @param proj_years [Projection years][ageproR::projection_years]:
#' Input can be Sequence of years in from first to last year of
#' projection or the number of years in the time projection.
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
#' @template elipses
#' @template inp_con
#' @template nline
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
    .inp_keyword = NULL,

    .projection_years = NULL,
    .num_ages = NULL,
    .num_fleets = NULL,

    #Rownames: Fleet-Years
    setup_stochastic_rownames = function (proj_years_sequence,
                                          num_fleets = 1,
                                          time_varying = FALSE){
      #Validate num_fleets
      checkmate::check_integerish(num_fleets, lower = 1)

      if(num_fleets > 1) {
        if(time_varying) {
          # Assemble Fleet-years rownames vector by creating a sequence of
          # Fleet and projected_years sequence strings. For fleet-dependent
          # stochastic parameters, repeat each unique element of the fleet
          # sequence by the length of the time projection.
          rownames_fleetyears <-

            paste(paste0("Fleet", rep(seq(num_fleets),
                                each = length(proj_years_sequence))),
                  proj_years_sequence, sep = "-")


        }else {
          rownames_fleetyears <- paste0("Fleet",seq(num_fleets))
        }

      } else {
        # If num_fleets is 1 && use the projection_years sequence as rownames,
        # Otherwise use the "All years" rowname
        if(time_varying){
          rownames_fleetyears <- proj_years_sequence
        }else{
          rownames_fleetyears <- "All Years"
        }
      }

      return(rownames_fleetyears)

    }

  ), public = list (

    #' @description
    #' Initializes the stochastic class
    #'
    initialize = function(proj_years,
                          num_ages,
                          num_fleets = 1,
                          input_option = 0,
                          time_varying = TRUE){

      #set and validate input_option value
      self$input_option <- input_option

      #Time Varying
      self$time_varying <- time_varying

      # Handle num_projection_years that may be a single int
      # or vector of sequential values
      proj_years_class <- ageproR::projection_years$new(as.numeric(proj_years))

      #Initialize Stochastic and CV tables
      self$setup_stochastic_tables(proj_years_class,
                                   num_ages,
                                   num_fleets,
                                   time_varying = self$time_varying)

      #Fallback Parameter Name
      self$parameter_name <- "Stochastic Parameter At Age"
      private$.inp_keyword <- "[STOCHASTIC]"

    },

    #' @description
    #' Initialize Stochastic and CV tables
    #'
    #' @param projection_years [Projection years][ageproR::projection_years]
    #' value
    #' @param num_ages Number of Ages
    #' @param num_fleets Number of Fleets. Defaults to 1
    #'
    setup_stochastic_tables = function (projection_years,
                                        num_ages,
                                        num_fleets = 1,
                                        time_varying = FALSE) {

      #Initialize private values
      private$.projection_years <- projection_years
      private$.num_ages <- num_ages
      private$.num_fleets <- num_fleets

      #Validate parameters
      checkmate::assert_numeric(projection_years$count, lower = 1)
      checkmate::assert_integerish(num_ages, lower = 1)
      checkmate::assert_integerish(num_fleets, lower = 1)

      #initialize tables
      private$.stochastic_table <- vector("list", 1)
      private$.cv_table <- vector("list", 1)

      if(time_varying){

        self$stochastic_table <- self$create_stochastic_table(
          (projection_years$count * num_fleets), num_ages)

      }else{
        #All Years
        self$stochastic_table <-
          self$create_stochastic_table((1 * num_fleets), num_ages)
      }

      self$cv_table <-
        self$create_stochastic_table((1 * num_fleets), num_ages)


      #Rownames: Fleet-Years
      # Fleet-year rownames for Stochastic Parameter of Age table
      rownames(self$stochastic_table)  <-
        private$setup_stochastic_rownames(projection_years$sequence,
                                          num_fleets,
                                          self$time_varying)

      # Fleet-year rownames for CV. Not affected by time varying
      rownames(self$cv_table) <-
        private$setup_stochastic_rownames(projection_years$sequence,
                                          num_fleets)

      #Colnames: Ages
      colnames_ages <- paste0("Age", seq(num_ages))
      colnames(self$stochastic_table) <- colnames_ages
      colnames(self$cv_table) <- colnames_ages

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

    },

    #' @description
    #' Formatted print out Stochastic Parameter Values
    #'
    print = function(...) {
      cli::cli_ul()
      cli::cli_li("Input Option: {.val {self$input_option}}")
      cli::cli_li("Time Varying: {.val {self$time_varying}}")
      cli::cli_alert_info("{self$parameter_name}")
      cli::cat_print(self$stochastic_table)
      cli::cli_alert_info("Coefficient of Variation")
      cli::cat_print(self$cv_table)
      cli::cli_end()

    },

    #' @description
    #' Reads in stochastic parameter's from AGEPRO Input file
    #'
    read_inp_lines = function(inp_con, nline, proj_years, num_ages, num_fleets = 1) {

      # Read an additional line from the file connection
      # and split into 2 substrings
      inp_line <- read_inp_numeric_line(inp_con)

      #Assign input option and time varying
      self$input_option <- inp_line[1]
      self$time_varying <- as.logical(inp_line[2])

      #Validate input option
      checkmate::assert_choice(self$input_option,
                               private$.valid_input_options,
                               .var.name = "Input Option")

      nline <- nline + 1
      cli::cli_alert("Line {nline} :")
      cli::cli_ul()
      a <- cli::cli_ul()
      cli::cli_li("Input Option: {.val {self$input_option}}")
      cli::cli_li("Time Varying: {.val {self$time_varying}}")
      cli::cli_end(a)
      cli::cli_end()


      # Setup new instance of stochastic of age and CV tables time_varying
      # value read from the AGEPRO input file. Including values for
      # projection_years. num_ages, and num_fleets.
      self$setup_stochastic_tables(ageproR::projection_years$new(proj_years),
                                   num_ages,
                                   num_fleets,
                                   time_varying = self$time_varying)


      if(self$input_option == 1) {
        #TODO: Read from file name
        stop("NOT IMPLMENTED")
      } else {
        #from interface
        nline <- self$read_inplines_stochastic_tables(inp_con, nline)
      }

      return(nline)
    },


    #' @description
    #' Internal helper function to set stochastic tables from
    #' AGEPRO input files.
    #'
    read_inplines_stochastic_tables = function(inp_con, nline) {

      cli::cli_alert_info("Number of Ages: {.val {private$.num_ages}}")
      #TODO: Verify inp_line is same length as num_ages
      if(self$time_varying){

        for(i in rownames(self$stochastic_table)){
          inp_line <- read_inp_numeric_line(inp_con)
          nline <- nline + 1
          cli_alert(c("Line {nline}: {self$parameter_name} for {i}: ",
                      "{.val {inp_line}}"))

          self$stochastic_table[i,] <- inp_line
        }


      }else{
        # Read in only one additional line from the file connection
        inp_line <- read_inp_numeric_line(inp_con)
        nline <- nline + 1
        cli_alert(c("Line {nline}: {self$parameter_name} for All Years: ",
                    "{.val {inp_line}}"))
        self$stochastic_table["All Years",] <- inp_line


      }

      # Read in only one additional line from the file connection
      inp_line <- read_inp_numeric_line(inp_con)
      nline <- nline + 1
      cli_alert(c("Line {nline}: Coefficent of Variation for All Years: ",
                  "{.val {inp_line}}"))
      self$cv_table["All Years",] <- inp_line

      return(nline)
    }







  ), active = list (

    #' @field input_option Stochastic Input option
    input_option = function(input_flag) {
      if(missing(input_flag)){
        private$.input_option
      } else {
        checkmate::assert_integerish(input_flag, lower = 0)
        checkmate::assert_subset(input_flag, private$.valid_input_options)
        private$.input_option <- input_flag
      }
    },

    #' @field time_varying Logical flag to list stochastic data by observation
    #' year
    time_varying = function(time_flag) {
      if(missing(time_flag)){
        private$.time_varying
      } else {
        checkmate::assert_logical(time_flag)
        private$.time_varying <- time_flag
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
    },

    #' @field parameter_name Parameter Name
    parameter_name = function(value) {
      if(missing(value)){
        private$.parameter_name
      } else {
        checkmate::assert_character(value)
        private$.parameter_name <- value
      }
    },

    #' @field inp_keyword
    #' Returns AGEPRO input-file formatted Parameter name
    inp_keyword = function() {
      private$.inp_keyword
    },

    #' @field json_list_stochastic
    #' Returns JSON list object with Stochastic Parameter values
    json_list_stochastic = function(){
      return(list(
        flag = self$input_option,
        timeflag = self$time_varying,
        value = self$stochastic_table,
        error = self$cv_table
      ))
    }

  )

)
