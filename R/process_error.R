

#' @title
#' Process Errors for Population and Fishery Processes
#'
#' @description
#' Generalized Class Structure for AGEPRO Keyword parameters who have process
#' errors that generate time-varying dynamics of population and fishery
#' process.
#'
#' @param num_fleets Number of Fleets. Default is 1
#'
#' @template elipses
#' @template inp_con
#' @template nline
#' @template delimiter
#' @template process_error_initalize_params
#' @template enable_cat_print
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @export
process_error <- R6Class(
  "process_error",
  private = list(

    .input_option = NULL,
    .time_varying = NULL,
    .parameter_datafile = NULL,
    .parameter_table = NULL,
    .cv_table = NULL,
    .upper_bounds = NULL,

    .valid_input_options = c(0,1),
    .parameter_title = NULL,
    .inp_keyword = NULL,
    .discards_parameter = NULL,

    .projection_years = NULL,
    .num_ages = NULL,
    .num_fleets = NULL,

    #Rownames: Fleet-Years
    setup_parameter_table_rownames = function (proj_years_sequence,
                                          num_fleets = 1,
                                          time_varying = FALSE){
      #Validate num_fleets
      checkmate::check_integerish(num_fleets, lower = 1)

      if(num_fleets > 1) {
        if(time_varying) {
          # Assemble Fleet-years rownames vector by creating a sequence of
          # Fleet and projected_years sequence strings. For fleet-dependent
          # parameters, repeat each unique element of the fleet
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

    },

    #Handles potential proj_years "Factor" types, and returns its
    #"levels", the intended values assigned to this value.
    handle_factors_proj_years = function(proj_years){

      #Check proj_years
      if(is.factor(proj_years)) {
        proj_years <- levels(proj_years)
      }

      return(proj_years)
    },

    #Change in time_varying will reset parameter and CV table
    time_varying_toggle_resets_parameter_table = function(time_flag) {

      if(is.null(private$.time_varying)){
        return()
      }

      if(time_flag != private$.time_varying){

        self$setup_parameter_tables(private$.projection_years,
                                      private$.num_ages,
                                      private$.num_fleets,
                                      time_varying = time_flag)
      }
      return()
    }


  ), public = list (

    #' @description
    #' Initializes the class
    #'
    initialize = function(proj_years,
                          num_ages,
                          num_fleets = 1,
                          input_option = 0,
                          time_varying = TRUE,
                          ...){

      #set and validate input_option value
      self$input_option <- input_option

      #Time Varying
      self$time_varying <- time_varying

      proj_years <- private$handle_factors_proj_years(proj_years)

      # Handle num_projection_years that may be a single int
      # or vector of sequential values
      proj_years_class <- ageproR::projection_years$new(as.numeric(proj_years))

      #Initialize parameter and CV tables
      self$setup_parameter_tables(proj_years_class,
                                   num_ages,
                                   num_fleets,
                                   time_varying = self$time_varying)

      #Fallback Parameter Name
      self$parameter_title <- "Process Error Parameter At Age"
      private$.inp_keyword <- "[PROCESS_ERROR]"
      private$.discards_parameter <- FALSE

    },

    #' @description
    #' Initialize Parameter and CV tables
    #'
    #' @param projection_years [Projection years][ageproR::projection_years]
    #' value
    #' @param num_ages Number of Ages
    #' @param num_fleets Number of Fleets. Defaults to 1
    #'
    setup_parameter_tables = function (projection_years,
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
      private$.parameter_table <- vector("list", 1)
      private$.cv_table <- vector("list", 1)

      if(time_varying){

        self$parameter_table <- self$create_parameter_table(
          (projection_years$count * num_fleets), num_ages)

      }else{
        #All Years
        self$parameter_table <-
          self$create_parameter_table((1 * num_fleets), num_ages)
      }

      self$cv_table <-
        self$create_parameter_table((1 * num_fleets), num_ages)


      #Rownames: Fleet-Years
      # Fleet-year rownames for Parameter of Age table
      rownames(self$parameter_table)  <-
        private$setup_parameter_table_rownames(projection_years$sequence,
                                               num_fleets,
                                               time_varying)

      # Fleet-year rownames for CV. Not affected by time varying
      rownames(self$cv_table) <-
        private$setup_parameter_table_rownames(projection_years$sequence,
                                               num_fleets)

      #Colnames: Ages
      colnames_ages <- paste0("Age", seq(num_ages))
      colnames(self$parameter_table) <- colnames_ages
      colnames(self$cv_table) <- colnames_ages

    },



    #' @description
    #' Creates an Population or Fishery process Parameter table
    #'
    #' @param fleet_yr_rows (Fleet-)Year Row
    #' @param ages_cols Age Columns
    #'
    create_parameter_table = function(fleet_yr_rows, ages_cols) {

      return(matrix(rep(NA, (fleet_yr_rows * ages_cols) ) ,
                    nrow = fleet_yr_rows,
                    ncol = ages_cols))

    },

    #' @description
    #' Formatted to print out the values pf the Process Error Parameter
    #'
    print = function(enable_cat_print = TRUE, ...) {
      #TODO: Option to hide or limit rows of parameter & CV table
      cli::cli_ul()
      cli::cli_li("input_option: {.val {self$input_option}}")
      cli::cli_li("time_varying: {.val {self$time_varying}}")
      cli::cli_end()

      cli::cli_par()
      cli::cli_alert_info("parameter_table: {self$parameter_title}")
      #Verbose flag check
      if(enable_cat_print){
        #Allow `cli::cat_print` message
        self$cli_print_process_error_table(self$parameter_table, ...)
      }else {
        #Suppress `cli::cat_print` message
        capture.output( x <- self$cli_print_process_error_table(
          self$parameter_table, ...))
      }
      cli::cli_end()

      cli::cli_par()
      cli::cli_alert_info("cv_table: Coefficient of Variation")
      if(enable_cat_print) {
        #Allow `cli::cat_print` message
        self$cli_print_process_error_table(self$cv_table, ...)
      }else {
        #Suppress `cli::cat_print` message
        capture.output( x <- self$cli_print_process_error_table(
          self$cv_table, ...))
      }


      cli::cli_end()

    },


    #' @description
    #' Helper function to print out Process Error keyword parameter data to
    #' Console.
    #'
    #' @param tbl Process Error Parameter or cv Table
    #' @param omit_rows Logical flag, if `TRUE`, will print the first rows of
    #' the process error table, via [`head()`][utils::head], to R console. In
    #' addition total number of rows and rows omitted will be displayed. By
    #' default is set to `FALSE`, prints normally.
    #'
    cli_print_process_error_table = function (tbl, omit_rows=FALSE) {

      if(omit_rows) {

        omitted_num_rows <- pmax(0, nrow(tbl)-6)

        cli::cat_print(head(tbl)) #first 6 roww
        cli::cli_text(
          paste0("{symbol$info} ","Total of {nrow(tbl)} row{?s}; ",
                 "{no(omitted_num_rows)} row{?s} omitted"))
      }else{
        cli::cat_print(tbl)
      }

    },


    #' @description
    #' Reads in Process Error keyword parameter's values from AGEPRO Input file
    #'
    read_inp_lines = function(inp_con,
                              nline,
                              proj_years,
                              num_ages,
                              num_fleets = 1) {

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
      cli::cli_li("input_option: {.val {self$input_option}}")
      cli::cli_li("time_varying: {.val {self$time_varying}}")
      cli::cli_end(a)
      cli::cli_end()


      # Setup new instance of Parameter and CV tables. time_varying
      # value read from the AGEPRO input file. Including values for
      # projection_years. num_ages, and num_fleets.
      self$setup_parameter_tables(ageproR::projection_years$new(proj_years),
                                   num_ages,
                                   num_fleets,
                                   time_varying = self$time_varying)


      if(self$input_option == 1) {
        #TODO: Read from file name
        stop("NOT IMPLMENTED")
      } else {
        #from interface
        nline <- self$read_inplines_parameter_tables(inp_con, nline)
        nline <- self$read_inplines_cv_table(inp_con, nline)
      }

      return(nline)
    },




    #' @description
    #' Helper function to set population or fishery process parameter
    #' tables from AGEPRO input files. Reads in an additional line (or lines)
    #' from the file connection to assign to the `parameter_table`
    #'
    read_inplines_parameter_tables = function(inp_con, nline) {

      #TODO: Verify inp_line is same length as num_ages

      #Non-time varying, single fleet data
      if(private$.num_fleets == 1 && !(self$time_varying)) {


        inp_line <- read_inp_numeric_line(inp_con)
        nline <- nline + 1
        cli_alert(c("Line {nline}: ", "parameter_table (",
                    "{self$parameter_title}) for All Years: ",
                    "{.val {inp_line}} ",
                    "{.emph ({private$.num_ages} Age{?s})}"))

        self$parameter_table["All Years",] <- inp_line

      #Multi-fleet or Single fleet w/ time varying
      }else {

        for(i in rownames(self$parameter_table)){
          inp_line <- read_inp_numeric_line(inp_con)
          nline <- nline + 1
          cli_alert(c("Line {nline}: ", "parameter_table(",
                      "{self$parameter_title} for {i}: ",
                      "{.val {inp_line}} ",
                      "{.emph ({private$.num_ages} Age{?s})}"))

          self$parameter_table[i,] <- inp_line
        }
      }


      return(nline)
    },

    #' @description
    #' Internal helper function to set cv tables from
    #' AGEPRO input files. Reads in an additional line (or lines) from the
    #' file connection to assign to the `cv_table`
    #'
    read_inplines_cv_table = function(inp_con, nline) {

      if(private$.num_fleets == 1) {
        inp_line <- read_inp_numeric_line(inp_con)
        nline <- nline + 1
        cli_alert(c("Line {nline}: ",
                    "cv_table (Coefficent of Variation) for All Years: ",
                    "{.val {inp_line}} ",
                    "{.emph ({private$.num_ages} Age{?s})}"))
        self$cv_table["All Years",] <- inp_line

      } else {

        for(i in rownames(self$cv_table)){
          inp_line <- read_inp_numeric_line(inp_con)
          nline <- nline + 1
          cli_alert(c("Line {nline}: ",
                      "cv_table (Coefficent of Variation) for {i}: ",
                      "{.val {inp_line}} ",
                      "{.emph ({private$.num_ages} Age{?s})}"))

          self$cv_table[i,] <- inp_line
        }

      }

      return(nline)

    },


    #' @description
    #' Returns the values for the Process Error parameter formatted
    #' to the AGEPRO input file format.
    inplines_process_error = function(delimiter = "  ") {

      return(list(
        self$inp_keyword,
        paste(self$input_option,
              as.numeric(self$time_varying),
              sep = delimiter),
        paste(apply(self$parameter_table, 1, paste,
                     collapse = delimiter), collapse = "\n"),
        paste(apply(self$cv_table, 1, paste,
                    collapse = delimiter), collapse = "\n")

      ))
    }








  ), active = list (

    #' @field input_option
    #' Option to indicate this parameter will be read:
    #' \itemize{
    #'  \item `0` By default, done interactively via interface.
    #'  \item `1` Imported via location of an existing data file.
    #' }
    input_option = function(input_flag) {
      if(missing(input_flag)){
        private$.input_option
      } else {
        checkmate::assert_integerish(input_flag, lower = 0)
        checkmate::assert_subset(input_flag, private$.valid_input_options)
        private$.input_option <- input_flag
      }
    },

    #' @field time_varying
    #' [Logical][base::logical] flag to list parameter process data by
    #' observation year
    time_varying = function(time_flag) {
      if(missing(time_flag)){
        private$.time_varying
      } else {
        checkmate::assert_logical(time_flag)
        private$time_varying_toggle_resets_parameter_table(time_flag)
        private$.time_varying <- time_flag
      }
    },

    #' @field parameter_table This is the logic for the fish population or
    #' fishery's processes by age (and by fleet if fleets are a
    #' factor).
    parameter_table = function(value) {
      if(missing(value)){
        private$.parameter_table
      } else {
        checkmate::assert_matrix(value, min.cols = 1, min.rows = 1)
        #TODO: Create parameter_table value Validation for upper_bound
        private$.parameter_table <- value
      }
    },

    #' @field cv_table Matrix containing the vector of the lognormal process
    #' error of the average population or fishery process parameter's at age
    #' (and by fleet if fleets are a factor).
    cv_table = function(value) {
      if(missing(value)) {
        private$.cv_table
      } else {
        checkmate::assert_matrix(value, min.cols = 1, min.rows = 1)
        private$.cv_table <- value
      }
    },

    #' @field parameter_title
    #' Name of the population or fishery process
    parameter_title = function(value) {
      if(missing(value)){
        private$.parameter_title
      } else {
        checkmate::assert_character(value)
        private$.parameter_title <- value
      }
    },

    #' @field inp_keyword
    #' Returns AGEPRO input-file formatted Parameter name
    inp_keyword = function() {
      private$.inp_keyword
    },

    #' @field json_list_process_error
    #' Returns JSON list object with Process Error Parameter values
    json_list_process_error = function(){
      return(list(
        flag = self$input_option,
        timeflag = self$time_varying,
        value = self$parameter_table,
        error = self$cv_table
      ))
    }

  )

)



#' @title
#' Input information for the natural mortality rate (M) at Age
#'
#' @description
#' Generalized Class Structure for Natural Mortality rate of age
#' AGEPRO Keyword parameter.
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @template enable_cat_print
#' @template process_error_initalize_params
#'
#' @export
natural_mortality <- R6Class(
  "natural_mortality",
  inherit = ageproR::process_error,
  public = list(

    #' @description
    #' Initializes the class
    #'
    #'
    initialize = function(proj_years,
                          num_ages,
                          input_option = 0,
                          time_varying = TRUE,
                          enable_cat_print = TRUE) {


      super$initialize(proj_years,
                       num_ages,
                       1, #Single, non-Fleet dependent parameter.
                       input_option,
                       time_varying)

      self$parameter_title <- "Natural mortality Of Age"
      private$.inp_keyword <- "[NATMORT]"

      cli_keyword_heading(tolower(
        substr(private$.inp_keyword, 2, nchar(private$.inp_keyword) - 1)
      ))
      cli_alert("Setting up Default Values")
      self$print(enable_cat_print, omit_rows=TRUE)

    }

  )

)

#' @title
#' Fishery Selectivity at age by fleet
#'
#' @description
#' Class Structure for Fishery Selectivity at age by fleet AGEPRO Keyword
#' parameter.
#'
#' @param num_fleets Number of Fleets.
#'
#' @template process_error_initalize_params
#' @template enable_cat_print
#'
#' @importFrom R6 R6Class
#'
#' @export
fishery_selectivity <- R6Class(
  "fishery_selectivity",
  inherit = ageproR::process_error,
  public = list(

    #' @description
    #' Initializes new instance
    #'
    initialize = function(proj_years,
                         num_ages,
                         num_fleets,
                         input_option = 0,
                         time_varying = TRUE,
                         enable_cat_print = TRUE) {

      super$initialize(proj_years,
                      num_ages,
                      num_fleets,
                      input_option,
                      time_varying)

      self$parameter_title <- "Fishery Selectivity at age by fleet"
      private$.inp_keyword <- "[FISHERY]"

      cli_keyword_heading(tolower(
        substr(private$.inp_keyword, 2, nchar(private$.inp_keyword) - 1)
      ))
      cli_alert("Setting up Default Values")
      self$print(enable_cat_print, omit_rows = TRUE)

    }

  )

)



#' @title
#' Discard fraction of numbers at age
#'
#' @description
#' Class Structure for discard faction at age AGEPRO Keyword parameter.
#' AGEPRO model must indicate _discards are present_, enabled via general
#' options [discards field][ageproR::general_params].
#'
#' @param num_fleets Number of Fleets.
#'
#' @template process_error_initalize_params
#'
#' @importFrom R6 R6Class
#'
#' @export
discard_fraction <- R6Class(
  "discard_fraction",
  inherit = ageproR::process_error,
  public = list (

    #' @description
    #' Initializes Class
    #'
    initialize = function(proj_years,
                          num_ages,
                          num_fleets,
                          input_option = 0,
                          time_varying = TRUE) {

      super$initalize(proj_years,
                      num_ages,
                      num_fleets,
                      input_option,
                      time_varying)

      self$parameter_title <- "Discards Fraction of Numbers at Age"
      private$.inp_keyword <- "[DISCARDS]"
      private$.discards_parameter <- TRUE

    }

  )
)
