
#' @title Input information for bootstrap numbers at age file
#'
#' @description
#' -The Number of data values of each row must equal to the number of age classes.
#' -The number of rows in a bootstrap file must be at least equal to the number of bootstrap
#' iterations containing the population of the first year in the projection
#'
#' @template inp_con
#' @template nline
#' @template elipses
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_numeric test_file_exists
bootstrap <- R6Class(
  "bootstrap",
  private = list (

    .num_bootstraps = NULL,
    .pop_scale_factor = NULL,
    .bootstrap_file = NULL

  ), public = list (

    #' @description
    #' Initializes the Bootstrap Class
    #'
    initialize = function(){

      cli_keyword_heading("Bootstrap")
      cli_alert("Setting up Default Values")

      self$num_bootstraps <- 0
      self$pop_scale_factor <- 0

      self$print()

    },

    #' @description
    #' Reads in BOOTSTRAP numbers and options from AGEPRO Input file
    #'
    read_inp_lines = function(inp_con, nline){
      #Read an additional line from the file connection,
      #and split the line into 2 substrings, and ...
      inp_line <- read_inp_numeric_line(inp_con)

      #Assign substrings
      self$num_bootstraps <- inp_line[1]
      self$pop_scale_factor <- inp_line[2]

      nline <- nline + 1
      cli_alert("Line {nline}: ")
      cli_text("Number of Bootstraps: {.val {self$num_bootstraps}}")
      cli_text("Population Scale Factor (BootFac): {.val {self$pop_scale_factor}}")

      #Read another line from the file connection, and
      #assign it as bootstrap filename
      nline <- nline + 1
      cli_alert("Line {nline}: ")
      self$bootstrap_file <- readLines(inp_con, n = 1, warn = FALSE)


      return(nline)

    },

    #' @description
    #' Prints out BOOTSTRAP fields
    #'
    print = function(...) {
      cli_ul()
      cli_li("Number of Bootstraps: {.val {self$num_bootstraps}}")
      cli_li("Population Scale Factor (BootFac): {.val {self$pop_scale_factor}}")
      cli_end()
      cli_alert_info("Bootstrap File")
      ifelse(is.null(self$bootstrap_file),
             cli_alert_warning(c("Replace with a valid Bootstrap file before ",
                            "processing to AGEPRO calcualtion engine")),
             self$bootstrap_file)
    }



  ), active = list (

    #' @field num_bootstraps
    #' Number of bootstraps replicates of initial popualion size
    num_bootstraps = function(value) {
      if(missing(value)){
        private$.num_bootstraps
      }else{
        assert_numeric(value, lower = 0)
        private$.num_bootstraps <- value
      }
    },

    #' @field pop_scale_factor
    #' Population Scale Factor, or BootFac, that represents the multiplicative
    #' factor to convert the relative bootstrap population numbers at age to
    #' absolute numbers at age.
    pop_scale_factor = function(value) {
      if(missing(value)){
        private$.pop_scale_factor
      }else{
        assert_numeric(value, lower = 0)
        private$.pop_scale_factor <- value
      }
    },

    #' @field bootstrap_file
    #' Bootstrap file path.
    bootstrap_file = function(value) {
      if(missing(value)){
        private$.bootstrap_file
      }else{
        #Validate that 'value' points to a existing bootstrap file.
        if(test_file_exists(value, access= "r", extension = "bsn")){
          cli_text("Bootstrap file: {.val {value}}")
          private$.bootstrap_file <- value
        }else {
          cli_div(theme = list(span.val = list(color="orange",
                                               "font-style"="italic")))
          cli_alert_warning(c("Bootstrap file path does not exist in system: ",
                            "{.val {value}}"))
          cli_end()
          private$.bootstrap_file <- NULL
        }
      }
    }

  )

)
