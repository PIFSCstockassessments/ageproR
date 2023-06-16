
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
    .bootstrap_file = NULL,


    #Validate bootstrap_file
    validate_bootstrap_file = function(value) {
      #Validate that 'value' points to a existing bootstrap file.
      if(test_file_exists(value, access= "r", extension = "bsn")){
        #If validated, assign value
        cli_alert_success("Bootstrap file: {.val {value}}")
        private$.bootstrap_file <- value
      }else {
        #Else, warn bootstrap file name does not exist
        cli_div(
          theme = list(span.val = list(color="orange",
                                       "font-style"="italic")))
        cli_alert_warning(c("Bootstrap file path does not exist in system: ",
                            "{.val {value}}"))
        cli_end()
        warning(paste0("'",value,"' does not exist. \n",
        "Please provide a vaild bootstrap filepath when saving to input ",
        "file for the AGEPRO calcuation engine."), call. = FALSE)
      }

    }


  ), public = list (

    #' @description
    #' Initializes the Bootstrap Class
    #'
    initialize = function(){

      cli_keyword_heading("Bootstrap")
      cli_alert("Setting up Default Values")

      self$num_bootstraps <- 0
      self$pop_scale_factor <- 0

      suppressWarnings(self$bootstrap_file <- NULL)

      self$print()

    },

    #' @description
    #' Uses file dialog interface to retrieve Bootstrap file name
    #'
    get_bootstrap_filename = function() {

      bootstrap_path <-
            open_file_dialog(c("AGEPRO Bootstrap File",".bsn"))

      if(test_file_exists(bootstrap_path, access = "r", extension = "bsn" )){
        self$bootstrap_file <- bootstrap_path
      }

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
      cli_alert_info("Bootstrap File:")
      ifelse(test_file_exists(self$bootstrap_file),
             cli_li("{.val {self$bootstrap_file}}"),
             cli_alert_warning(c("Replace with a valid Bootstrap file before ",
                            "processing to AGEPRO calcualtion engine"))
             )
      cli_end()
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
        private$validate_bootstrap_file(value)
      }
    },

    #' @field json_bootstrap
    #' JSON list object for BOOTSTRAP keyword parameter
    #'
    json_bootstrap = function() {
      return(list(
        nboot = self$num_bootstraps,
        bootFac = self$pop_scale_factor,
        bootFile = self$bootstrap_file
      ))
    },


    #' @field inp_bootstrap
    #' Returns BOOTSTRAP values AGEPRO input file format (*,inp)
    inp_bootstrap = function() {
      #Warn if bootstrap file does not exists on system
      if(!test_file_exists(self$bootstrap_file)){
        warning("Bootstrap filename does not exist on system.", call. = FALSE)
      }
      return(list(
        paste(self$num_bootstraps, self$pop_scale_factor),
        self$bootstrap_file
      ))
    }

  )

)
