

#' AGEPRO Input File
#'
#' File Functionality is based on AGEPRO-CoreLib implementation
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_character
#' @importFrom collections dict
#'
input_file <- R6Class(
  "input_file",
  private = list(

    .pre_v4 = FALSE,
    .supported_inp_versions = "AGEPRO VERSION 4.0"

  ),
  public = list(

    #' @description
    #' Initializes the input file
    initialize = function() {

      private$.pre_v4 <- FALSE

    },

    #' @description
    #' Reads in AGEPRPO input file
    #'
    #' @param inpfile input file
    #'
    #' @export
    read_inpfile = function(inpfile) {

      #Verify that input file location is valid

      #Console Message

      tryCatch(
        {
          #(Reset) File connection to input file
          inp_con <- file(file.path(inpfile), "r")

          message("Test")
          self$read_inpfile_values(inp_con)


        },
        warning = function(cond) {
          message("Warning. There was an issue reading this file:")
          message(cond)
          close(inp_con)
          return()
        },
        error = function(cond) {
          message("There was an error reading this file.")
          message("Error:", cond)
          close(inp_con)
          return()
        },
        finally = function(cond) {
          message("Input File Read")
          #close file connections
          close(inp_con)
        }

      )

    },


    #' @description
    #' Check Input File Version
    #'
    #' @param inpline Input File Line
    #'
    check_inpfile_version = function(inpline) {
      #checkmate::assert_character(inpline, length = 1)
      tryCatch(
        {
          message("inpline:", inpline)
          inpline %in% private$.supported_inp_versions
        },
        error = function(cond) {
          message("This version of this input file is not supported : ",
                  inpline)
          message("Supported verion(s): ", private$.supported_inp_versions)
          message("Error: ", cond)
        }
      )
    },

    #' @description
    #' Read Input file Values
    #'
    #' @param inp_con File connection
    #' @export
    #'
    read_inpfile_values = function(inp_con) {

      # AGEPRO keyword parameter dictionary #TODO: Refactor to function
      keyword_dict <- dict(list(
        "[CASEID]" = message("CASEID Not Implmented. "),
        "[GENERAL]" = message("GENERAL not Implemented"),
        "[RECRUIT]" = self$not_implemented,
        "[STOCK_WEIGHT]" = self$not_implemented,
        "[SSB_WEIGHT]" = self$not_implemented,
        "[CATCH_WEIGHT]" = self$not_implemented,
        "[DISC_WEIGHT]" = self$not_implemented,
        "[MATURITY]" = self$not_implemented,
        "[FISHERY]" = self$not_implemented,
        "[DISCARD]" = self$not_implemented,
        "[BIOLOGICAL]"  = self$not_implemented,
        "[BOOTSTRAP]" = self$not_implemented,
        "[HARVEST]" = self$not_implemented,
        "[REFPOINT]" = self$not_implemented,
        "[BOUNDS]" = self$not_implemented,
        "[RETROADJUST" = self$not_implemented,
        "[OPTIONS]" = self$not_implemented,
        "[SCALE]" = self$not_implemented,
        "[PERC]" = self$not_implemented,
        "[PSTAR]" = self$not_implemented
      ))

      message("Check Version")
      #check_inputfile_version
      #assume line 1 is version string
      self$check_inpfile_version( readLines(inp_con, n = 1, warn = FALSE) )

      nlines <- 0L
      message("LOOP")
      #loop through inpfile to read in value fore each parameter keyword
      #while(length(inp_line <- readLines(inp_con, n = 1, warn = FALSE)) > 0 ) {
      while(TRUE) {
        inp_line <- readLines(inp_con, n = 1, warn = FALSE)
        if(length(inp_line) == 0 ) {
          break
        }

        nlines <- nlines + 1
        message("line:", inp_line)
        #Match line to keyword.
        if(!keyword_dict$has(inp_line)){
          warning("Input line ",nlines, " does not match AGEPRO keyword parameter")
          next
        }
        #If there is a match w/ keyword_dict then use the keyword's own
        #readLine function
        keyword_dict$get(inp_line)


      }

    },

    #' @description
    #' Throws a Not Implemented exception
    not_implemented = function() {
      stop("Not Implmented")
    }

  )
)

