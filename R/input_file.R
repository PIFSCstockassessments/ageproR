

#' AGEPRO Input File
#'
#' File Functionality is based on r4ss
#'
#' @export
#' @importFrom R6 R6Class
#'
input_file <- R6Class(
  "input_file",
  private = list(

    .pre_v4 = FALSE,
    .supported_inp_versions = "AGEPRO VERSION 4.0"

  ),
  public = list(


    #' @description
    #' input_file w/ defaults.
    #'
    initialize = function() {


    },

    #' @description
    #' reads
    #'
    read_inpfile <- function(inpfile) {

      #Verify that input file location is valid

      #Console Message

      tryCatch(
        {
          inp_lines <- readLines(inpfile, warn = FALSE)
        },
        warning = function(cond) {
          message("Warning. There was an issue reading this file:")
          message(cond)
          return(NULL)
        },
        finally = function(cond) {
          message("Input File Read")
        }
      )

      #check_inputfile_version
      self$check_inpfile_version(inp_lines[1])

      #loop through inpfile to read in value fore each parameter keyword


    },


    check_inpfile_version <- function(inpline) {
      checkmate::assert_character(inpline, length = 1)
      tryCatch(
        {
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

    match_keyword <- function() {

      model_dict <- dict(list(
        "[CASEID]" = stop("Not Implmented"), #TODO:agepro_model CASEID
        "[GENERAL]" = agepro_model$general$read(), #TODO
        "[RECRUIT]" = stop("Not Implmented"), #TODO
        "[STOCK_WEIGHT]" = stop("Not Implmented"),
        "[SSB_WEIGHT]" = stop("Not Implmented"),
        "[CATCH_WEIGHT]" = stop("Not Implmented"),
        "[DISC_WEIGHT]" = stop("Not Implmented"),
        "[MATURITY]" = stop("Not Implmented"),
        "[FISHERY]" = stop("Not Implmented"),
        "[DISCARD]" = stop("Not Implmented"),
        "[BIOLOGICAL]"  = stop("Not Implmented"),
        "[BOOTSTRAP]" = stop("Not Implmented"),
        "[HARVEST]" = stop("Not Implmented"),
        "[REFPOINT]" = stop("Not Implmented"),
        "[BOUNDS]" = stop("Not Implmented"),
        "[RETROADJUST" = stop("Not Implmented"),
        "[OPTIONS]" = stop("Not Implmented"),
        "[SCALE]" = stop("Not Implmented"),
        "[PERC]" = stop("Not Implmented"),
        "[PSTAR]" = stop("Not Implmented")

      ))
    }

  ),
  active = list(



  )
)

