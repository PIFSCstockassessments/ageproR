
#' AGEPRO case id
#'
#' Input title identifying model attributes
#'
#' @template inp_con
#' @template nline
#'
#' @export
#' @importFrom R6 R6Class
case_id <- R6Class(
  "case_id",
  private = list(
    .case_id = NULL
  ),
  public = list(

    #' @description Initalize
    initalize = function() {
      self$case_id <- private$.case_id
    },


    #' @description
    #' Read AGEPRO Case ID from input data file
    #'
    read_inp_lines = function(inp_con, nline) {

      nline <- nline + 1
      self$case_id <- readLines(inp_con, n = 1, warn = FALSE)
      message("Line ", nline, ": Case ID: ", self$case_id)
      return(nline)
    },


    #' @description
    #' Returns the values for the CASEID keyword parameter formatted
    #' to the AGEPRO input file format.description
    #'
    inplines_case_id = function() {
      return(list(
        "[CASEID]",
        self$case_id
      ))
    }

    #TODO: CASE ID print function

  ),
  active = list(

    #' @field case_id case id
    case_id = function(val) {
      if (missing(val)) {
        return(private$.case_id)
      }else {
        private$.case_id <- val
      }
    }
  )
)
