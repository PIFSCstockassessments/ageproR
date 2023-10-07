
#' @title
#' AGEPRO Case ID
#'
#' @description
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

    .keyword_name = "caseid",

    .caseid = NULL
  ),
  public = list(

    #' @description
    #' Initialize Class
    #'
    initalize = function() {
      self$caseid <- NULL
    },

    #' @description
    #' Prints out Model case id
    #'
    print = function() {
      cli::cli_text("{symbol$info} case_id: {.val {self$caseid}}")
    },

    #' @description
    #' Read AGEPRO Case ID from input data file
    #'
    read_inp_lines = function(inp_con, nline) {

      nline <- nline + 1
      self$case_id <- readLines(inp_con, n = 1, warn = FALSE)
      #message("Line ", nline, ": Case ID: ", self$case_id)
      cli::cli_alert("Line {nline}: CASE ID: {self$caseid}")
      return(nline)
    },


    #' @description
    #' Returns the values for the CASEID keyword parameter formatted
    #' to the AGEPRO input file format.description
    #'
    inplines_case_id = function() {
      return(list(
        self$inp_keyword,
        self$caseid
      ))
    }

  ),
  active = list(

    #' @field caseid case id
    caseid = function(val) {
      if (missing(val)) {
        return(private$.caseid)
      }else {
        private$.caseid <- val
      }
    },

    #' @field keyword_name
    #' AGEPRO keyword parameter name
    keyword_name = function() {
      private$.keyword_name
    },

    #' @field inp_keyword
    #' Returns AGEPRO input-file formatted Parameter
    inp_keyword = function() {
      paste0("[",toupper(private$.keyword_name),"]")
    }
  )
)
