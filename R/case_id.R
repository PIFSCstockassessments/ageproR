
#' AGEPRO case id
#'
#' Input title identifying model attributes
#'
#' @export
#' @importFrom R6 R6Class
case_id <- R6Class(
  "case_id",
  private = list(
    .case_id = NULL
  ),
  public = list (

    # #' @field inp_pointer inp file
    #inp_pointer = input_file$new(),

    #' @description Initalize
    initalize = function() {

    },

    #' @description Read inp
    #'
    #' @param con input file connection
    #' @param nline Line number
    read_inp = function(con, nline){

      nline <- nline + 1
      self$case_id <- readLines(con, n = 1, warn = FALSE)
      message("Line ", nline, " placeholders: ", self$case_id)
      return(nline)
    }


  ),
  active = list (

    #' @field case_id case id
    case_id = function(val) {
      if(missing(val)){
        return(private$.case_id)
      }else{
        private$.case_id <- val
      }
    }
  )
)
