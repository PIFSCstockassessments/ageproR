
#' Projection Years
#'
#' Handle the ambiguous use of "projection years" that can interpreted as
#' a single int representing the count of projection years or a vector of
#' sequential values representing a vector of "years" from first to last
#' year of the AGEPRO model's time projection.
#'
#' @import cli
#' @importFrom R6 R6Class
#' @export
#'
projection_years <- R6Class(
  "projection_years",
  public = list(

    #' @description
    #' Initializes the class
    #'
    #' @param x Projected year value or vector
    #'
    initialize = function (x) {

      # Validation
      checkmate::assert_numeric(x)

      #Handle as single int or a vector of sequential values
      if (checkmate::test_int(x)) {
        #single
        self$count <- x
        self$sequence <- 1:x

      } else {
        self$count <- length(x)
        self$sequence <- x
      }

    },

    #' @description
    #' Prints out projection_years fields
    #'
    print = function (){
      cli_ul()
      cli_alert_info("Projection Years")
      cli_li("count: {.val {self$count}}")
      cli_li("sequence: {.val {self$sequence}}")
    }

  ),
  active = list(

    #' @field count The count of projection_years
    count = function(value){
      if(missing(value)){
        private$.count
      } else {
        checkmate::assert_integerish(value, lower = 1, len = 1,
                                     .var.name = "projection_years count")
        private$.count <- value
      }

    },

    #' @field sequence Vector of years of time projection
    sequence = function(value){
      if(missing(value)){
        private$.sequence
      } else {
        # Validate input
        validation_error <- checkmate::makeAssertCollection()
        checkmate::assert_numeric(value, unique = TRUE, sorted = TRUE,
                                  add = validation_error)
        assert_proj_years_sequence(value, add = validation_error)
        checkmate::reportAssertions(validation_error)

        private$.sequence <- value
      }
    }

  ),
  private = list(

    .count = NULL,
    .sequence = NULL

  )
)

