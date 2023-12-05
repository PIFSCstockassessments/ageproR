
#' @title
#' Generalized structure of projection analyses keyword parameters
#'
#' @description
#' Generalized or common data structure for Standard (Harvest Table only),
#' Rebuilding, and PStar projection analyses based AGEPRO Keyword parameters.
#'
#' @template elipses
#' @template inp_con
#' @template nline
#' @template delimiter
#' @template enable_cat_print
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @keywords projection_analyses
#'
projection_analyses <- R6Class(
  "projection_analyses",
  private = list(

    .target_year = NULL,
    .projection_analyses = NULL,

    .keyword_name = NULL,

    #setup variables at initialization
    .projection_years = NULL,

    setup_projection_years_class = function(proj_years) {

      #Handles potential proj_years "Factor" types, and returns its
      #"levels", the intended values assigned to this value.
      if(is.factor(proj_years)) {
        proj_years <- levels(proj_years)
      }

      # Handle num_projection_years that may be a single int
      # or vector of sequential values
      projection_years_class <- ageproR::projection_years$new(as.numeric(proj_years))

      return(projection_years_class)
    }



  ),
  public = list(

    #' @description
    #' Initializes class
    #'
    #' @param proj_years [Projection years][ageproR::projection_years]:
    #' Input can be Sequence of years in from first to last year of
    #' projection or the number of years in the time projection.
    #'
    initialize = function(proj_years) {

      #Private Helper method to handle proj_years parameter value
      private$.projection_years <-
        private$setup_projection_years_class(proj_years)

      self$target_year <- 0

    }

  ),
  active = list(

    #' @field target_year
    #' User-Selected target year for rebuilder and pstar projection analyses
    target_year = function(value){
      if(missing(value)){
        private$.target_year
      }else{
        checkmate::assert_numeric(value, lower = 0, .var.name = "target_year" )
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

#' @title
#' Standard Projection Analyses
#'
#' @description
#' Class Structure containing Standard Projection Analyses
#'
#' @importFrom R6 R6Class
#'
#' @keywords projection_analyses
#'
standard_projection <- R6Class(
  "standard_projection",
  inherit = projection_analyses,
  private = list(

    .projection_analyses = "standard"

  ),
  public = list(

    #' @description
    #' Initializes class
    #'
    #' @param proj_years [Projection years][ageproR::projection_years]:
    #' Input can be Sequence of years in from first to last year of
    #' projection or the number of years in the time projection.
    #'
    initialize = function(proj_years) {

      super$initialize(proj_years)
    }
  )

)

#' @title
#' Projection analyses that shows the probability of exceeding overfishing
#' threshold of the target year
#'
#' @description
#' Input information for calculating Total Allowable Catch (\eqn{TAC_pstar})
#' to produce \eqn{P*}, which is the probability of overfishing in the target
#' projection year
#'
#' @importFrom R6 R6Class
#'
#' @keywords projection_analyses
#'
#' @export
#'
pstar_projection <- R6Class(
  "pstar_projection",
  inherit = projection_analyses,
  private = list(

    .projection_analyses = "pstar",

    .num_star_values = NULL,
    .pstar_values = NULL,
    .pstar_overfishing_f = NULL

  ),
  public = list(

    #' @description
    #' Initializes class
    #'
    #' @param proj_years [Projection years][ageproR::projection_years]:
    #' Input can be Sequence of years in from first to last year of
    #' projection or the number of years in the time projection.
    #'
    initialize = function(proj_years) {

      super$initialize(proj_years)

      self$num_pstar_values <- 1
      self$pstar_overfishing_f <- 0.0
      self$pstar_levels <-
        self$create_blank_pstar_levels_table(self$num_pstar_values)


    },

    #' @description
    #' Creates a blank table-like matrix of probabilities of overfishing,
    #' or PStar values, to be used.
    #'
    #' @param num_pstar_values Number of pstar values
    #'
    create_blank_pstar_levels_table = function (num_pstar_values = 1){
      matrix(rep(NA, num_pstar_values),
             nrow = 1,
             ncol = num_pstar_values,
             dimnames = list(NULL,
                             paste("Level",1:num_pstar_values)))
    }

  ),
  active = list(

    #' @field num_pstar_values
    #' Number of pstar values to be evaluated
    #'
    num_pstar_values = function(value) {
      if(missing(value)){
        private$.num_pstar_values
      }else{
        checkmate::assert_numeric(value, len = 1, lower = 1,
                                  .var.name = "num_pstar_values")

        private$.num_pstar_values <- value

      }
    },

    #' @field pstar_levels
    #' The vector of probabilities of overfishing or PStar values to be used
    #'
    pstar_levels = function(value) {
      if(missing(value)){
        private$.pstar_levels
      }else{
        checkmate::assert_numeric(value, lower = 0,
                                  .var.name = "pstar_levels")

        private$.pstar_levels <- value
      }
    },


    #' @field pstar_overfishing_f
    #' Fishing mortality rate that defines the overfishing level
    pstar_overfishing_f = function(value) {
      if(missing(value)){
        private$.pstar_overfishing_f
      }else{
        checkmate::assert_numeric(value, len = 1, lower = 0,
                                  .var.name = "pstar_overfishing_f")

        private$.pstar_overfishing_f <- value
      }
    }
  )







)
