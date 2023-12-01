
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
      projection_years_class <-
        private$setup_projection_years_class(proj_years)

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

      super$initalize(proj_years)
    }
  )

)
