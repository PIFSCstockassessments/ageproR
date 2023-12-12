
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
    .projection_analyses_type = NULL,

    .keyword_name = NULL,

    #setup variables at initialization
    .projection_years = NULL


  ),
  public = list(

    #' @description
    #' Initializes class
    #'
    #' @param proj_years May be a single numeric value: the number of years in
    #' the time projection; a vector of sequential values: Sequence of years
    #' in from first to last year of the time projection; or an instance of
    #' [Projection years][ageproR::projection_years]:
    #'
    initialize = function(proj_years) {

      # Handle `proj_years` that may be a single int or vector of sequential
      # values or an instance of ageproR::projection_years
      private$.projection_years <- check_proj_years_parameter(proj_years)

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
        checkmate::assert_numeric(value, len = 1,
                                  lower = 0,
                                  .var.name = "target_year")
        private$.target_year <- value
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
#' @param proj_years May be a single numeric value: the number of years in the
#' time projection; a vector of sequential values: Sequence of years in from
#' first to last year of the time projection; or an instance of
#' [Projection years][ageproR::projection_years]
#'
#' @importFrom R6 R6Class
#'
#' @keywords projection_analyses
#'
standard_projection <- R6Class(
  "standard_projection",
  inherit = projection_analyses,
  public = list(

    #' @description
    #' Initializes class
    #'
    initialize = function(proj_years) {

      super$initialize(proj_years)
      private$.projection_analyses_type <- "standard"

    }
  )

)

#' @title
#' Projection analyses that shows the probability of exceeding overfishing
#' threshold of the target year
#'
#' @description
#' Input information for calculating Total Allowable Catch (\eqn{TAC_{pstar}})
#' to produce \eqn{P*}, which is the probability of overfishing in the target
#' projection year
#'
#' @template inp_con
#' @template nline
#' @template elipses
#'
#' @param proj_years May be a single numeric value: the number of years in the
#' time projection; a vector of sequential values: Sequence of years in from
#' first to last year of the time projection; or an instance of
#' [Projection years][ageproR::projection_years]
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

    .num_pstar_levels = NULL,
    .pstar_levels_table = NULL,
    .pstar_overfishing_f = NULL

  ),
  public = list(

    #' @description
    #' Initializes class
    #'
    #' @param num_pstar_levels Number of Pstar levels. Default is `1`
    #' @param pstar_f Fishing mortality rate \eqn{f}. Default is `0.0`
    #'
    initialize = function(proj_years,
                          num_pstar_levels = 1,
                          pstar_f = 0.0) {

      super$initialize(proj_years)

      private$.projection_analyses_type <- "pstar"

      self$num_pstar_levels <- num_pstar_levels
      self$pstar_overfishing_f <- pstar_f

      self$pstar_levels_table <-
        self$create_blank_pstar_levels_table(self$num_pstar_levels)

    },

    #' @description
    #' Creates a blank table-like matrix of probabilities of overfishing,
    #' or PStar values, to be used.
    #'
    #' @param num_pstar_levels Number of pstar values
    #'
    create_blank_pstar_levels_table = function(num_pstar_levels = 1){

      dimnames_pstar_levels_table <-
        list(NULL, paste("Level", 1:num_pstar_levels))

      return(create_blank_parameter_table(num_rows = 1,
                                     num_cols = num_pstar_levels,
                                     dimnames = dimnames_pstar_levels_table))

    },


    #' @description
    #' Reads in the values from the keyword parameter PSTAR from the
    #' AGEPRO Input file
    #'
    read_inplines = function (inp_con,
                              nline,
                              proj_years) {

      #TODO: Verify pstar_projections class


      cli::cli_alert_info("Reading {.strong {private$.keyword_name}}")

      nline <- nline + 1

      #Read an additional line from the file connection, and assign it to
      #num_pstar_levels
      inp_line <- read_inp_numeric_line(inp_con)

      self$num_pstar_levels <- inp_line

      cli::cli_alert(paste0("Line {nline}:",
                            "Number of Pstar Levels ({.emph KPstar}): ",
                            "{.val {self$num_pstar_levels}}"))

      # Create new pstar_level_table matrix based on num_pstar_levels
      self$pstar_levels_table <-
        self$create_blank_pstar_levels_table(self$num_pstar_levels)

      # Read an additional line from the file connection, delimit the line
      # by whitespace, and assign as pstar_levels_table
      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      self$pstar_levels_table[1,] <- inp_line

      cli::cli_alert(c("Line {nline}: ",
                       "Pstar Levels : ",
                       "{.val {self$pstar_levels_table[1,]}} "))

      # Read an additional line from the file connection, and assign it to
      # pstar_overfishing_f(PStarF)
      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      self$pstar_overfishing_f <- inp_line
      cli::cli_alert(paste0("Line {nline}:",
                            "Overfishing Rate ({.emph PstarF}): ",
                            "{.val {self$pstar_overfishing_f}}"))

      # Read an additional line from the file connection, and assign it to
      # target_year(TargetYear)
      nline <- nline + 1
      cli::cli_alert(paste0("Line {nline}",
                            "Target Year: ",
                            "{.val {self$target_year}}"))

      return(nline)

    }

  ),
  active = list(

    #' @field num_pstar_levels
    #' Number of pstar values to be evaluated
    #'
    num_pstar_levels = function(value) {
      if(missing(value)){
        private$.num_pstar_levels
      }else{
        checkmate::assert_numeric(value, len = 1, lower = 1,
                                  .var.name = "num_pstar_levels")

        private$.num_pstar_levels <- value

      }
    },

    #' @field pstar_levels_table
    #' The vector of probabilities of overfishing or PStar values to be used
    #'
    pstar_levels_table = function(value) {
      if(missing(value)){
        private$.pstar_levels_table
      }else{
        checkmate::assert_matrix(value, mode = "numeric",
                                 nrows = 1, min.cols = 1,
                                  .var.name = "pstar_levels_table")

        private$.pstar_levels_table <- value
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
