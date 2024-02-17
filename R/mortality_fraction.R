

#' @title
#' Proportion of total mortality occurring prior to spawn in year t
#'
#' @description
#' Class Structure that includes proportion seasonal timing for fishing mortality & natural mortality
#' prior to spawning season
#'
#' @template proj_years_vector
#' @template inp_con
#' @template nline
#'
#' @param time_varying
#' Logical flag that enables the stochastic parameter
#' to use as a time-varying array if TRUE (or 1). Otherwise, FALSE the
#' vector will cover "all years" of the projection. Default is FALSE
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @export
mortality_fraction_prior_spawn <- R6Class(
  "mortality_fraction_prior_spawn",
  private = list(

    .keyword_name = "biological",

    .projection_years = NULL,
    .time_varying = NULL,
    .natural_mortality_prior_spawn = NULL,
    .fishing_mortality_prior_spawn = NULL,


    set_time_varying = function(value){
      validation_error <- checkmate::makeAssertCollection()
      checkmate::assert(
        checkmate::check_numeric(value),
        checkmate::check_choice(value, choices = c(0, 1)),
        combine = "and",
        add = validation_error
      )
      checkmate::reportAssertions(validation_error)

      private$.time_varying <- value
    },

    # Handle proj_years that may be a single int or sequential numeric vector
    set_projection_years = function(value){

      # Handle instances where value is passed as projection_years class
      if(checkmate::test_r6(value, public = c("count","sequence"))){
        private$.projection_years <- value
        invisible(value)
      }

      checkmate::assert_numeric(value)
      private$.projection_years <-
        ageproR::projection_years$new(as.numeric(value))


    },


    # Creates matrix object vector for natural_mortality_before_spawn and
    # fishing_mortality_before_spawn
    set_fraction_mortality_matrix = function(value,
                                             time_varying,
                                             row_names = NULL) {

      #TODO: validate time_varying field

      default_proportion <- 0.5

      if(time_varying){
        return(
          matrix(rep(default_proportion/private$.projection_years$count,
                     private$.projection_years$count),
                 nrow = 1,
                 ncol = private$.projection_years$count,
                 dimnames = list(row_names,
                                 private$.projection_years$sequence))
        )

      }else{
        return(
          matrix(default_proportion,
                 nrow = 1,
                 ncol = 1,
                 dimnames = list(row_names, "All Years"))
        )

      }

    }


  ),
  public = list(


    #' @description
    #' Initializes the class
    #'
    #' @param default_proportion
    #' Proportion default values. Time varying will adjust the values by
    #' number of projection years.
    #'
    initialize = function(proj_years_vector, time_varying = FALSE,
                          default_proportion = 0.5) {

      checkmate::assert_logical(time_varying,
                                any.missing = FALSE, all.missing=FALSE,
                                len = 1)

      #setup
      private$set_projection_years(proj_years_vector)
      private$.time_varying <- time_varying

      #zfrac_default: defaults
      if(private$.time_varying) {
        default_proportion <-
          rep(default_proportion/private$.projection_years$count,
              private$.projection_years$count)
      }

      private$.natural_mortality_prior_spawn <-
        private$set_fraction_mortality_matrix(default_proportion,
                                              private$.time_varying,
                                              "natural_mortality_prior_spawn")

      private$.fishing_mortality_prior_spawn <-
        private$set_fraction_mortality_matrix(default_proportion,
                                              private$.time_varying,
                                              "fishing_mortality_prior_spawn")

    },

    #' @description
    #' Formatted to print out the values of the Fraction Mortality Parameter
    #'
    #' @template enable_cat_print
    #'
    print = function(enable_cat_print = TRUE){

      cli::cli_ul()
      #cli statement included in call to time_varying active binding
      cli::cli_li(paste0("time_varying: ",
                         "{.val {private$.time_varying} ",
                         "({as.logical(private$.time_varying)})}"))
      cli::cli_end()

      if(enable_cat_print){
        cli::cli_alert(paste0("proportion_total_mortality ",
                              "(Fraction Mortality prior to spawn)"))
        cli::cat_print(private$.proportion_total_mortality)

      }

    },



    #' @description
    #' Reads in the values from the keyword parameter BIOLOGICAL from the
    #' AGEPRO Input file
    #'
    read_inp_lines = function(inp_con,
                              nline,
                              proj_years_vector) {


      private$set_projection_years(proj_years_vector)

      cli::cli_alert_info("Reading {.strong {private$.keyword_name}}")

      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      private$set_time_varying(inp_line)
      cli::cli_alert(paste0("Line {nline}: ",
                            "time_varying: {.var {private$.time_varying}} ",
                            "{.emph {as.logical(private$.time_varying)}}"))

      if(isTRUE(private$.time_varying)){
        names_proj_years_count <- paste0(private$.projection_years$count,
                                         " years")
      }else{
        names_proj_years_count <- "All Years"
      }


      nline <- nline + 1
      tf_inp_line <- read_inp_numeric_line(inp_con)

      cli::cli_alert(paste0("Line {nline}: ",
                            "fishing_mortality_prior_spawn: ",
                            "{.val {tf_inp_line}} ",
                            "{.emph ({names_proj_years_count})}" ))

      private$.fishing_mortality_prior_spawn <-
        private$set_fraction_mortality_matrix(
          tf_inp_line,
          private$.time_varying,
          row_names = "fishing_mortality_prior_spawn")


      nline <- nline + 1
      tm_inp_line <- read_inp_numeric_line(inp_con)
      cli::cli_alert(paste0("Line {nline}: ",
                            "natural_mortality_prior_spawn: ",
                            "{.val {tm_inp_line}} ",
                            "{.emph ({names_proj_years_count})}" ))

      private$.natural_mortality_prior_spawn <-
        private$set_fraction_mortality_matrix(
          tm_inp_line,
          private$.time_varying,
          row_names = "natural_mortality_prior_spawn")

    }


  ),
  active = list(

    #' @field time_varying
    #' [Logical][base::logical] flag to list fishing and natural mortality per
    #' observation year if TRUE or representative of the
    time_varying = function(value) {
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      cli::cli_text(paste0("{as.numeric(private$.time_varying)} ",
                           "({.emph { as.logical(private$.time_varying)}})"))
    },

    #' @field proportion_total_mortality
    #' Proportion of total mortality occurring prior to spawning
    proportion_total_mortality = function(value) {
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      rbind(private$.natural_mortality_prior_spawn,
            private$.fishing_mortality_prior_spawn)
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
