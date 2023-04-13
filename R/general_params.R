
#' @title Input general model parameters
#'
#' @description Stores overall AGERPRO model project parameters including
#' the projection time horizon, the target model's age class range, the
#' quantity of recruitment models used, and fishery processes that affect
#' projections.
#'
#' @template elipses
#' @template inp_line
#' @template inp_con
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom checkmate test_logical test_true assert_number
general_params <- R6Class(
  classname = "general_params",

  public = list(
    #' @field yr_start First Year in Projection
    yr_start = NULL,

    #' @field yr_end Last Year in Projection
    yr_end = NULL,

    #' @field age_begin First Age Class
    age_begin = NULL,

    #' @field age_end Last Age Class
    age_end = NULL,

    #' @field num_fleets Number of Fleets
    num_fleets = NULL,

    #' @field num_rec_models Number of Recruitment Models
    num_rec_models = NULL,

    #' @field num_pop_sims Number of Population Simulations
    num_pop_sims = NULL,

    #' @field discards Are discards present?
    discards = NULL,

    #' @field seed Psuedorandom number seed
    seed = NULL,

    #' @field inp_file File name of opened/imported input file
    inp_file = NULL,

    #' @description
    #' Starts an instances of the AGEPRO Model
    #'
    #' @param yr_start First Year of Projection
    #' @param yr_end Last Year of Projection
    #' @param age_begin age begin
    #' @param age_end age end
    #' @param num_fleets Number of fleets
    #' @param num_rec_models Number of Recruit Modles
    #' @param num_pop_sims Number of population sims
    #' @param discards discards
    #' @param seed Random Number seed
    #'
    initialize = function(yr_start = 0,
                           yr_end = 2,
                           age_begin = 1,
                           age_end = 6,
                           num_fleets = 1,
                           num_rec_models = 1,
                           num_pop_sims = 1000,
                           discards = FALSE,
                           seed = 0) {

      private$cli_general_rule()
      # Discards: Assert logical format
      if (!test_logical(discards)) {
        assert_number(discards, lower = 0, upper = 1)
        discards <- as.logical(discards)
      }

      self$yr_start <- yr_start
      self$yr_end <- yr_end
      self$age_begin <- age_begin
      self$age_end <- age_end
      self$num_fleets <- num_fleets
      self$num_rec_models <- num_rec_models
      self$num_pop_sims <- num_pop_sims
      self$discards <- discards
      self$seed <- seed

      self$print()


    },

    #' @description
    #' Prints out General Parameters
    #'
    print = function(...) {
      cli_ul()
      cli_li("First Year in Projection: {.val {self$yr_start}}")
      cli_li("Last Year in Projection: {.val {self$yr_end}}")
      cli_li("First Age Class: {.val {self$age_begin}}")
      cli_li("Last Age Class: {.val {self$age_end}}")
      cli_li("Number of Fleets: {.val {self$num_fleets}}")
      cli_li("Number of Recruitment Model(s): {.val {self$num_rec_models}}")
      cli_li("Number of Population Simulations: {.val {self$num_pop_sims}}")
      cli_li("Discards are present: {.val {test_true(self$discards)}} ")
      cli_li("Calculation Engine Random Number Seed: {.val {self$seed}}")
      invisible(self)
    },

    #' @description
    #' Reads General AGEPRO parameters from AGEPRO INP Input File
    #' @param nline Reference to current line read
    read_inp_lines = function (inp_con, nline){
      # Read an additional line from the file connection and split the string
      # into substrings by whitespace
      inp_line <-
        unlist(strsplit(readLines(inp_con, n = 1, warn = FALSE), " +"))

      nline <- nline + 1
      cli_alert("Line {nline} ...")

      inp_line <- private$assert_numeric_substrings(inp_line)

      #TODO: Refactor
      self$yr_start <- inp_line[1]
      self$yr_end <- inp_line[2]
      self$age_begin <- inp_line[3]
      self$age_end <- inp_line[4]
      self$num_fleets <- inp_line[5]
      self$num_rec_models <- inp_line[6]
      self$num_pop_sims <- inp_line[7]
      self$discards <- inp_line[8]
      self$seed <- inp_line[9]

      self$print()

      return(nline)
    }

  ),
  active = list(

    #' @field num_years Determines the number of years in projection by the
    #' (absolute) difference of the last and first year of projection.
    num_years = function() {
      abs(self$yr_end - self$yr_start) + 1
    },

    #' @field num_ages Determines number of ages by the (absolute) difference
    #' of the first and last age class.
    num_ages = function() {
      abs(self$age_begin - self$age_end) + 1
    },

    #' @field seq_years Returns a sequence of years from First year of
    #' projection
    seq_years = function() {
      seq(self$yr_start, self$yr_end)
    }

  ), private = list(
    cli_general_rule = function() {
      d <- cli_div(theme = list(rule = list(
          color = "cyan",
          "line-type" = "double")))
      cli_rule("General")
      cli_end(d)
    },

    assert_numeric_substrings = function (inp_line){

      if(!all(grepl("^[[:digit:]]",inp_line))) {

        non_numerics <- inp_line[!grepl("^[[:digit:]]",inp_line)]
        stop("Line contains a Non Numeric Substring",
             paste(non_numerics, collapse = ", "))
      }

      invisible(as.numeric(inp_line))

    }
  )

)
