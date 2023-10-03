
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
#' @template nline
#' @template delimiter
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom checkmate test_logical assert_number
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

    #' @field discards_present Are discards present?
    discards_present = NULL,

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
    #' @param num_pop_sims Number of population sims
    #' @param num_fleets Number of fleets
    #' @param num_rec_models Number of Recruit Modeles
    #' @param discards_present discards_present
    #' @param seed Random Number seed
    #'
    initialize = function(yr_start = 0,
                          yr_end = 2,
                          age_begin = 1,
                          age_end = 6,
                          num_pop_sims = 1000,
                          num_fleets = 1,
                          num_rec_models = 1,
                          discards_present = FALSE,
                          seed = sample.int(1e8, 1)) {

      private$cli_general_rule()
      # Discards: Assert logical format
      if (!test_logical(discards_present)) {
        assert_number(discards_present, lower = 0, upper = 1)
        discards_present <- as.logical(discards_present)
      }

      self$yr_start <- yr_start
      self$yr_end <- yr_end
      self$age_begin <- age_begin
      self$age_end <- age_end
      self$num_pop_sims <- num_pop_sims
      self$num_fleets <- num_fleets
      self$num_rec_models <- num_rec_models
      self$discards_present <- discards_present
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
      cli_li("Number of Population Simulations: {.val {self$num_pop_sims}}")
      cli_li("Number of Fleets: {.val {self$num_fleets}}")
      cli_li("Number of Recruitment Model(s): {.val {self$num_rec_models}}")
      cli_li("Discards are present: {.val {as.logical(self$discards_present)}} ")
      cli_li("Calculation Engine Random Number Seed: {.val {self$seed}}")
      invisible(self)
    },

    #' @description
    #' Reads General AGEPRO parameters from AGEPRO INP Input File
    read_inp_lines = function(inp_con, nline) {
      # Read an additional line from the file connection and split the string
      # into substrings by whitespace
      inp_line <-
        unlist(strsplit(readLines(inp_con, n = 1, warn = FALSE), " +"))

      nline <- nline + 1
      cli_alert("Line {nline} : Reading AGEPRO model GENERAL options ...")

      inp_line <- assert_numeric_substrings(inp_line)

      #TODO: Refactor
      self$yr_start <- inp_line[1]
      self$yr_end <- inp_line[2]
      self$age_begin <- inp_line[3]
      self$age_end <- inp_line[4]
      self$num_pop_sims <- inp_line[5]
      self$num_fleets <- inp_line[6]
      self$num_rec_models <- inp_line[7]
      self$discards_present <- inp_line[8]
      self$seed <- inp_line[9]

      self$print()

      return(nline)
    },

    #' @description
    #' Returns the values for the GENERAL keyword parameter formatted
    #' to the AGEPRO input file format.
    #'
    inplines_general = function(delimiter = "  ") {
      return(list(
        "[GENERAL]",
        paste(self$yr_start,
              self$yr_end,
              self$age_begin,
              self$age_end,
              self$num_pop_sims,
              self$num_fleets,
              self$num_rec_models,
              as.numeric(self$discards_present),
              self$seed,
              sep = delimiter)
      ))
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
    },

    #' @field json_list_general
    #' List of GENERAL keyword fields values, exportable to JSON.
    json_list_general = function() {

      #If discard Flag is numeric check if it is 0 or 1
      if (!test_logical(self$discards_present)) {
        #Assert for 0 and 1
        assert_number(self$discards_present, lower = 0, upper = 1)
      }

      return(list(
        nFYear = self$yr_start,
        nXYear = self$yr_end,
        nFAge = self$age_begin,
        nXAge = self$age_end,
        nSims = self$num_pop_sims,
        nFleet = self$num_fleets,
        nRecModel = self$num_rec_models,
        discFlag = as.numeric(self$discards_present),
        seed = self$seed
      ))
    }





  ), private = list(
    cli_general_rule = function() {
      d <- cli_div(theme = list(rule = list(
          color = "cyan",
          "line-type" = "double")))
      cli_rule("General")
      cli_end(d)
    }
  )

)
