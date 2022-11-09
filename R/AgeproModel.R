
#' @title R6 class representing AGEPRO model
#'
#' @description
#' AGEPRO model contains the projection time horizon, age class range, number
#' of fleets, recruitment, and unertainties
#'
#' @details
#' AGEPRO performs stochastic projections on exploited fisheries stock to
#' determine age-structured population over a time period. Brodziak, 2022
#'
#' @export
#' @importFrom R6 R6Class
AgeproModel <- R6Class(
  classname = "AgeproModel",

  public = list(
    #' @field yr_start First Year in Projection
    yr_start = 0,

    #' @field yr_end Last Year in Projection
    yr_end = 1,

    #' @field age_begin First Age Class
    age_begin = 0,

    #' @field age_end Last Age Class
    age_end = 1,

    #' @field num_fleets Number of Fleets
    num_fleets = 1,

    #' @field num_rec_models Number of Recruitment Models
    num_rec_models = 1,

    #' @field num_pop_sims Number of Population Simulations
    num_pop_sims = 1000,

    #' @field discards Are discards present?
    discards = FALSE,

    #' @field seed Psuedorandom number seed
    seed = 0,

    #' @field inp_file File name of opened/imported input file
    inp_file = NULL
  ),
  active = list(

    #' @field num_years Determines the number of years in projection by the (absolute) difference
    #' of the last and first year of projection.
    num_years = function () {
      abs(self$yr_end - self$yr_start) + 1
    },

    #' @field num_ages Determines number of ages by the (absolute) difference of the first and
    #' last age class.
    num_ages = function() {
      abs(self$age_begin - self$age_end) + 1
    },

    #' @field seq_years Returns a sequence of years from First year of projection
    seq_years = function() {
      seq(self$yr_start,self$yr_end)
    }

  )
)
