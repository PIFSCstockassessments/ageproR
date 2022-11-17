
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
#' @importFrom checkmate test_logical assert_number
AgeproModel <- R6Class(
  classname = "AgeproModel",

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

    #@field case_id Name of the AGEPRO model
    #case_id = NULL,

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
    initialize = function (yr_start = 0,
                           yr_end = 1,
                           age_begin= 0,
                           age_end=1,
                           num_fleets=1,
                           num_rec_models=1,
                           num_pop_sims=1000,
                           discards=FALSE,
                           seed=0) {

      #TODO:Assert discard logicals
      test_logiceal(discards)

      self$yr_start <- yr_start
      self$yr_end <- yr_end
      self$age_begin <- age_begin
      self$age_end <- age_end
      self$num_fleets <- num_fleets
      self$num_rec_models <- num_rec_models
      self$num_pop_sims <- num_pop_sims
      self$discards <- discards
      self$seed <- seed


    },

    #' @description
    #' Get json
    get_json = function () {

      if(!test_logical(self$discard)){
        assert_number(self$discard,lower=0,upper=1)
        self$discard <- as.numeric(self$discard)
      }

      general_json <- list(
        nFYear= self$yr_start,
        nXYear= self$yr_end,
        nFAge= self$age_begin,
        nXAge= self$age_end,
        nSims= self$num_pop_sims,
        nFleet= self$num_fleets,
        nRecModel= self$num_rec_model,
        discFlag= self$discard,
        seed= self$seed
      )
    }

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
