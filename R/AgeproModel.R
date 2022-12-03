
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
#' @template model_num
#' @template seq_years
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom checkmate test_logical assert_number
AgeproModel <- R6Class(
  classname = "AgeproModel",

  public = list(


    #' @field general General Parameters
    general = NULL,

    #' @field recruit AGEPRO Recruitmment Model(s)
    recruit = NULL,

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
    initialize = function (yr_start,
                           yr_end,
                           age_begin,
                           age_end,
                           num_fleets,
                           num_rec_models,
                           num_pop_sims,
                           discards=FALSE,
                           seed=sample.int(1e8,1)) {

      ## TODO TODO: Consider a helper function to create a new instance of
      ## AgeproModel

      #TODO:Assert discard logicals
      #test_logical(discards)


      self$general <- GeneralParams$new(yr_start,
                                        yr_end,
                                        age_begin,
                                        age_end,
                                        num_fleets,
                                        num_rec_models,
                                        num_pop_sims,
                                        discards,
                                        seed)

      self$recruit <- Recruitment$new(0,self$general$num_years)

    },

    ##' @description
    ##' Set model's Recruitment model
    set_recruit_model = function (model_num, seq_years) {

      self$recruit$set_recruit_data(model_num, seq_years)


    },

    #' @description
    #' Get json
    get_json = function () {

      if(!test_logical(general$discard)){
        assert_number(general$discard,lower=0,upper=1)
        general$discard <- as.numeric(general$discard)
      }

      version_json <- list (
        legacyVer= private$str_legacy_ver,
        ver= private$ver
      )

      general_json <- list(
        nFYear=  general$yr_start,
        nXYear= general$yr_end,
        nFAge= general$age_begin,
        nXAge= general$age_end,
        nSims= general$num_pop_sims,
        nFleet= general$num_fleets,
        nRecModel= general$num_rec_model,
        discFlag= general$discard,
        seed= general$seed
      )


    }


  ),

  private = list (

    str_legacy_ver = "AGEPRO VERSION 4.0",
    str_ver = "4.0.0.0"
  )
)
