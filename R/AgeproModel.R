
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


    #' @field general General Parameters
    general = NULL,

    #' @description
    #' Starts an instances of the AGEPRO Model
    #'
    #' @param ... Parameters to pass to GeneralParams
    #'
    initialize = function (...) {

      #TODO:Assert discard logicals
      #test_logical(discards)

      self$general <- GeneralParams$new(...)

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
