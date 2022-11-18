
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


    #' @field general_par General Parameters
    general_par = NULL,

    #' @description
    #' Starts an instances of the AGEPRO Model
    #'
    #' @param ... Parameters to pass to GeneralParams
    #'
    initialize = function (...) {

      #TODO:Assert discard logicals
      #test_logical(discards)

      self$general_par <- GeneralParams$new(...)

    },

    #' @description
    #' Get json
    get_json = function () {

      if(!test_logical(general_par$discard)){
        assert_number(general_par$discard,lower=0,upper=1)
        general_par$discard <- as.numeric(general_par$discard)
      }

      version_json <- list (
        legacyVer= private$str_legacy_ver,
        ver= private$ver
      )

      general_json <- list(
        nFYear=  general_par$yr_start,
        nXYear= general_par$yr_end,
        nFAge= general_par$age_begin,
        nXAge= general_par$age_end,
        nSims= general_par$num_pop_sims,
        nFleet= general_par$num_fleets,
        nRecModel= general_par$num_rec_model,
        discFlag= general_par$discard,
        seed= general_par$seed
      )


    }


  ),

  private = list (

    str_legacy_ver = "AGEPRO VERSION 4.0",
    str_ver = "4.0.0.0"
  )
)
