
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

      if(!test_logical(self$general$discards)){
        #Assert for 0 and 1
        assert_number(self$general$discards,lower=0,upper=1)
      }
      self$general$discards <- as.numeric(self$general$discards)


      version_json <- list (
        legacyVer= private$str_legacy_ver,
        ver= private$str_ver
      )

      general_json <- list(
        nFYear= self$general$yr_start,
        nXYear= self$general$yr_end,
        nFAge= self$general$age_begin,
        nXAge= self$general$age_end,
        nSims= self$general$num_pop_sims,
        nFleet= self$general$num_fleets,
        nRecModel= self$general$num_rec_models,
        discFlag= self$general$discards,
        seed= self$general$seed
      )

      #TODO: Rename print_recruit to describe returning
      #recruitment object data
      recruit_json <- self$recruit$print_recruit(print_json=FALSE)


      agepro_json <- list("version"=version_json,
                          "general"=general_json,
                          "recruit"=recruit_json)


      # TODO: use the write() function to write JSON files
      toJSON(agepro_json,
             pretty =TRUE,
             auto_unbox =TRUE)

    },

    #' @description
    #' Write JSON file
    write_json = function () {
      tmp <- tempfile("agepro_", fileext = ".json")
      write(self$get_json(), tmp)

      message("Saved at :\n", tmp)
    }


  ),

  private = list (

    str_legacy_ver = "AGEPRO VERSION 4.0",
    str_ver = "4.0.0.0"
  )
)
