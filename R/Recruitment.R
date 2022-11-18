


#' @title AGEPRO Recruitment Parameter
#'
#' @description
#' Recruitment is the primary stochastic element in the AGEPRO model (Brodziak,
#' 2022). AGEPRO handles 21 Recruitment stochastic recruitment models for
#' projections. Please refer to the AGEPRO reference Manual for more detail on
#' these models.
#'
#' @template model_num
#' @template seq_years
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
Recruitment <- R6Class(
  "Recruitment",

  public = list(

    #' @field rec_fac multiplier to convert recruitment submodel's recruitment
    #' units to absolute numbers of fish
    rec_fac = 0,

    #' @field ssb_fac multiplier to convert recruitment submodel's Spawning
    #' Biomass (SSB) to absolute spawning weight of fish in kilograms
    ssb_fac = 0,

    #' @field max_rec_obs Recruitment submodel's maximum number of observations
    max_rec_obs = 1000,

    #' @field recruit_type Recruitment Type
    recruit_type = NULL,

    #' @field recruit_prob recruitment probabilities
    recruit_prob = NULL,

    #' @field model_collection_list List of recruitment models
    model_collection_list = NULL,


    #' @description
    #' Initializes the Recruitment Class
    #'
    initialize = function (model_num, seq_years) {

      #TODO:Assert params are numeric

      private$seq_yrs <- seq_years
      self$recruit_type <- model_num
      num_rec_models <- length(self$recruit_type)

      #TODO: Assert num_rec_models & recruit_type (model_num) vector are valid

      message(num_rec_models, " recruitment model(s) for ", private$seq_yrs, " year(s)")

      self$recruit_prob <- vector ("list", num_rec_models)
      self$model_collection_list <- vector ("list", num_rec_models)

      seq_rec_models <- 1:num_rec_models


      for (recruit in seq_rec_models) {

        # Fill recruit_prob
        # TODO: Check validity
        self$recruit_prob[[recruit]] <- rep("1", private$seq_yrs)

        #Add Recruitment Data
        self$model_collection_list[[recruit]] <-
          self$get_recruit_data(self$recruit_type[[recruit]], private$seq_yrs)

      }
      message("\nRecruitment Probability:")
      print(self$recruit_prob)
    },

    #'@description
    #'Gets Recruitment Data
    #'
    get_recruit_data = function(model_num, seq_years){

      if (model_num == 14) {
        EmpiricalRecruitModel$new(model_num,
                                  seq_years,
                                  with_ssb = FALSE)

      }

    },
    #'@description
    #' Prints out Recruitment object data as JSON
    #'
    #' @param print Option to print recruitment object as written in JSON
    #' format into console
    get_json = function (print = FALSE) {

      #Gather Recruit Model Data
      model_data_list <- vector("list", length(self$recruit_type))
      for(recruit in length(self$recruit_type)){
        model_data_list[[recruit]] <- self$model_collection_list[[recruit]][["recruit_data"]]
      }

      recruit_json <- list(
        recFac=self$rec_fac,
        ssbFac=self$ssb_fac,
        maxRecObs=self$max_rec_obs,
        type=self$recruit_type,
        prob=self$recruit_prob,
        modelData=model_data_list)

      if(print){
        toJSON(recruit_json,
               pretty =TRUE,
               auto_unbox =TRUE)
      }else{
        return(recruit_json)
      }

    }

  ), private = list (

    seq_yrs = NULL
  )
)







