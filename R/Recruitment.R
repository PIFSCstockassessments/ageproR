


#' @title AGEPRO Recruitment Parameter
#'
#' @description
#' Recruitment is the primary stochastic element in the AGEPRO model (Brodziak,
#' 2022). AGEPRO handles 21 Recruitment stochastic recruitment models for
#' projections. Please refer to the AGEPRO reference Manual for more detail on
#' these models.
#'
#' @export
#' @importFrom R6 R6Class
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


    #' @description
    #' Initializes the Recruitment Class
    #'
    #' @param num_rec_models Number of Recruitment Models
    #' @param seq_years Arrau representing the Projection Time Horizon
    initialize = function (num_rec_models, seq_years) {

      recruit_prob <- vector ("list", num_rec_models)

      seq_rec_models <- 1:num_rec_models

      for (recruit in seq_rec_models) {

        # Fill recruit_prob
        # TODO: Check validity
        recruit_prob[[recruit]] <- rep(1, seq_years)

        #Add Recruitment Data
        #RecruitModel$new


      }

    }

  )
)






#' set recruitment
#'
#' Writes the generalized AGEPRO Recruitment json data object.
#'
#' TODO: Recruitment models will call this function.
#'
#' @param model Recruitment model
#' @param recFac Recruitment Scaling Factor
#' @param ssbFac Spawning Stock Biomass (SSB) Scaling Factor
#' @param maxRecObs Limits on Maximun Records Observed
#' @param prob Recruitment Probability over each time period
#'
#'
set_recruitment <- function (model, recFac, ssbFac, maxRecObs, prob) {



  return(list("recFac": recFac,
              "ssbFac": ssbFac,
              "maxRecObs": maxRecObs,
              "type": model,
              "prob": prob) )
  # "recruit": {
  #   "recFac": 1,
  #   "ssbFac": 75,
  #   "maxRecObs": 1000000,
  #   "type": 14,
  #   "prob": ["1", "1", "1", "1", "1", "1"],
  #   "submodel": {
  #     "points": 53,
  #     "recruits": [73.5939, 78.1845, 70.6004, 62.1267, 66.0886, 69.9814, 49.9445, 70.4022, 42.6731, 85.2977, 48.2887, 98.1364, 76.867, 33.8211,
  #                  7.8195, 4.3288, 2.6275, 2.7917, 4.2174, 249.227, 6.5051, 2.5329, 1.9038, 1.7011, 1.5596, 2.2002, 52.7585, 2.4754, 2.8037, 10.179,
  #                  21.2349, 8.6637, 20.0313, 11.1925, 5.0913, 4.3675, 28.9894, 51.3917, 8.7334, 35.7165, 327.489, 73.3318, 35.0047, 22.4337, 24.9481,
  #                  32.1726, 34.4703, 29.245, 81.7098, 30.5807, 25.3895, 26.28, 30.1793]
  #   }
  # },

}
