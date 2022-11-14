

#' Recruit Model
#'
#' @description
#' Recruitment Model Description
#'
#' @field model_num Model number
#' @field model_type Model Type
#' @field model_name Name of Recruitment Model
#'
#' @importFrom R6 R6Class
#' @export
RecruitModel <- R6Class(
  "RecruitModel",

  public = list (

    model_num = NULL,
    model_type = NULL,
    model_name = NULL,

    #' @description
    #' Creates a new instance of this class
    #'
    #' @param model_num Model Number
    #' @param model_type Model Type
    initialize = function (model_num, model_type) {
      self$model_num <- model_num
      self$model_type <- model_type
    }
  )
)


#' Null Recruitment UI Fallback Default
#' @inherit RecruitModel description
#' @export
NullRecruitModel <- R6Class(
  "NullRecruitModel",
  inherit = RecruitModel,
  public = list (

    #'@description
    #'Initialize
    #'
    initialize = function() {
      self$model_num = 0
      self$model_type = 0
    }
  )
)

#' Empirical Recritment Model Data
#' @inherit RecruitModel description
#'
#' @export
EmpiricalRecruitModel <- R6Class(
  "EmpiricalRecruitModel",
  inherit = RecruitModel,
  public = list (

    #' @field num_obs num obs
    num_obs = NULL,

    #' @field with_ssb with ssb
    with_ssb = NULL,

    #' @field low_bound Lowest significant number bound
    low_bound = 0.0001,

    #' @field obs_array Obs Array (data)
    obs_array = NULL,

    #'@description
    #'Creates an Empirical Recruit instance
    #'
    #' @param num_obs Number of Recruitmebt Observations
    #' @param with_ssb Empirical Recruitment includes Spawning
    #' Stock Biomass (SSB)
    #' @param model_num AGEPRO Recruitment Model Number
    #'
    initialize = function (model_num, num_obs, with_ssb = FALSE) {
      self$num_obs = num_obs
      self$with_ssb = with_ssb

      super$initialize(model_num, 1)
    },
    #'@description
    #'Create Obs table
    #'
    #' @param num_obs Number of Recruitmebt Observations
    #' @export
    new_obs_table = function () {
      message("Has SSB? ", self$with_ssb)
      message("Number of OBS: ", self$num_obs)

      self$obs_array <- rep("0",self$num_obs)
      if(self$with_ssb){
        self$obs_array <- matrix(c(self$obs_array,rep("0",self$num_obs)),
                                  nrow=2,ncol=self$num_obs)
      }
      print(self$obs_array)
    }
    #TODO:populate json data to class

  )
)
