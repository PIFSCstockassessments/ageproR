

#' Recruit Model
#'
#' @description
#' Recruitment Model Description
#'
#' @template model_num
#'
#' @field model_num Model number
#' @field model_group Group type of Recruitment Model
#' @field model_name Name of Recruitment Model
#'
#' @importFrom R6 R6Class
#' @export
RecruitModel <- R6Class(
  "RecruitModel",

  public = list (

    model_num = NULL,
    model_group = NULL,
    model_name = NULL,

    #' @description
    #' Creates a new instance of this class
    #'
    #' @param model_group Model Type
    initialize = function (model_num, model_group) {
      self$model_num <- model_num
      self$model_group <- model_group
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

      super$initialize(0, 0)
    }
  )
)

#' Empirical Recritment Model Data
#' @inherit RecruitModel description
#'
#' @template model_num
#'
#' @importFrom jsonlite toJSON
#'
#' @export
EmpiricalRecruitModel <- R6Class(
  "EmpiricalRecruitModel",
  inherit = RecruitModel,
  public = list (

    #' @field rec_points num obs
    rec_points = NULL,

    #' @field with_ssb with ssb
    with_ssb = NULL,

    #' @field low_bound Lowest significant number bound
    low_bound = 0.0001,

    #' @field rec_array Recruitment Inupt Array (data)
    rec_array = NULL,

    #'@description
    #'Creates an Empirical Recruit instance
    #'
    #' @param rec_points Number of Recruitmebt Observations
    #' @param with_ssb Empirical Recruitment includes Spawning
    #' Stock Biomass (SSB)
    #'
    initialize = function (model_num, rec_points, with_ssb = FALSE) {
      self$rec_points = length(rec_points)
      self$with_ssb = with_ssb

      super$initialize(model_num, 1)
      self$new_obs_table()
    },
    #'@description
    #'Create Obs table
    #'
    new_obs_table = function () {
      message("Has SSB? ", self$with_ssb)
      message("Number of Recruitment Data Points: ", self$rec_points)

      # Fill Data fill Default Values (0)
      if(self$with_ssb){
        self$rec_array <- matrix(rep("0",self$rec_points),
                                  nrow=2, ncol=self$rec_points)
      }else{
        self$rec_array <- matrix(rep("0",self$rec_points),
                                 nrow=1, ncol= self$rec_points)
      }
      print(self$rec_array)
    },
    #' @description
    #' Returns Recuit data as JSON
    #'
    print_json = function () {
      #check
      toJSON(list(points=self$rec_points,
                  recruits=self$rec_array),
             pretty = TRUE,
             auto_unbox = TRUE )
    }

  ),
  active = list(

    #' @field recruit_data gets JSON-ready Recruit Model Data
    #'
    recruit_data = function () {
      return(list(points=self$rec_points,
           recruits=self$rec_array))
    }
  )
)
