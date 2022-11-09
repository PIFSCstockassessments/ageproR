

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




NullRecruitModel <- R6::R6Class(
  "NullRecruitModel",
  inherit = "RecruitModel",
  public = list (
    model_num = 0,
    model_type = 0,
    initialize = function() {
      self$model_num = 0
      self$model_type = 0
    }
  )
)

EmpiricalRecruitModel <- R6::R6Class(
  "EmpiricalRecruitModel",
  inherit = "RecruitModel",
  public = list (

  )
)
