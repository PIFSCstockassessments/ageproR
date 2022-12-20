

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
#' @field seq_yrs Time Series of Projected Years
#'
#' @import cli
#' @importFrom R6 R6Class
#' @export
RecruitModel <- R6Class(
  "RecruitModel",

  public = list (

    model_num = NULL,
    model_group = NULL,
    model_name = NULL,
    seq_yrs = NULL,

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

      self$model_name <- "Null Recruitment"
      super$initialize(0, 0)
    }
  )
)

#' Empirical Recritment Model Data
#' @inherit RecruitModel description
#'
#' @template model_num
#' @template recruit_data
#'
#' @importFrom jsonlite toJSON
#' @importFrom checkmate test_int assert_integerish
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
    #' @param rec_points Number of Recuitment Observations
    #' @param with_ssb Empirical Recruitment includes Spawning
    #' Stock Biomass (SSB)
    #'
    initialize = function (model_num, rec_points, with_ssb = FALSE) {

      self$model_name = "Empirical Recruitment"

      #Handle/Check rec_points for single or array vector
      assert_integerish(rec_points)
      if(test_int(rec_points)){
        self$rec_points = rec_points
        self$seq_yrs = 1:rec_points
      }else{
        self$rec_points = length(rec_points)
        self$seq_yrs = rec_points
      }

      self$with_ssb = with_ssb

      super$initialize(model_num, 1)
      self$new_obs_table()
    },

    #'@description
    #'Create Obs table
    #'
    new_obs_table = function () {
      cli_ul()
      cli_li("Has SSB?  {.field {self$with_ssb}}")
      cli_li("Number of Recruitment Data Points: {.field  {self$rec_points}}")

      # Fill Data fill Default Values (0)
      if(self$with_ssb){
        self$rec_array <- matrix(rep("0",self$rec_points),
                                  nrow=2, ncol=self$rec_points)
      }else{
        self$rec_array <- matrix(rep("0",self$rec_points),
                                 nrow=1, ncol= self$rec_points)
      }
      #Set data matrix Column names to projected years time series array,
      colnames(self$rec_array) <- self$seq_yrs
      cat_print(self$rec_array)
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

    recruit_data = function () {
      return(list(points=self$rec_points,
           recruits=self$rec_array))
    }
  )
  #TODO: Set MaxRecObs
)

#' Parametric Recruitment Model
#' @inherit RecruitModel description
#'
#' @template model_num
#' @template parametric_curve
#' @template recruit_data
#'
#' @export
ParametricCurveModel <- R6Class(
  "ParametricRecruitModel",
  inherit = RecruitModel,
  public = list (

    #' @field alpha Stock Recruitment Parameter
    alpha = NULL,

    #' @field beta Stock Recruitment Parameter
    beta = NULL,

    #' @field variance Variance
    variance = NULL,

    #'@description
    #'Instantiate Parametric Recruitment Curve Model
    #'
    initialize = function (model_num,
                           alpha=0,
                           beta=0,
                           variance=0) {

      self$set(alpha, beta, variance)
      super$initialize(model_num,2)

    },

    #' @description
    #' Sets Parametric Curve parameters
    set = function (alpha, beta, variance) {

      #TODO: get Model Name

      self$alpha = alpha
      self$beta = beta
      self$variance = variance

      cli_ul()
      cli_li("Alpha: {.field {self$alpha}}")
      cli_li("Beta: {.field {self$beta}}")
      cli_li("Variance: {.field {self$variance}}")
    }

  ),
  active = list (

    recruit_data = function () {
      return(list(alpha=self$alpha,
                  beta=self$beta,
                  variance=self$variance))
    }
  )
)
