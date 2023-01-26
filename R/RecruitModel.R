

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
#' @template elipses
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
      self$print()
    },
    #' @description
    #' Prints out NULL Recruiment Model Data
    print = function (...) {
      cli_alert_info("{self$model_name}")
      cli_alert_warning("Replace with a valid recruitment model before processing to AGEPRO calcualtion engine")
    }
  )
)

#' Empirical Recritment Model Data
#' @inherit RecruitModel description
#'
#' @template model_num
#' @template recruit_data
#' @template elipses
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
    #' @param rec_points Number of Recruitment Observations
    #' @param with_ssb Empirical Recruitment includes Spawning
    #' Stock Biomass (SSB)
    #'
    initialize = function (model_num, rec_points, with_ssb = FALSE) {

      self$model_name = "Empirical Recruitment Class"
      self$with_ssb = with_ssb

      #Handle/Check rec_points for single or array vector
      #TODO: Modularize this rec_points check
      assert_integerish(rec_points)
      if(test_int(rec_points)){
        self$rec_points = rec_points
        self$seq_yrs = 1:rec_points
      }else{
        self$rec_points = length(rec_points)
        self$seq_yrs = rec_points
      }

      super$initialize(model_num, 1)
      self$new_obs_table()
    },

    #'@description
    #'Create Obs table
    #'
    new_obs_table = function () {

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
      #cat_print(self$rec_array)
    },

    #' @description
    #' Prints out Recruitment Model
    print = function (...) {

      cli_ul()
      cli_alert_info("{self$model_name}")
      cli_li("Has SSB?  {.val {self$with_ssb}}")
      cli_li("Number of Recruitment Data Points: {.val  {self$rec_points}}")
      cli_li("Observations:")
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

#' Empirical Recruitment Distribution (Model #3)
#'
#' @template seq_years
#'
EmpiricalDistributionModel <- R6Class (
  "EmpiricalDistributionModel",
  inherit = EmpiricalRecruitModel,
  public = list(
    #' @description
    #' Initialize the Empirical Recruitment Distribution Model
    initialize = function (seq_years) {

      super$initialize(3, seq_years, FALSE)
      self$model_name = "Empirical Recruitment Distribution"
      self$print()
    }
  )
)

#' Empirical CDF of Recruitment (Model #14)
#'
#' @template seq_years
EmpiricalCDFModel <- R6Class(
  "EmpiricalCDFModel",
  inherit = EmpiricalRecruitModel,
  public = list (
    #' @description
    #' Initialize the Empirical CDF Model
    initialize = function (seq_years) {

      super$initialize(14, seq_years, FALSE)
      self$model_name = "Empirical Cumulative Distribution Function of Recruitment"
      self$print()
    }
  )

)

#' Parametric Recruitment Model
#' @inherit RecruitModel description
#'
#' @template model_num
#' @template parametric_curve
#' @template recruit_data
#' @template elipses
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
    #'
    set = function (alpha, beta, variance) {

      self$alpha = alpha
      self$beta = beta
      self$variance = variance
    },

    #' @description
    #' Prints out Parametric Data
    #'
    print = function(...) {

      #Model Name
      cli_ul()
      cli_alert_info("{self$model_name}")
      cli_li("Alpha: {.val {self$alpha}}")
      cli_li("Beta: {.val {self$beta}}")
      cli_li("Variance: {.val {self$variance}}")
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

#' Beverton-Holt w/ Lognormal Error
#'
#' @template parametric_curve
#'
BevertonHoltCurveModel <- R6Class (
  "BevertonHoltCurveModel",
  inherit = ParametricCurveModel,
  public = list (
    #' @description
    #' Initializes the Beverton Holt Curve Model
    initialize = function(alpha = 0,
                         beta = 0,
                         variance = 0) {

      super$initialize(5, alpha, beta, variance)
      self$model_name = "Beverton-Holt Curve w/ Lognormal Error"
      self$print()
    }
  )
)

#'Ricker Curve #/ Lognormal Error (Model #6)
#'
#'@template parametric_curve
#'
RickerCurveModel <- R6Class (
  "RickerCurveModel",
  inherit = ParametricCurveModel,
  public = list (
    #' @description
    #' Initalizes the Ricker Curve Model
    initialize = function(alpha = 0,
                          beta = 0,
                          variance = 0) {

      super$initialize(6, alpha, beta, variance)
      self$model_name = "Ricker Curve w/ Lognonormal Error"
      self$print()
    }
  )
)

#' Shepherd Curve with Lognormal Error (Model #7)
#'
#' @template parametric_curve
#' @template elipses
#'
ShepherdCurveModel <- R6Class (
  "ShepherdCurveModel",
  inherit = ParametricCurveModel,
  public = list (

    #' @field kpar Recruitment Parameter k
    kpar = NULL,

    #' @description
    #' Initializes the Shepherd Curve Model
    #'
    #' @param kpar kpar
    initialize = function (alpha = 0,
                           beta = 0,
                           kpar = 0,
                           variance = 0) {

      self$set(alpha, beta, kpar, variance)
      self$model_num = 7
      self$model_group = 2
      self$model_name = "Shepherd Curve w/ Lognormal Error"
      self$print()

    },

    #' @description
    #' Sets Parametric Recruitment Parameters
    #'
    #' @param kpar kpar
    set = function (alpha, beta, kpar, variance) {

      self$alpha = alpha
      self$beta = beta
      self$kpar = kpar
      self$variance = variance
    },

    #' @description
    #' Prints out Parameteric Curve Data
    #'
    print = function (...) {

      cli_ul()
      cli_li("Alpha: {.val {self$alpha}}")
      cli_li("Beta: {.val {self$beta}}")
      cli_li("k: {.val {self$kpar}}")
      cli_li("Variance: {.val {self$variance}}")

    }

  )
)
