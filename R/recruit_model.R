

#' Recruit Model
#'
#' @description
#' Recruitment Model Description
#'
#' @template model_num
#'
#' @import cli
#' @importFrom R6 R6Class
#'
#' @export
recruit_model <- R6Class(
  "recruit_model",
  private = list(
    .model_num = NULL,
    .model_group = NULL,
    .model_name = NULL,
    .projected_years = NULL
  ),
  public = list(

    #' @description
    #' Creates a new instance of this class
    #'
    initialize = function(model_num) {
      self$model_num <- model_num

    }
  ),
  active = list(

    #' @field model_num Model number
    model_num = function() {
      private$.model_num
    },

    #' @field model_group Group type of Recruitment Model
    model_group = function() {
      private$.model_group
    },

    #' @field model_name Name of Recruitment Model
    model_name = function() {
      private$.model_name
    },

    #' @field projected_years Time Series of Projected Years
    projected_years = function(value) {
      if(missing(value)) {
        private$.projected_years
      }else {
        assert_numeric(value)
        private$.projected_years <- value
      }
    }


  )
)


#' Null Recruitment UI Fallback Default
#' @inherit recruit_model description
#' @template elipses
null_recruit_model <- R6Class(
  "null_recruit_model",
  inherit = recruit_model,
  public = list(

    #'@description
    #'Initialize
    #'
    initialize = function() {

      self$model_name <- "NULL Recruitment"
      self$model_group <- 0
      super$initialize(0)

    },
    #' @description
    #' Prints out NULL Recruiment Model Data
    print = function(...) {
      cli_text("{self$model_name}")
      cli_alert_warning("Replace with a valid recruitment model before " +
                          "processing to AGEPRO calcualtion engine")
    }
  )
)

#' Empirical Recruitment Model Data
#' @inherit recruit_model description
#'
#' @template model_num
#' @template elipses
#'
#' @importFrom jsonlite toJSON
#' @importFrom checkmate test_int assert_integerish assert_logical
#' @export
empirical_recruit <- R6Class(
  "empirical_recruit",
  inherit = recruit_model,
  private = list(

    .low_bound = 0.0001,
    .with_ssb = FALSE

  ),
  public = list(

    #' @field rec_points num obs
    rec_points = NULL,

    #' @field rec_array Recruitment Inupt Array (data)
    rec_array = NULL,

    #'@description
    #'Creates an Empirical Recruit instance
    #'
    #' @param rec_points Number of Recruitment Observations
    #' @param with_ssb Empirical Recruitment includes Spawning
    #' Stock Biomass (SSB)
    #'
    initialize = function(model_num, rec_points, with_ssb = FALSE) {

      self$model_name <- "Empirical Recruitment Class"
      self$with_ssb <- with_ssb

      #Handle/Check rec_points for single or array vector
      #TODO: Modularize this rec_points check
      assert_integerish(rec_points)
      if (test_int(rec_points)) {
        self$rec_points <- rec_points
        self$seq_yrs <- 1:rec_points
      }else {
        self$rec_points <- length(rec_points)
        self$seq_yrs <- rec_points
      }

      self$model_group <- 1
      super$initialize(model_num)
      self$new_obs_table()
    },

    #'@description
    #'Create Obs table
    #'
    new_obs_table = function() {

      # Fill Data fill Default Values (0)
      if (self$with_ssb) {
        self$rec_array <- matrix(rep("0", self$rec_points),
                                  nrow = 2, ncol = self$rec_points)
      }else {
        self$rec_array <- matrix(rep("0", self$rec_points),
                                 nrow = 1, ncol = self$rec_points)
      }
      #Set data matrix Column names to projected years time series array,
      colnames(self$rec_array) <- self$seq_yrs

    },

    #' @description
    #' Prints out Recruitment Model
    print = function(...) {

      cli_text("{self$model_name}")
      cli_ul()
      cli_li("Has SSB?  {.val {self$with_ssb}}")
      cli_li("Number of Recruitment Data Points: {.val  {self$rec_points}}")
      cli_end()
      cli_alert_info("Observations:")
      cat_print(self$rec_array)

    },

    #' @description
    #' Returns Recuit data as JSON
    #'
    print_json = function() {
      #check
      toJSON(list(points = self$rec_points,
                  recruits = self$rec_array),
             pretty = TRUE,
             auto_unbox = TRUE)
    }

  ),
  active = list(

    #' @field with_ssb with ssb
    with_ssb = function(value) {
      if(missing(value)) {
        private$.with_ssb
      }else {
        assert_logical(value)
        private$.with_ssb <- value
      }
    },

    #' @field low_bound Lowest significant number bound
    low_bound = function() {
      private$.low_bound
    },

    #' @field recruit_data
    #' gets JSON-ready Recruit Model Data
    recruit_data = function() {
      return(list(points = self$rec_points,
           recruits = self$rec_array))
    },

    #' @field super_
    #' Binds the super class with the empirical_recruit child classes
    super_ = function() {
      super
    }

  )
  #TODO: Set MaxRecObs
)

#' Empirical Recruitment Distribution (Model #3)
#'
#' @template seq_years
#'
empirical_distribution_model <- R6Class(
  "empirical_distribution_model",
  inherit = empirical_recruit,
  public = list(
    #' @description
    #' Initialize the Empirical Recruitment Distribution Model
    initialize = function(seq_years) {

      super$initialize(3, seq_years, FALSE)
      self$model_name <- "Empirical Recruitment Distribution"

    }
  )
)

#' Empirical CDF of Recruitment (Model #14)
#'
#' @template seq_years
empirical_cdf_model <- R6Class(
  "empirical_cdf_model",
  inherit = empirical_recruit,
  public = list(
    #' @description
    #' Initialize the Empirical CDF Model
    initialize = function(seq_years) {

      super$initialize(14, seq_years, FALSE)
      self$model_name <- "Empirical Cumulative Distribution Function " +
        "of Recruitment"

    }
  )

)

#' Parametric Recruitment Model
#' @inherit recruit_model description
#'
#' @template model_num
#' @template parametric_parameters
#' @template elipses
#'
#' @importFrom checkmate assert_numeric
#'
#' @export
parametric_curve <- R6Class(
  "parametric_curve",
  inherit = recruit_model,
  private = list(

    .alpha = NULL,
    .beta = NULL,
    .variance = NULL

  ),
  active = list(

    #' @field recruit_data gets JSON-ready Recruit Model Data
    #'
    #'
    recruit_data = function() {
      return(list(alpha = private$.alpha,
                  beta = private$.beta,
                  variance = private$.variance))

    },

    #' @field alpha \cr
    #' Sets the Parametric Curve Parameter, alpha. Returns the
    #' current value if no argument was passed
    alpha = function(value) {
      if (missing(value)) {
        return(private$.alpha)
      }else {
        assert_numeric(value)
        private$.alpha <- value
      }
    },

    #' @field beta \cr
    #' Sets the Parametric Curve Parameter, beta. Returns the
    #' current value if no argument was passed
    #'
    beta = function(value) {
      if (missing(value)) {
        return(private$.beta)
      }else {
        assert_numeric(value)
        private$.beta <- value
      }
    },

    #' @field variance \cr
    #' Sets the Parametric Curve Parameter, variance. Returns the
    #' current value if no argument was passed.
    #'
    variance = function(value) {
      if (missing(value)) {
        return(private$.variance)
      }else {
        assert_numeric(value)
        private$.variance <- value
      }
    },

    #' @field super_ \cr
    #' Binds the super class to parametric_curve child classes
    super_ = function(value) {
      super
    }

  ),
  public = list(


    #'@description
    #'Instantiate Parametric Recruitment Curve Model
    #'
    initialize = function(model_num,
                           alpha = 0,
                           beta = 0,
                           variance = 0) {

      #Set to Active Bindings
      private$.alpha <- alpha
      private$.beta <- beta
      private$.variance <- variance

      #Set Model Number and Name
      self$model_group <- 2
      super$initialize(model_num)

    },

    #' @description
    #' Prints out Parametric Data
    #'
    print = function(...) {

      #Model Name
      cli_text("{self$model_name}")
      cli_ul()
      cli_li("Alpha: {.val {private$.alpha}}")
      cli_li("Beta: {.val {private$.beta}}")
      cli_li("Variance: {.val {private$.variance}}")
      cli_end()
    }

  )

)

#' Beverton-Holt w/ Lognormal Error
#'
#' @template parametric_parameters
#'
beverton_holt_curve_model <- R6Class(
  "beverton_holt_curve_model",
  inherit = parametric_curve,
  public = list(
    #' @description
    #' Initializes the Beverton Holt Curve Model
    initialize = function(alpha = 0,
                         beta = 0,
                         variance = 0) {

      super$initialize(5, alpha, beta, variance)
      self$model_name <- "Beverton-Holt Curve w/ Lognormal Error"

    }
  )
)

#'Ricker Curve #/ Lognormal Error (Model #6)
#'
#'@template parametric_parameters
#'
ricker_curve_model <- R6Class(
  "ricker_curve_model",
  inherit = parametric_curve,
  public = list(
    #' @description
    #' Initalizes the Ricker Curve Model
    initialize = function(alpha = 0,
                          beta = 0,
                          variance = 0) {

      super$initialize(6, alpha, beta, variance)
      self$model_name <- "Ricker Curve w/ Lognonormal Error"

    }
  )
)

#' Shepherd Curve with Lognormal Error (Model #7)
#'
#' @template parametric_parameters
#' @template elipses
#'
shepherd_curve_model <- R6Class(
  "shepherd_curve_model",
  inherit = parametric_curve,
  private = list(

    .alpha = NULL,
    .beta = NULL,
    .kpar = NULL,
    .variance = NULL

  ),
  public = list(


    #' @description
    #' Initializes the Shepherd Curve Model
    #'
    #' @param kpar kpar
    initialize = function(alpha = 0,
                           beta = 0,
                           kpar = 0,
                           variance = 0) {

      self$model_num <- 7
      self$model_group <- 2
      self$model_name <- "Shepherd Curve w/ Lognormal Error"

      #Set Active Bindings
      private$.alpha <- alpha
      private$.beta <- beta
      private$.kpar <- kpar
      private$.variance <- variance

    },

    #' @description
    #' Prints out Parametric Curve Data
    #'
    print = function(...) {

      cli_text("{self$model_name}")
      cli_ul()
      cli_li("Alpha: {.val {private$.alpha}}")
      cli_li("Beta: {.val {private$.beta}}")
      cli_li("k: {.val {private$.kpar}}")
      cli_li("Variance: {.val {private$.variance}}")
      cli_end()

    }

  ),
  active = list(

    #' @field kpar \cr
    #' Sets the Parametric Curve Parameter, k. Returns the
    #' current value if no argument was passed
    #'
    kpar = function(value) {
      if (missing(value)) {
        return(private$.kpar)
      }else {
        assert_numeric(value)
        private$.kpar <- value
      }
    }
  )
)
