

#' Recruit Model
#'
#' @description
#' Recruitment Model Description
#'
#' @template model_num
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_character assert_numeric
#'
#' @export
recruit_model <- R6Class(
  "recruit_model",
  private = list(
    .model_num = NULL,
    .model_group = NULL,
    .model_name = NULL,
    .projected_years = NULL,
    .length_projected_years = NULL
  ),
  public = list(

    #' @description
    #' Creates a new instance of this class
    #'
    initialize = function() {

    }
  ),
  active = list(

    #' @field model_num Model number
    model_num = function(value) {
      if (missing(value)) {
        private$.model_num
      }else {
        assert_numeric(value, lower = 0, upper = 21)
        private$.model_num <- value
      }

    },

    #' @field model_group Group type of Recruitment Model
    model_group = function(value) {
      if (missing(value)) {
        private$.model_group
      }else {
        assert_numeric(value, lower = 0, upper = 4)
        private$.model_group <- value
      }
    },

    #' @field model_name Name of Recruitment Model
    model_name = function(value) {
      if (missing(value)) {
        private$.model_name
      }else {
        assert_character(value)
        private$.model_name <- value
      }
    },

    #' @field projected_years Time Series of Projected Years
    projected_years = function(value) {
      if (missing(value)) {
        private$.projected_years
      }else {
        #Handle/Check 'value' for single or array vector
        assert_integerish(value)
        if (test_int(value)) {
          #Create vector 1 to 'value'
          private$.projected_years <- 1:value
        }else {
          private$.projected_years <- value
        }
      }
    },

    #' @field length_projected_years Length of projected_years counted as the
    #' the number of recruitment observations for some models.
    length_projected_years = function(value) {
      if (missing(value)) {
        private$.length_projected_years
      }else {
        #Handle/Check 'value' for single or array vector
        assert_integerish(value)
        if (test_int(value)) {
          private$.length_projected_years <- value
        }else {
          private$.length_projected_years <- length(value)
        }
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

      super$model_name <- "NULL Recruitment"
      super$model_group <- 0
      super$model_num <- 0

    },

    #' @description
    #' Prints out NULL Recruiment Model Data
    print = function(...) {
      cli_text("{private$.model_name}")
      cli_alert_warning(c("Replace with a valid recruitment model before ",
                          "processing to AGEPRO calcualtion engine"))
    }
  )
)

#' DEPRECATED Recruitment Model #9
#' (Time-Varying Empirical Recruitment Distribution)
#'
#' Handles an instance for deprecated recruitment model #9.
#'
#' @template elipses
#'
deprecated_recruit_model_9 <- R6Class(
  "deprecated_recruit_model_9",
  inherit = recruit_model,
  private = list(

    cli_recruit_danger = function() {
      d <- cli_div(class = "tmp", theme = list(.tmp = list(
        color = "red")))
      cli_text("{symbol$cross} {private$.model_name}")
      cli_end(d)
    }

  ),
  public = list(
    #' @description
    #' Initializes the class
    initialize = function() {

      super$model_name <-
        "Time-Varying Empirical Recruitment Distribution (DEPRECATED)"
      super$model_group <- 0
      super$model_num <- 9

    },

    #' @description
    #' Prints out the error
    print = function(...) {
      private$cli_recruit_danger()
      stop(paste0("Recruitment model #9 has been deperecated. ",
                              "Please use recruitment model #3 to implement ",
                              "Time-Varying Empirical Distribution."),
           call. = FALSE)



    }

  )
)

#' Empirical Recruitment Model Data
#' @inherit recruit_model description
#'
#' @template model_num
#' @template num_observations
#' @template elipses
#' @template inp_con
#'
#' @importFrom jsonlite toJSON
#' @importFrom checkmate test_int assert_integerish assert_logical assert_matrix
#' @importFrom tibble as_tibble
#' @export
empirical_recruit <- R6Class(
  "empirical_recruit",
  inherit = recruit_model,
  private = list(

    .low_bound = 0.0001,
    .with_ssb = FALSE,
    .model_group = 1,
    .observed_points = 0,
    .observations = NULL

  ),
  public = list(

    #'@description
    #'Creates an Empirical Recruit instance
    #'
    #' @param with_ssb Empirical Recruitment includes Spawning
    #' Stock Biomass (SSB)
    #'
    initialize = function(num_observations = 1, with_ssb = FALSE) {

      super$model_group <- 1

      #Set the number of observations used of the model projection
      if(!missing(num_observations)) {
        self$observed_points <- num_observations
      }

      if (!missing(with_ssb)) {
        private$.with_ssb <- with_ssb
      }

      self$new_obs_table()
    },

    #'@description
    #'Create Obs table
    #'
    new_obs_table = function() {

      # Fill Data fill Default Values (0)
      if (self$with_ssb) {
        self$observations <- matrix(rep(0, self$observed_points),
                                 ncol = 2,
                                 nrow = self$observed_points)
      }else {
        self$observations <- matrix(rep(0, self$observed_points),
                                 ncol = 1,
                                 nrow = self$observed_points)
      }
      #Set data matrix Column names to projected years time series array,
      colnames(self$observations) <- "recruit"

    },

    #' @description
    #' Prints out Recruitment Model
    print = function(...) {

      cli_text("{self$model_name}")
      cli_ul()
      cli_li("Has SSB?  {.val {self$with_ssb}}")
      cli_li(paste0("Number of Recruitment Data Points: ",
               "{.val {self$observed_points}}"))
      cli_end()
      cli_alert_info("Observations:")
      cat_print(as_tibble(self$observations))

    },

    #' @description
    #' Returns Recruit data as JSON
    #'
    print_json = function() {
      #check
      toJSON(list(points = self$observed_points,
                  recruits = self$observations),
             pretty = TRUE,
             auto_unbox = TRUE)
    },

    #' @description
    #' Read inp lines
    #'
    #' @param nline Line Number
    read_inp_lines = function(inp_con, nline) {

      # Read an additional line from the file connection and split the string
      # into substrings by whitespace and assign as observation recruits
      inp_line <- read_inp_numeric_line(inp_con)

      nline <- nline + 1
      cli_alert("Line {nline}: Observed points : {.val {inp_line}}...")

      #Validate input line holds single value for observation recruits
      assert_numeric(inp_line, len = 1)

      self$observed_points <- inp_line

      ## TODO: refactor reading observation table as function

      # Read an additional line from the file connection and split the string
      # into substrings by whitespace and assign as observation table
      inp_recruit <- read_inp_numeric_line(inp_con)

      nline <- nline + 1
      cli_alert("Line {nline} Observations ...")


      if(self$with_ssb) {

        # Read an additional line from the file connection and split the string
        # into substrings by whitespace and assign as observation table
        inp_ssb <- read_inp_numeric_line(inp_con)

        nline <- nline + 1
        cli_alert("Line {nline} ...")

        self$observations <- cbind(recruit=inp_recruit,
                                       ssb=inp_ssb)

      } else {
        self$observations <- cbind(recruit=inp_recruit)
      }

      print(as_tibble(self$observations), n = self$observed_points)


      return(nline)
    }

  ),
  active = list(

    #' @field with_ssb with ssb
    with_ssb = function(value) {
      if (missing(value)) {
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
      return(list(points = self$observed_points,
           recruits = self$observations))
    },

    #' @field observed_points
    #' Gets/Sets the number of observations used of the model projection
    observed_points = function(value) {
      if (missing(value)) {
        private$.observed_points
      }else {
        assert_integerish(value)
        if (test_int(value)) {
          private$.observed_points <- value
        }else {
          private$.observed_points <- length(value)
        }
      }
    },


    #' @field observations
    #' Recruitment Inupt Array (data)
    observations = function(value) {
      if(missing(value)) {
        private$.observations
      }else {
        assert_matrix(value, min.cols = 1, max.cols = 2)
        private$.observations <- value
      }
    },

    #' @field super_
    #' Binds the super class with the empirical_recruit child classes
    super_ = function(value) {
      super
    }

  )
  #TODO: Set MaxRecObs
)

#' Empirical Recruitment Distribution (Model #3)
#'
#' @template num_observations
#'
empirical_distribution_model <- R6Class(
  "empirical_distribution_model",
  inherit = empirical_recruit,
  public = list(
    #' @description
    #' Initialize the Empirical Recruitment Distribution Model
    initialize = function(num_observations) {

      super$with_ssb <- FALSE
      super$super_$model_num <- 3
      super$super_$model_name <- "Empirical Recruitment Distribution"
      super$initialize(num_observations)

    }
  )
)

#' Empirical CDF of Recruitment (Model #14)
#'
#' @template num_observations
empirical_cdf_model <- R6Class(
  "empirical_cdf_model",
  inherit = empirical_recruit,
  public = list(
    #' @description
    #' Initialize the Empirical CDF Model
    initialize = function(num_observations = 1) {

      #Set the number of observations used of the model projection
      if(!missing(num_observations)) {
        self$observed_points <- num_observations
      }

      super$with_ssb <- FALSE
      super$super_$model_num <- 14
      super$super_$model_name <-
        "Empirical Cumulative Distribution Function of Recruitment"
      super$initialize(num_observations)

    }
  )

)




#'Two-Stage Empirical Recruitment Base
#'
#' @template inp_con
#' @template elipses
#' @template two_stage_empirical_parameters
#'
#' @importFrom checkmate assert_numeric
#'
two_stage_empirical_recruit <- R6Class(
  "two_stage_empirical_recruit",
  inherit = empirical_recruit,
  private = list(

    .num_low_recruits = NULL,
    .num_high_recruits = NULL,
    .ssb_cutoff = NULL,
    .low_recruitment = NULL,
    .high_recruitment = NULL,
    .with_ssb = FALSE

  ), public = list (

    #' @description
    #' Initialize the Empirical CDF Model
    #'
    #' @param with_ssb
    #' flag to include Spawning Stock Biomass in Observations
    initialize = function(low_recruits = 1,
                          high_recruits = 1,
                          with_ssb = FALSE) {

      #Set the number of observations used of the model projection
      self$num_low_recruits <- low_recruits
      self$num_high_recruits <- high_recruits

      private$.with_ssb <- with_ssb


      super$initialize((self$num_low_recruits + self$num_high_recruits),
                       with_ssb = private$.with_ssb)

      #Initialize Low and High stage recruitment vector
      self$low_recruitment <- self$new_recruitment_matrix(low_recruits)
      self$high_recruitment <- self$new_recruitment_matrix(high_recruits)

      self$ssb_cutoff <- 0

    },

    #' @description
    #' Creates a recruitment matrix
    #'
    #' @param recruit_points Number of Empirical Observation Records
    new_recruitment_matrix = function(recruit_points) {

      # Fill Data fill Default Values (0)
      if (self$with_ssb) {
        recruitment_matrix <- matrix(rep(0, recruit_points),
                                    ncol = 2,
                                    nrow = recruit_points)

        #Set data matrix Column names to projected years time series array,
        colnames(recruitment_matrix) <- c("recruit", "ssb")

      }else {
        recruitment_matrix <- matrix(rep(0, recruit_points),
                                    ncol = 1,
                                    nrow = recruit_points)
        #Set data matrix Column names to projected years time series array,
        colnames(recruitment_matrix) <- "recruit"
      }

      return(recruitment_matrix)
    },


    #' @description
    #' Reads the two State Empirical model data from AGEPRO Input file
    #'
    #' @param nline Line Number
    #'
    read_inp_lines = function(inp_con, nline) {
      # Read an additional line from the file connection and split the string
      # into substrings by whitespace and assign as observation recruits
      inp_line <- read_inp_numeric_line(inp_con)

      nline <- nline + 1
      cli_alert("Line {nline}: Number of low & high recruits ...")
      cli_text("{inp_line}")

      self$num_low_recruits <- inp_line[1]
      self$num_high_recruits <- inp_line[2]

      ## low_recruitment
      # Read an additional line from the file connection and split the string
      # into substrings by whitespace and assign as observation table
      inp_low_recruits <- read_inp_numeric_line(inp_con)
      self$low_recruitment <- cbind(recruit=inp_low_recruits)

      nline <- nline + 1
      cli_alert_info("Line {nline} Low Recruitment ...")
      print(as_tibble(self$low_recruitment))


      ## high_recruitment
      # Read an additional line from the file connection and split the string
      # into substrings by whitespace and assign as observation table
      inp_high_recruits <- read_inp_numeric_line(inp_con)
      self$high_recruitment <- cbind(recruit=inp_high_recruits)

      nline <- nline + 1
      cli_alert_info("Line {nline} High Recruitment ...")
      print(as_tibble(self$high_recruitment))

      ## ssb_cutoff
      # Read an additional line from the file connection and split the string
      # into substrings by whitespace and assign as observation recruits
      inp_line <- read_inp_numeric_line(inp_con)
      self$ssb_cutoff <- inp_line

      nline <- nline + 1
      cli_alert("Line {nline}: SSB cutoff: {.val {self$ssb_cutoff}}")

      return(nline)

    },

    #' @description
    #' Prints out Recruitment Model
    print = function(...) {

      cli_text("{self$model_name}")
      cli_ul()
      cli_li("Include state SSB vector? {.val {self$with_ssb}}")
      cli_li("SSB cutoff level: {.val {self$ssb_cutoff}}")
      cli_li("Number of Recruitment Data Points: ")
      a <- cli_ul()
      cli_li("Number of Low recruitment: {.val {self$num_low_recruits}}")
      cli_li("Number of High recruitment: {.val {self$num_high_recruits}}")
      cli_end(a)
      cli_end()
      cli_alert_info("Observations:")
      cli_text("Low recruitment")
      cat_print(as_tibble(self$low_recruitment))
      cli_text("High recruitment")
      cat_print(as_tibble(self$high_recruitment))


    }

  ), active = list(

    #' @field num_low_recruits
    #' Number of Low State Recruitments
    num_low_recruits = function(value){
      if(missing(value)){
        private$.num_low_recruits
      }else{
        assert_numeric(value, lower = 1, len = 1)
        private$.num_low_recruits <- value
      }
    },

    #' @field num_high_recruits
    #' Number of high State Recruitments
    num_high_recruits = function(value){
      if(missing(value)){
        private$.num_high_recruits
      }else {
        assert_numeric(value, lower = 1, len = 1)
        private$.num_high_recruits <- value
      }
    },

    #' @field ssb_cutoff
    #' Cutoff level of spawning Biomass
    ssb_cutoff = function(value){
      if(missing(value)){
        private$.ssb_cutoff
      }else {
        #Validate input holds single value
        assert_numeric(value, len = 1 )
        private$.ssb_cutoff <- value
      }
    },

    #' @field low_recruitment
    #' Vector of Low State Recruitments per Spawning Biomass
    low_recruitment = function(value){
      if(missing(value)){
        private$.low_recruitment
      }else {
        assert_numeric(value)
        private$.low_recruitment <- value
      }
    },

    #' @field high_recruitment
    #' Vector of High State Recruitments per Spawning Biomass
    high_recruitment = function(value){
      if(missing(value)){
        private$.high_recruitment
      }else {
        assert_numeric(value)
        private$.high_recruitment <- value
      }
    }

  )

)

#'Two-Stage Empirical Recruits Per Spawning Biomass Distribution (Model #4)
#'
#' @template elipses
#' @template two_stage_empirical_parameters
#'
two_stage_empirical_ssb <- R6Class(
  "two_stage_empirical_ssb",
  inherit = two_stage_empirical_recruit,
  public = list (
    #' @description
    #' Initialize the Empirical CDF Model
    #'
    initialize = function(low_recruits = 1, high_recruits = 1) {

      #Set the number of observations used of the model projection
      self$num_low_recruits <- low_recruits
      self$num_high_recruits <- high_recruits

      super$super_$model_num <- 4
      super$super_$model_name <-
        "Two-Stage Empirical Recruits Per Spawning Biomass Distribution"
      super$initialize(self$num_low_recruits,
                       self$num_high_recruits,
                       with_ssb = TRUE)
    }

  )
)

#'Two-Stage Empirical Cumulative Distribution Function of Recruitment
#'(Recruit #15)
#'
#' @template two_stage_empirical_parameters
#'
two_stage_empirical_cdf <- R6Class(
  "two_stage_empirical_cdf",
  inherit = two_stage_empirical_recruit,
  public = list (
    #' @description
    #' Initialize the Empirical CDF Model
    #'
    initialize = function(low_recruits = 1, high_recruits = 1) {

      #Set the number of observations used of the model projection
      self$num_low_recruits <- low_recruits
      self$num_high_recruits <- high_recruits

      super$super_$model_num <- 15
      super$super_$model_name <-
        "Two-Stage Empirical Cumulative Distribution Function of Recruitment"
      super$initialize(self$num_low_recruits,
                       self$num_high_recruits,
                       with_ssb = FALSE)
    }

  )
)

#' Parametric Recruitment Model
#' @inherit recruit_model description
#'
#' @template model_num
#' @template parametric_parameters
#' @template elipses
#' @template inp_con
#'
#' @importFrom checkmate assert_numeric
#'
#' @export
parametric_curve <- R6Class(
  "parametric_curve",
  inherit = recruit_model,
  private = list(

    .alpha = 0,
    .beta = 0,
    .variance = 0,
    .model_group = 2

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

    #' @field alpha
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

    #' @field beta
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

    #' @field variance
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

    #' @field model_group Model group Number
    #'
    model_group = function() {
      private$.model_group
    },


    #' @field super_
    #' Binds the super class to parametric_curve child classes
    super_ = function(value) {
      super
    }

  ),
  public = list(


    #'@description
    #'Instantiate Parametric Recruitment Curve Model
    #'
    initialize = function(alpha = 0,
                           beta = 0,
                           variance = 0) {

      #Set to Active Bindings
      if (!missing(alpha)) {
        private$.alpha <- alpha
      }

      if (!missing(beta)) {
        private$.beta <- beta
      }

      if (!missing(variance)) {
        private$.variance <- variance
      }



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
    },

    #' @description
    #' Reads Parametric Curve model data from AGEPRO Input file
    #'
    #' @param nline Line Number
    #'
    read_inp_lines = function (inp_con, nline){
      # Read an additional line from the file connection and split the string
      # into substrings by whitespace
      inp_line <- read_inp_numeric_line(inp_con)

      nline <- nline + 1
      cli_alert("Line {nline} ...")

      # Assign substrings
      self$alpha <- inp_line[1]
      self$beta <- inp_line[2]
      self$variance <- inp_line[3]

      return(nline)
    }

  )

)

#' Beverton-Holt w/ Lognormal Error (Model #5)
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

      super$initialize(alpha, beta, variance)
      super$super_$model_num <- 5
      super$super_$model_name <- "Beverton-Holt Curve w/ Lognormal Error"

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

      super$initialize(alpha, beta, variance)
      super$super_$model_num <- 6
      super$super_$model_name <- "Ricker Curve w/ Lognonormal Error"


    }
  )
)

#' Shepherd Curve with Lognormal Error (Model #7)
#'
#' @template parametric_parameters
#' @template elipses
#' @template inp_con
#'
shepherd_curve_model <- R6Class(
  "shepherd_curve_model",
  inherit = parametric_curve,
  private = list(

    .alpha = 0.1,
    .beta = 0.1,
    .kpar = 0.1,
    .variance = 0.1

  ),
  public = list(


    #' @description
    #' Initializes the Shepherd Curve Model
    #'
    #' @param kpar kpar
    initialize = function(alpha = 0.1,
                           beta = 0.1,
                           kpar = 0.1,
                           variance = 0.1) {

      #Set Active Bindings
      if (!missing(kpar)) {
        private$.kpar <- kpar
      }

      super$initialize(alpha, beta, variance)
      super$super_$model_num <- 7
      super$super_$model_name <- "Shepherd Curve w/ Lognormal Error"



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

    },

    #' @description
    #' Reads Parametric Curve model data from AGEPRO Input file
    #'
    #' @param nline Line Number
    #'
    read_inp_lines = function (inp_con, nline){
      # Read an additional line from the file connection and split the string
      # into substrings by whitespace
      inp_line <- read_inp_numeric_line(inp_con)

      nline <- nline + 1
      cli_alert("Line {nline} ...")

      # Assign substrings
      self$alpha <- inp_line[1]
      self$beta <- inp_line[2]
      self$kpar <- inp_line[3]
      self$variance <- inp_line[4]


      return(nline)
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
