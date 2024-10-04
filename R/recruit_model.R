

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
recruit_model <- R6Class(
  "recruit_model",
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

  ),
  private = list(
    .model_num = NULL,
    .model_group = NULL,
    .model_name = NULL,
    .projected_years = NULL,
    .length_projected_years = NULL

  )

)


#' Null Recruitment UI Fallback Default
#'
#' @inherit recruit_model description
#'
#' @template elipses
#' @template delimiter
#'
#' @export
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
    },

    #' @description
    #' Function container to export recruitment model data to AGEPRO input model
    #' file lines, but since NULL recruitment is not a valid recruitment model
    #' type for the AGEPRO calculation engine, an error will thrown to indicate
    #' NuLL recruitment
    #'
    inp_lines_recruit_data = function(delimiter= " ") {
      stop("Found invalid NULL Recruruitment recruitment model data.",
          call. = FALSE)
    }

  )
)

#' DEPRECATED Recruitment Model #9
#' (Time-Varying Empirical Recruitment Distribution)
#'
#' Handles an instance for deprecated recruitment model #9.
#'
#' @template elipses
#' @template delimiter
#' @export
#'
deprecated_recruit_model_9 <- R6Class(
  "deprecated_recruit_model_9",
  inherit = recruit_model,
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
      stop(private$.err_model_deprecated, call. = FALSE)

    },

    #' @description
    #' Function container to export recruitment model data to AGEPRO input model
    #' file lines, but since this model is DEPRECATED; not a valid recruitment
    #' model type for the AGEPRO calculation engine, an error will thrown.
    #'
    inp_lines_recruit_data = function(delimiter= " ") {
      stop(private$.err_model_deprecated, call. = FALSE)
    }

  ),
  private = list(

    .err_model_deprecated =
      paste0("Recruitment Model #9 is DEPRECATED",
             "Please use the Empirical Recruitment Distribution Model ",
             "(#3) with Time-Variance."),

    cli_recruit_danger = function() {
      d <- cli_div(class = "tmp", theme = list(.tmp = list(
        color = "red")))
      cli_text("{symbol$cross} {private$.model_name}")
      cli_end(d)
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
#' @template nline
#' @template delimiter
#'
#' @importFrom jsonlite toJSON
#' @importFrom checkmate test_int assert_integerish assert_logical assert_matrix
#' @importFrom tibble as_tibble
#'
empirical_recruit <- R6Class(
  "empirical_recruit",
  inherit = recruit_model,
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
      if (!missing(num_observations)) {
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

        #Set data matrix Column names to projected years time series array,
        colnames(self$observations) <- c("recruit", "ssb")

      }else {
        self$observations <- matrix(rep(0, self$observed_points),
                                 ncol = 1,
                                 nrow = self$observed_points)

        #Set data matrix Column names to projected years time series array,
        colnames(self$observations) <- "recruit"
      }


    },

    #' @description
    #' Prints out Recruitment Model
    print = function(...) {

      #Model Name
      cli::cli_alert_info("{self$model_name}")
      cli_ul()
      cli_li("Has SSB?  {.val {self$with_ssb}}")
      cli_li(paste0("Number of Recruitment Data Points: ",
               "{.val {self$observed_points}}"))
      cli_alert_info("Observations:")
      cat_line(paste0("  ", capture.output(
        tibble::as_tibble(self$observations, .name_repair = "minimal"))))
      cli_end()


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
    read_inp_lines = function(inp_con, nline) {

      #Model Name
      cli::cli_text("{.emph {.field {self$model_name}}}")

      # Read an additional line from the file connection and split the string
      # into substrings by whitespace and assign as observation recruits
      inp_line <- read_inp_numeric_line(inp_con)

      nline <- nline + 1
      cli_alert("Line {nline}: observed_points: {.val {inp_line}} ")

      #Validate input line holds single value for observation recruits
      assert_numeric(inp_line, len = 1)

      self$observed_points <- inp_line

      ## TODO: refactor reading observation table as function

      # Read an additional line from the file connection and split the string
      # into substrings by whitespace and assign as observation table
      inp_recruit <- read_inp_numeric_line(inp_con)

      nline <- nline + 1

      cli::cli_alert(c("Line {nline}: ",
                       "observations (recruit): ",
                       "{.val {inp_recruit}} ",
                       "{.emph ({self$observed_points} observation{?s})} "))


      if (self$with_ssb) {

        # Read an additional line from the file connection and split the string
        # into substrings by whitespace and assign as observation table
        inp_ssb <- read_inp_numeric_line(inp_con)

        nline <- nline + 1

        cli::cli_alert(c("Line {nline}: ",
                         "observations (ssb): ",
                         "{.val {inp_ssb}} ",
                         "{.emph ({self$observed_points} observation{?s})} "))

        self$observations <- cbind(recruit = inp_recruit,
                                       ssb = inp_ssb)

      } else {
        self$observations <- cbind(recruit = inp_recruit)
      }

      return(nline)
    },


    #' @description
    #' Exports RECRUIT submodel data for empirical recruitment types
    #' to AGEPRO input file lines.
    inp_lines_recruit_data = function(delimiter = " ") {

      #Observation Matrix columns are labeled "recruit" and "ssb"
      if(self$with_ssb){
        return(list(
          self$observed_points,
          paste(self$observations[,"recruit"], collapse = delimiter),
          paste(self$observations[,"ssb"], collabse = delimiter)
        ))

      }else{
        return(list(
          self$observed_points,
          paste(self$observations[,"recruit"], collapse = delimiter)
        ))
      }


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

    #' @field json_recruit_data
    #' gets JSON-ready Recruit Model Data
    json_recruit_data = function() {
      return(list(points = self$observed_points,
                  recruits = subset(self$observations,
                                    select = "recruit", drop = TRUE) ))
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
      if (missing(value)) {
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

  ),
  private = list(

    .low_bound = 0.0001,
    .with_ssb = FALSE,
    .model_group = 1,
    .observed_points = 0,
    .observations = NULL

  )
)

#' Empirical Recruitment Distribution (Model #3)
#'
#' @template num_observations
#' @export
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
#' @export
empirical_cdf_model <- R6Class(
  "empirical_cdf_model",
  inherit = empirical_recruit,
  public = list(
    #' @description
    #' Initialize the Empirical CDF Model
    initialize = function(num_observations = 2) {

      self$observed_points <- num_observations

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
#' @template nline
#' @template delimiter
#'
#' @importFrom checkmate assert_numeric
#' @export
#'
two_stage_empirical_recruit <- R6Class(
  "two_stage_empirical_recruit",
  inherit = empirical_recruit,
  public = list(

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

      #Initialize Low and High stage recruitment vector
      cli_alert(paste0("Generating default low state recruitment of ",
                       "{no(self$num_low_recruits)} row{?s}"))
      self$low_recruitment <-
        self$new_recruitment_matrix(self$num_low_recruits)

      cli_alert(paste0("Generating default high state recruitment of ",
                       "{no(self$num_low_recruits)} row{?s}"))
      self$high_recruitment <-
        self$new_recruitment_matrix(self$num_high_recruits)

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
    read_inp_lines = function(inp_con, nline) {

      #Model Name
      cli::cli_text("{.emph {.field {self$model_name}}}")

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
      self$low_recruitment <- cbind(recruit = inp_low_recruits)

      nline <- nline + 1
      cli_alert("Line {nline} Low Recruitment ...")
      cat_line(capture.output(
        tibble::as_tibble(self$low_recruitment, .name_repair = "minimal")))


      ## high_recruitment
      # Read an additional line from the file connection and split the string
      # into substrings by whitespace and assign as observation table
      inp_high_recruits <- read_inp_numeric_line(inp_con)
      self$high_recruitment <- cbind(recruit = inp_high_recruits)

      nline <- nline + 1
      cli_alert("Line {nline} High Recruitment ...")
      cat_line(capture.output(
        tibble::as_tibble(self$high_recruitment, .name_repair = "minimal")))

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
    #' Exports RECRUIT submodel data for two-stage empirical recruitment types
    #' to AGEPRO input file lines.
    inp_lines_recruit_data = function(delimiter = " ") {
      return(list(
        paste(self$num_low_recruits,self$num_high_recruits),
        paste(self$low_recruitment, collapse =  delimiter),
        paste(self$high_recruitment, collapse = delimiter),
        self$ssb_cutoff
      ))
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
      cli_alert_info("Observations:")
      cli_text("Low recruitment")
      cat_line(paste0("  ", capture.output(
        tibble::as_tibble(self$low_recruitment, .name_repair = "minimal"))))
      cli_text("High recruitment")
      cat_line(paste0("  ", capture.output(
        tibble::as_tibble(self$high_recruitment, .name_repair = "minimal"))))
      cli_end()



    }

  ), active = list(

    #' @field num_low_recruits
    #' Number of Low State Recruitments
    num_low_recruits = function(value) {
      if (missing(value)) {
        private$.num_low_recruits
      }else {
        assert_numeric(value, lower = 1, len = 1)
        private$.num_low_recruits <- value
      }
    },

    #' @field num_high_recruits
    #' Number of high State Recruitments
    num_high_recruits = function(value) {
      if (missing(value)) {
        private$.num_high_recruits
      }else {
        assert_numeric(value, lower = 1, len = 1)
        private$.num_high_recruits <- value
      }
    },

    #' @field ssb_cutoff
    #' Cutoff level of spawning Biomass
    ssb_cutoff = function(value) {
      if (missing(value)) {
        private$.ssb_cutoff
      }else {
        #Validate input holds single value
        assert_numeric(value, len = 1)
        private$.ssb_cutoff <- value
      }
    },

    #' @field low_recruitment
    #' Vector of Low State Recruitments per Spawning Biomass
    low_recruitment = function(value) {
      if (missing(value)) {
        private$.low_recruitment
      }else {
        assert_numeric(value)
        private$.low_recruitment <- value
      }
    },

    #' @field high_recruitment
    #' Vector of High State Recruitments per Spawning Biomass
    high_recruitment = function(value) {
      if (missing(value)) {
        private$.high_recruitment
      }else {
        assert_numeric(value)
        private$.high_recruitment <- value
      }
    },

    #' @field json_recruit_data
    #' gets JSON-ready Recruit Model Data
    json_recruit_data = function() {
      return(list(
        numLowRecruits = self$num_low_recruits,
        numHighRecruits = self$num_high_recruits,
        lowRecruits = self$low_recruitment,
        highRecruits = self$high_recruitment,
        ssbCutoff = self$ssb_cutoff
      ))
    }

  ),
  private = list(

    .num_low_recruits = NULL,
    .num_high_recruits = NULL,
    .ssb_cutoff = NULL,
    .low_recruitment = NULL,
    .high_recruitment = NULL,
    .with_ssb = FALSE

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
  public = list(
    #' @description
    #' Initialize the Empirical CDF Model
    #'
    initialize = function(low_recruits = 2, high_recruits = 2) {

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

  ),
  active = list(

    #' @field json_recruit_data
    #' gets JSON-ready Recruit Model Data
    json_recruit_data = function() {
      return(list(
        numLowRecruits = self$num_low_recruits,
        numHighRecruits = self$num_high_recruits,
        #Use subset to get JSON list in a list object structure
        lowRecruits = subset(self$low_recruitment,
                             select = "recruit", drop = TRUE),
        lowSSB = subset(self$low_recruitment,
                        select = "ssb", drop = TRUE),
        highRecruits = subset(self$high_recruitment,
                              select = "recruit", drop = TRUE),
        highSSB = subset(self$high_recruitment,
                         select = "ssb", drop = TRUE),
        ssbCutoff = self$ssb_cutoff
      ))
    }
  )
)

#'Two-Stage Empirical Cumulative Distribution Function of Recruitment
#'(Recruit #15)
#'
#' @template two_stage_empirical_parameters
#' @export
#'
two_stage_empirical_cdf <- R6Class(
  "two_stage_empirical_cdf",
  inherit = two_stage_empirical_recruit,
  public = list(
    #' @description
    #' Initialize the Empirical CDF Model
    #'
    initialize = function(low_recruits = 2, high_recruits = 2) {

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

  ),
  active = list(

    #' @field json_recruit_data
    #' gets JSON-ready Recruit Model Data
    json_recruit_data = function() {
      return(list(
        numLowRecruits = self$num_low_recruits,
        numHighRecruits = self$num_high_recruits,
        #Use subset to get JSON list in a list object structure
        lowRecruits = subset(self$low_recruitment,
                             select = "recruit", drop = TRUE),
        highRecruits = subset(self$high_recruitment,
                              select = "recruit", drop = TRUE),
        ssbCutoff = self$ssb_cutoff
      ))
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
#' @template nline
#' @template delimiter
#'
#' @importFrom checkmate assert_numeric
#'
parametric_curve <- R6Class(
  "parametric_curve",
  inherit = recruit_model,
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
    #' Exports RECRUIT submodel data for parametric curve recruitment
    #' to AGEPRO input file lines.
    #'
    inp_lines_recruit_data = function(delimiter = " ") {
      return(list(paste(self$alpha,
                        self$beta,
                        self$variance,
                        sep = delimiter)))
    },

    #' @description
    #' Prints out Parametric Data
    #'
    print = function(...) {

      #Model Name
      cli::cli_alert_info("{self$model_name}")
      cli_ul()
      cli_li("Alpha: {.val {private$.alpha}}")
      cli_li("Beta: {.val {private$.beta}}")
      cli_li("Variance: {.val {private$.variance}}")
      cli_end()
    },

    #' @description
    #' Reads Parametric Curve model data from AGEPRO Input file
    #'
    read_inp_lines = function(inp_con, nline) {

      #Model Name
      cli::cli_text("{.emph {.field {self$model_name}}}")

      # Read an additional line from the file connection and split the string
      # into substrings by whitespace
      inp_line <- read_inp_numeric_line(inp_con)

      nline <- nline + 1
      cli_alert("Line {nline} ...")

      # Assign substrings
      self$alpha <- inp_line[1]
      self$beta <- inp_line[2]
      self$variance <- inp_line[3]

      #self$print()
      cli_ul()
      cli_li("Alpha: {.val {private$.alpha}}")
      cli_li("Beta: {.val {private$.beta}}")
      cli_li("Variance: {.val {private$.variance}}")
      cli_end()

      return(nline)
    }

  ),
  active = list(

    #' @field json_recruit_data
    #' Returns JSON-ready Recruit Model Data'
    #'
    json_recruit_data = function() {
      return(list(
        alpha = self$alpha,
        beta = self$beta,
        variance = self$variance))
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
  private = list(

    .alpha = 0,
    .beta = 0,
    .variance = 0,
    .model_group = 2

  )


)

#' Beverton-Holt w/ Lognormal Error (Model #5)
#'
#' @template parametric_parameters
#' @export
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
#' @template parametric_parameters
#' @export
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
#' @template nline
#' @template delimiter
#' @export
#'
shepherd_curve_model <- R6Class(
  "shepherd_curve_model",
  inherit = parametric_curve,
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

      #Model Name
      cli::cli_alert_info("{self$model_name}")
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
    read_inp_lines = function(inp_con, nline) {

      cli::cli_text("{.emph {.field {self$model_name}}}")

      # Read an additional line from the file connection and split the string
      # into substrings by whitespace
      inp_line <- read_inp_numeric_line(inp_con)

      nline <- nline + 1
      cli_alert("Line {nline}: parameters ...")

      # Assign substrings
      self$alpha <- inp_line[1]
      self$beta <- inp_line[2]
      self$kpar <- inp_line[3]
      self$variance <- inp_line[4]

      cli_ul()
      cli_li("Alpha: {.val {private$.alpha}}")
      cli_li("Beta: {.val {private$.beta}}")
      cli_li("K: {.val {private$.kpar}}")
      cli_li("Variance: {.val {private$.variance}}")
      cli_end()



      return(nline)
    },

    #' @description
    #' Exports RECRUIT submodel data for shepherd curve recruitment
    #' to AGEPRO input file lines.
    inp_lines_recruit_data = function(delimiter = " ") {
      return(list(paste(self$alpha,
                        self$beta,
                        self$kpar,
                        self$variance,
                        sep = delimiter
      )))
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
    },

    #' @field json_recruit_data
    #' Returns JSON-ready Recruit Model Data
    json_recruit_data = function() {
      return(list(
        alpha = self$alpha,
        beta = self$beta,
        k = self$kpar,
        variance = self$variance
      ))
    }

  ),
  private = list(

    .alpha = 0.1,
    .beta = 0.1,
    .kpar = 0.1,
    .variance = 0.1

  )
)
