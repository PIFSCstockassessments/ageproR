
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
#' @template elipses
#' @template inp_con
#' @template nline
#'
#' @export
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#' @importFrom checkmate test_int assert_numeric assert_list assert_r6
#' @importFrom checkmate assert_int
#' @importFrom collections dict
#' @importFrom rlang expr eval_tidy
#'
recruitment <- R6Class( # nolint: cyclocomp_linter
  "recruitment",
  private = list(
    .number_projection_years = NULL,
    .number_recruit_models = NULL,
    .sequence_projection_years = NULL,
    .max_rec_obs = 10000,

    .recruit_scaling_factor = NULL,
    .ssb_scaling_factor = NULL,

    .recruit_probability = NULL,
    .recruit_model_num_list = NULL,



    cli_recruit_rule = function() {
      d <- cli_div(theme = list(rule = list(
        color = "cyan",
        "line-type" = "double")))
      cli_rule("Recruitment")
      cli_end(d)
    },

    cli_recruit_probability = function() {
      #Module to printout Recruitment probability to Rconsole
      cli_alert_info("Recruitment Probability:")
      assert_list(private$.recruit_probability) #verify recruit_prob list
      cat_print(private$.recruit_probability)
    },

    #Handle observed_years as single int or a vector of sequential values
    assert_observed_years = function(obs_years) {

      if (test_int(obs_years)) {
        #single
        private$.number_projection_years <- obs_years
        private$.sequence_projection_years <- 1:obs_years

      } else {
        private$.number_projection_years <- length(obs_years)
        private$.sequence_projection_years <- obs_years
      }


    },

    setup_recruitment_list_vectors = function(model_num) {

      # Setup number of recruits based on the vector length of the recruitment
      # models field sent to the function.
      private$.number_recruit_models <- length(model_num)

      #Setup Recruitment Model Number list
      self$recruit_model_num_list <-
        vector("list", private$.number_recruit_models)

      #Setup Recruitment Model Data List
      self$model_collection_list <-
        vector("list", private$.number_recruit_models)

    },

    #Setup recruitment probability for each recruitment model.
    setup_recruitment_probability = function() {

      assert_numeric(private$.number_recruit_models, lower = 1)
      assert_numeric(private$.number_projection_years, lower = 1)
      assert_numeric(private$.sequence_projection_years)

      #Setup Recruitment Probability list
      private$.recruit_probability <-
        vector("list", private$.number_recruit_models)

      for (recruit in 1:private$.number_recruit_models) {

        # Recruitment Probability: Fill the time series with a recruitment
        # probability sums equal to unity
        private$.recruit_probability[[recruit]] <-
          as.numeric(format(
            round(rep(1, private$.number_projection_years) /
                    private$.number_projection_years, digits = 4), nsmall = 4))

        names(private$.recruit_probability[[recruit]]) <-
          private$.sequence_projection_years

      }
    }



  ), public = list(

    #' @field recruit_model_num_list Recruitment Type
    recruit_model_num_list = NULL,

    #' @field model_collection_list List of recruitment models. Use this field
    #' to access a specific recruitment models field.
    model_collection_list = NULL,

    #' @field observation_years Sequence of projected years
    observation_years = NULL,

    #' @description
    #' Initializes the Recruitment Class
    #'
    #' @param max_rec_obs
    #' Max limit of recruitment observations. Default is 10000.
    #'
    #' @param cat_verbose
    #' Flag to print out `cat` based cli messages printed on console. Default
    #' is TRUE.
    #'
    initialize = function(model_num, seq_years, max_rec_obs = 10000,
                          cat_verbose = TRUE) {

      #Handle seq_years as a single int or a vector of sequential values
      #This is used to set parameters for some recruitment models
      self$observation_years <- seq_years

      # Handle seq_years as a single int or a vector of sequential values
      private$assert_observed_years(seq_years)

      # Setup .number_recruit_models vectors
      private$setup_recruitment_list_vectors(model_num)

      # Setup Recruitment probability
      private$setup_recruitment_probability()

      # Set Recruitment Model data
      self$set_recruit_data(model_num)
      self$recruit_scaling_factor <- 1000
      self$ssb_scaling_factor <- 0


      if (!missing(max_rec_obs)) {
        private$.max_rec_obs <- max_rec_obs
      }


      self$print(cat_verbose)


    },


    #' @description
    #' Creates Recruitment Model Data
    set_recruit_data = function(model_num) {

      # Setup .number_recruit_models vectors
      private$setup_recruitment_list_vectors(model_num)

      #Set recruitment probability and model data for each recruitment model.
      for (recruit in 1:private$.number_recruit_models) {

        # #Model Num
        self$recruit_model_num_list[[recruit]] <- model_num[[recruit]]

        #Add Recruitment Data with recruitment model number
        self$model_collection_list[[recruit]] <-
          self$set_recruit_model(self$recruit_model_num_list[[recruit]])

      }


    },


    #' @description
    #' Initializes Recruit Model Data.
    #'
    #' Recruitment class field `observation_years` is used for recruitment
    #' models that use the model projection year time horizon for setup.
    #'
    #' @export
    set_recruit_model = function(model_num) {

      assert_numeric(model_num, lower = 0, upper = 21)


      model_dict <- dict(list(
        "0" = expr(null_recruit_model$new()),
        "3" = expr(empirical_distribution_model$new(self$observation_years)),
        "4" = expr(two_stage_empirical_ssb$new()),
        "5" = expr(beverton_holt_curve_model$new()),
        "6" = expr(ricker_curve_model$new()),
        "7" = expr(shepherd_curve_model$new()),
        "9" = expr(deprecated_recruit_model_9$new()),
        "14" = expr(empirical_cdf_model$new()),
        "15" = expr(two_stage_empirical_cdf$new())
      ))

    eval_tidy(model_dict$get(as.character(model_num)))

    },



    #' @description
    #' Sets the recruitment probability
    #'
    #' @param j Index of the recruitment collection list
    #' @param year index of the Time series
    #' @param value Recruitment Probability
    #' @param verbose Flag to allow based cli messages printed on
    #' console. Default is TRUE
    set_recruit_probability = function(j, year, value, verbose = TRUE) {

      assert_int(j, lower = 1, upper = self$num_recruit_models)
      assert_numeric(year,
                     max.len = length(self$recruit_probability[[j]]))
      assert_numeric(value, lower = 0, upper = 1,
                     max.len = length(year))

      if (!all(year %in% private$.sequence_projection_years)) {
        stop(paste0(
          "Year ",
          subset(year, !(year %in% private$.sequence_projection_years)),
          " is not within model projected year time horizon.\n  "))
      }

      private$.recruit_probability[[j]][as.character(year)] <- value

      if (verbose) private$cli_recruit_probability()

    },

    #' @description
    #' Prints out Recruitment
    #'
    #' @param cat_verbose Flag to allow `cat` based cli messages printed on
    #' console. Default is TRUE
    print = function(cat_verbose = TRUE, ...) {

      #verify private fields are numeric
      assert_numeric(private$.number_recruit_models)
      assert_numeric(private$.number_projection_years)

      cli_alert_info(c("{private$.number_recruit_models} recruitment model{?s}",
                     " for {private$.number_projection_years} year{?s}."))
      cli_ul()
      cli_li("Recruitment Scaling Factor: {.val {self$recruit_scaling_factor}}")
      cli_li("SSB Scaling Factor: {.val {self$ssb_scaling_factor}}")
      cli_par()
      cli_end()

      #Module to printout Recruitment Probability
      #Verbose flag check
      ifelse(cat_verbose,
             #Allow Recruitment Probability 'cat' cli message
             private$cli_recruit_probability(),
             #Suppress Recruitment Probability 'cat' cli message
             capture.output(x <- private$cli_recruit_probability())
             )

      for (recruit in 1:private$.number_recruit_models){
        cli_par()
        cli_alert_info(paste0("Recruit {recruit} of ",
                              "{private$.number_recruit_models} : ",
                              "Recruitment Model #",
                              "{self$recruit_model_num_list[[recruit]]} "))

        #Verify class inherits from "recruit_model"
        assert_r6(self$model_collection_list[[recruit]], "recruit_model")

        self$model_collection_list[[recruit]]$print()
        cli_end()
      }
      cli_par()
      cli_end()
    },

    #TODO: Create a active field function for showing model_collection_list

    #' @description
    #' Helper Function To View Recruitment Model Collection Data
    view_recruit_data = function() {

      cli_alert_info(paste0("Recruitment Model{?s}: ",
                            "{.field {self$recuit_model_num_list}} "))

    },

    #' @description
    #' Reads in Recruitment AGEPRO parameters from AGEPRO INP Input File
    read_inp_lines = function(inp_con, nline) {

      #Check
      assert_numeric(self$observation_years, sorted = TRUE)
      #Setup .number_projection_years and .sequence_projection_years
      private$assert_observed_years(self$observation_years)

      # Read an additional line from the file connection and split the string
      # into substrings by whitespace
      inp_line <- read_inp_numeric_line(inp_con)

      nline <- nline + 1
      cli_alert(
        "Line {nline} : Recruit/SSB Scaling Factors & max recruit obs ...")
      cli_text("{.val {inp_line}}")

      # Assign substrings
      self$recruit_scaling_factor <- inp_line[1]
      self$ssb_scaling_factor <- inp_line[2]
      self$max_recruit_obs <- inp_line[3]
      #TODO: rename to max_recruit_observations

      # Read an additional line from the file connection, and parse the
      # substring(s) for Recruitment Model(s) for model_collection_list
      inp_line <- read_inp_numeric_line(inp_con)

      nline <- nline + 1
      cli_alert(c("Line {nline}: Recruitment model number: ",
                  "{.val {inp_line}}"))

      #?Validate length Recruitment's recruit_model_num_list matches
      #length of recruitment models field from the set_recruit_data function

      ## Setup for .number_recruit_models and recruitment list vectors:
      # recruit_model_mum_list, .recruit_probability, & model_collection_list
      private$setup_recruitment_list_vectors(inp_line)

      # Setup Default Recruitment probability
      private$setup_recruitment_probability()

      #TODO: Refactor loop
      # Assign "recruit type" inp_line values to recruit_model_num_list.
      for (recruit in 1:private$.number_recruit_models) {
        #Model Num
        self$recruit_model_num_list[[recruit]] <- inp_line[recruit]

      }

      cli_alert_info("{.emph Reading Recruitment Probabaility}")
      # Set Input File Recruitment Probability values over default values.
      # For each year in AGEPRO Model's observation years ...
      for (year in self$observation_years){

        # Read an additional line from the file connection ...
        inp_line <- read_inp_numeric_line(inp_con)

        nline <- nline + 1
        cli_alert(c("Line {nline}: Recruitment probabaility for year {year} : ",
                    "{.val {inp_line}}"))

        # Verify recruit probability value ...
        assert_numeric(inp_line, lower = 0, upper = 1)

        #TODO: Refactor loop
        # And then append line to the recruitment probability (list) ...
        for (j in seq_along(inp_line)) {
          self$set_recruit_probability(j, year, inp_line[[j]], verbose = FALSE)
        }

      }

      #Print out Recruitment Probability from Input data to console
      private$cli_recruit_probability()

      # For each recruit model in recruit_model_collection
      for (recruit in private$.number_recruit_models){

        #Setup Recruitment Model w/ default values
        self$model_collection_list[[recruit]] <-
          self$set_recruit_model(self$recruit_model_num_list[[recruit]])

        cli_alert(c("Reading recruitment model ",
                    "{.field #{self$recruit_model_num_list[[recruit]]}}"))
        #Read in inp lines to set recruitment model data values
        nline <-
          self$model_collection_list[[recruit]]$read_inp_lines(inp_con, nline)

      }




      return(nline)

    }

  ), active <- list(

    #' @field max_recruit_obs
    #' Recruitment submodel's maximum number of observations
    max_recruit_obs = function(value) {
      if (missing(value)) {
        return(private$.max_rec_obs)
      }else {
        assert_int(value)
        private$.max_rec_obs <- value
      }
    },

    #' @field recruit_probability
    #' The Recruitment Probabilities.
    recruit_probability = function() {
      return(private$.recruit_probability)
    },

    #' @field num_recruit_models
    #' Returns number of recruitment models
    num_recruit_models = function() {
      return(private$.number_recruit_models)
    },

    #' @field recruit_scaling_factor
    #' The multiplier to convert recruitment submodel's recruitment units to
    #' absolute numbers of fish
    recruit_scaling_factor = function(value) {
      if (missing(value)) {
        return(private$.recruit_scaling_factor)
      }else {
        assert_numeric(value)
        private$.recruit_scaling_factor <- value
      }

    },

    #' @field ssb_scaling_factor
    #' The multiplier to convert recruitment submodel's SSB to absolute
    #' spawning weight of fish in kilograms (kg)
    ssb_scaling_factor = function(value) {
      if (missing(value)) {
        return(private$.ssb_scaling_factor)
      }else {
        assert_numeric(value)
        private$.ssb_scaling_factor <- value
      }

    },

    #' @field json_list_recruit
    #' List of RECRUIT keyword fields values, exportable to JSON.
    #'
    json_list_recruit = function() {

      #Gather Recruit Model Data
      recruit_model_data_list <-
        vector("list", length(self$recruit_model_num_list))

      for (recruit in seq_along(self$recruit_model_num_list)){
        recruit_model_data_list[[recruit]] <-
          self$model_collection_list[[recruit]][["recruit_data"]]
      }

      return(list(
        recFac = self$recruit_scaling_factor,
        ssbFac = self$ssb_scaling_factor,
        maxRecObs = private$.max_rec_obs,
        type = self$recruit_model_num_list,
        prob = self$recruit_probability,
        recruitData = recruit_model_data_list))

    }


  )

)
