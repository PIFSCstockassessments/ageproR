


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
#'
#' @export
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#' @importFrom checkmate test_int assert_numeric assert_list assert_r6
#' @importFrom checkmate assert_int
#' @importFrom collections dict
#'
recruitment <- R6Class(
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

    assert_observed_years = function(obs_years) {

      if(test_int(obs_years)) {
        #single
        private$.number_projection_years <- obs_years
        private$.sequence_projection_years <- 1:obs_years

      } else {
        private$.number_projection_years <- length(obs_years)
        private$.sequence_projection_years <- obs_years
      }


    },

    #TODO: shared function?
    assert_numeric_substrings = function (inp_line){

      if(!all(grepl("^[[:digit:]]",inp_line))) {

        non_numerics <- inp_line[!grepl("^[[:digit:]]",inp_line)]
        stop("Line contains a Non Numeric Substring",
             paste(non_numerics, collapse = ", "))
      }

      invisible(as.numeric(inp_line))

    }

  ), public = list(

    #' @field recruit_model_num_list Recruitment Type
    recruit_model_num_list = NULL,

    #' #' @field recruit_probability The Recruitment Probabilities.
    #' recruit_probability = NULL,

    #' @field model_collection_list List of recruitment models
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
      self$observation_years <- seq_years

      self$set_recruit_data(model_num, self$observation_years)
      self$recruit_scaling_factor <- 1000
      self$ssb_scaling_factor <- 0


      if(!missing(max_rec_obs)) {
        private$.max_rec_obs <- max_rec_obs
      }


      self$print(cat_verbose)


    },


    #' @description
    #' Creates Recruitment Model Data
    set_recruit_data = function(model_num, seq_years) {

      # Handle seq_years as a single int or a vector of sequential values
      private$assert_observed_years(seq_years)

      #Setup vectors based on number of recruitment models.
      private$.number_recruit_models <- length(model_num)

      #Recruitment Model Number list
      self$recruit_model_num_list <-
        vector("list", private$.number_recruit_models)

      #Recruitment Probability list
      private$.recruit_probability <-
        vector("list", private$.number_recruit_models)

      #Recruitment Model Data List
      self$model_collection_list <-
        vector("list", private$.number_recruit_models)

      #Set recruitment probability and model data for each recruitment model.
      for (recruit in 1:private$.number_recruit_models) {

        # Recruitment Probability: Fill the time series with a recruitment
        # probability sums equal to unity
        # TODO: Check validity
        # TODO: Refactor to function
        private$.recruit_probability[[recruit]] <-
          as.numeric(format(round(rep(1, private$.number_projection_years) /
                    private$.number_projection_years, digits = 4), nsmall = 4))

        names(private$.recruit_probability[[recruit]]) <-
          private$.sequence_projection_years

        #Model Num
        self$recruit_model_num_list[[recruit]] <- model_num[[recruit]]

        #Add Recruitment Data
        self$model_collection_list[[recruit]] <-
          self$set_recruit_model(self$recruit_model_num_list[[recruit]],
                                 self$observation_years)
      }


    },


    #' @description
    #' Initializes RecruitModel Data
    #' @export
    set_recruit_model = function(model_num, seq_years) {

      assert_numeric(model_num, lower = 0, upper = 21)

      model_dict <- dict(list(
        "0" = null_recruit_model$new(),
        "3" = empirical_distribution_model$new(seq_years),
        "5" = beverton_holt_curve_model$new(),
        "6" = ricker_curve_model$new(),
        "7" = shepherd_curve_model$new(),
        "9" = deprecated_recruit_model_9$new(),
        "14" = empirical_cdf_model$new(seq_years)
      ))

    model_dict$get(as.character(model_num))

    },

    #' @description
    #' Sets the recruitment probability
    #'
    #' @param j Index of the recruitment collection list
    #' @param year index of the Time series
    #' @param value Recruitment Probability
    set_recruit_probability = function(j, year, value) {

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

      private$cli_recruit_probability()

    },

    #' @description
    #' Prints out Recruitment
    #'
    #' @param cat_verbose Flag to allow `cat` based cli messages printed on
    #' console. Default is TRUE
    print = function(cat_verbose = TRUE , ...) {

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
             capture.output( x <- private$cli_recruit_probability())
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





    #' @description
    #' Prints out Recruitment object data to console, with an option to display
    #' recruit object data to JSON format.
    #'
    #' @param print_json Option to print recruitment object as written in JSON
    #' format into console
    print_recruit = function(print_json = TRUE) {

      #Gather Recruit Model Data
      model_data_list <- vector("list", length(self$recruit_model_num_list))

      for (recruit in seq_along(self$recruit_model_num_list)){

        model_data_list[[recruit]] <-
          self$model_collection_list[[recruit]][["recruit_data"]]
      }

      recruit_json <- list(
        recFac = self$recruit_scaling_factor,
        ssbFac = self$ssb_scaling_factor,
        maxRecObs = private$.max_rec_obs,
        type = self$recruit_model_num_list,
        prob = self$recruit_probability,
        modelData = model_data_list)

      if (print_json) {
        toJSON(recruit_json,
               pretty = TRUE,
               auto_unbox = TRUE)
      }else {
        return(recruit_json)
      }

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
    #' @param nlines Reference to current line read
    read_inp_lines = function(inp_con, nlines) {

      #Check
      assert_numeric(self$observation_years, sorted = TRUE)

      # Read an additional line from the file connection and split the string
      # into substrings by whitespace
      inp_line <-
        unlist(strsplit(readLines(inp_con, n = 1, warn = FALSE), " +"))

      nline <- nline + 1
      cli_alert("Line {nline} ...")

      inp_line <- private$assert_numeric_substrings(inp_line)

      # Assign substrings
      self$recruit_scaling_factor <- self$inp_line[1]
      self$ssb_scaling_factor <- self$inp_line[2]
      self$max_recruit_obs <- self$inp_line[3]
      #TODO: rename to max_recruit_observations

      # Read an additional line from the file connection, and parse the
      # substring(s) for Recruitment Model(s) for model_collection_list
      inp_line <-
        unlist(strsplit(readLines(inp_con, n = 1, warn = FALSE), " +"))

      nline <- nline + 1
      cli_alert("Line {nline} ...")
      cli_alert_info("{inp_line}")

      inp_line <- private$assert_numeric_substrings(inp_line)

      self$recruit_model_num_list <- inp_line

      #For each year in AGEPRO Model's observation years,
      #read an additional line from the file connection, and append line to
      #the recruitment probably (list)



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

    }

  )

)
