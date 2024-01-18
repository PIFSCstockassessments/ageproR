
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
#' @template proj_years_vector
#' @template elipses
#' @template inp_con
#' @template nline
#' @template delimiter
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
    .max_recruit_obs = NULL,

    .keyword_name = "recruit",

    .recruit_scaling_factor = NULL,
    .ssb_scaling_factor = NULL,
    .model_collection_list = NULL,
    .observation_years = NULL,

    .recruit_probability = NULL,
    .recruit_model_num_list = NULL,


    #private setters for recruitment fields
    set_recruit_scaling_factor = function(value) {
      checkmate::assert_numeric(value, len = 1,
                                .var.name = "recruit_scaling_factor")
      private$.recruit_scaling_factor <- value
    },

    set_ssb_scaling_factor = function(value) {
      checkmate::assert_numeric(value, len = 1,
                                .var.name = "ssb_scaling_factor")
      private$.ssb_scaling_factor <- value
    },

    set_max_recruit_obs = function(value) {
      checkmate::assert_int(value,
                            .var.name = "max_recruit_obs")
      private$.max_recruit_obs <- value
    },

    set_observation_years = function(value) {
      checkmate::assert_numeric(value,
                                .var.name = "observation_years")
      private$.observation_years <- value
    },


    #Module to printout Recruitment probability to Rconsole
    cli_recruit_probability = function() {
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



    # Creates Recruitment Model Data.
    # Helper function to help setup .number_recruit_models,
    # recruit_model_num_list, & model_collection_list vectors.
    setup_recruit_data = function(model_num) {

      #Setup Recruitment Model Number list
      self$recruit_model_num_list <-
        vector("list", private$.number_recruit_models)

      #Setup Recruitment Model Data List
      self$model_collection_list <-
        vector("list", private$.number_recruit_models)


      #Set Model data for each recruitment model.
      for (recruit in 1:private$.number_recruit_models) {

        # #Model Num
        self$recruit_model_num_list[[recruit]] <- model_num[[recruit]]

        #Add Recruitment Data with recruitment model number
        self$model_collection_list[[recruit]] <-
          self$set_recruit_model(self$recruit_model_num_list[[recruit]])

      }


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

    #' @description
    #' Initializes the Recruitment Class
    #'
    #' @param max_recruit_obs
    #' Max limit of recruitment observations. Default is 10000.
    #'
    #' @param cat_verbose
    #' Flag to print out `cat` based cli messages printed on console. Default
    #' is TRUE.
    #'
    #' @param num_recruit_models
    #' Number of Recruitment Models in AGEPRO model. Default is 1.
    #'
    initialize = function(model_num, seq_years, num_recruit_models = 1,
                          max_recruit_obs = 10000,
                          cat_verbose = TRUE) {

      #Handle seq_years as a single int or a vector of sequential values
      #This is used to set parameters for some recruitment models
      private$set_observation_years(seq_years)

      # Handle seq_years as a single int or a vector of sequential values
      private$assert_observed_years(seq_years)

      ## Validation
      # Check if input model number matches the number of observed years
      if(isFALSE(length(model_num) == num_recruit_models)){
        stop(paste0("Length of Recruitment number vector does not match ",
                    "AGEPRO model's number of recruits: ",
                    length(model_num), " (Number of Recruits: ",
                    num_recruit_models, ")"))
      }

      # Setup number of recruits based on the vector length of the recruitment
      # models field sent to the function.
      private$.number_recruit_models <- length(model_num)

      # Setup Recruitment Model data
      private$set_recruit_scaling_factor(1000)
      private$set_ssb_scaling_factor(0)
      private$set_max_recruit_obs(max_recruit_obs)
      private$setup_recruitment_probability()
      private$setup_recruit_data(model_num)


      # 'recruit' cli messages at initialization
      div_keyword_header(self$keyword_name)
      cli_alert("Creating Default Recruitment Model")
      self$print(cat_verbose)


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
        "3" = expr(empirical_distribution_model$new(private$.number_projection_years)),
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

      cli_par()
      cli::cli_alert_info("Recruit Data in recruitment's model collection list:")
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
    #' Reads in Recruitment AGEPRO parameters from AGEPRO INP Input File
    #'
    #' @param num_recruit_models
    #' Number of Recruitment Models. Default is 1
    read_inp_lines = function(inp_con, nline, num_recruit_models = 1) {

      #Check
      assert_numeric(self$observation_years, sorted = TRUE)
      #Setup .number_projection_years and .sequence_projection_years
      private$assert_observed_years(self$observation_years)

      # Read an additional line from the file connection and split the string
      # into substrings by whitespace
      inp_line <- read_inp_numeric_line(inp_con)

      nline <- nline + 1
      cli_alert(
        "Line {nline} : Scaling Factors & Max Recruit Observations ...")

      # Assign substrings
      private$set_recruit_scaling_factor(inp_line[1])
      private$set_ssb_scaling_factor(inp_line[2])
      private$set_max_recruit_obs(inp_line[3])

      # Console Output
      cli::cli_ul()
      cli::cli_li(paste0("Recruit Scaling Factor: ",
                    "{.val {self$recruit_scaling_factor}}"))
      cli::cli_li(paste0("SSB Scaling Factor: ",
                    "{.val {self$ssb_scaling_factor}}"))
      cli::cli_li(paste0("Max Recruit Observations: ",
                    "{.val {self$max_recruit_obs}}"))
      cli::cli_end()

      # Read an additional line from the file connection, and parse the
      # substring(s) for Recruitment Model(s) for model_collection_list
      inp_line <- read_inp_numeric_line(inp_con)

      nline <- nline + 1
      cli_alert(c("Line {nline}: Reading recruitment model number ",
                  "{.val {inp_line}} ..."))

      # Validate length Recruitment's recruit_model_num_list matches
      # Check if input model number matches the number of observed years
      if(isFALSE(length(inp_line) == num_recruit_models)){
        stop(paste0("Length of Recruitment number vector does not match ",
                    "AGEPRO model's number of recruits: ",
                    length(inp_line), " (Number of Recruits: ",
                    num_recruit_models, ")"))
      }

      # Setup number of recruits based on the vector length of the recruitment
      # models field sent to the function.
      private$.number_recruit_models <- length(inp_line)

      ## Setup for recruitment list vectors:
      private$setup_recruit_data(inp_line)
      private$setup_recruitment_probability()


      #TODO TODO :: FIX READING RECRUITMENT PROBABILY LINES BY OBSERVED YEARS

      # Assign "recruit type" inp_line values to recruit_model_num_list.
      for (recruit in 1:private$.number_recruit_models) {
        #Model Num
        self$recruit_model_num_list[[recruit]] <- inp_line[recruit]
      }

      cli_alert_info("Reading Recruitment Probabaility ... ")
      # Set Input File Recruitment Probability values over default values.
      # For each year in AGEPRO Model's observation years ...
      for (year in private$.sequence_projection_years){

        # Read an additional line from the file connection ...
        inp_line <- read_inp_numeric_line(inp_con)

        nline <- nline + 1
        cli_alert(c("Line {nline}: Recruitment probabaility for year {year}: ",
                    "{.val {inp_line}}"))

        # Verify recruit probability value ...
        assert_numeric(inp_line, lower = 0, upper = 1)

        # And then append line to the recruitment probability (list) ...
        for (j in seq_along(inp_line)) {
          self$set_recruit_probability(j, year, inp_line[[j]], verbose = FALSE)
        }

      }

      # For each recruit model in recruit_model_collection
      for (recruit in 1:private$.number_recruit_models){

        #Setup Recruitment Model w/ default values
        self$model_collection_list[[recruit]] <-
          self$set_recruit_model(self$recruit_model_num_list[[recruit]])

        cli::cli_alert_info(
          paste0("{.strong model_collection_list} ({recruit} of ",
                 "{length(self$model_collection_list)} ",
                 "recruit model{?s})"))

        #Nest Recruitment model read_inp_lines Output per model
        li_nested <- cli::cli_div(class = "input_field",
                                  theme = list(.input_field =
                                                 list("margin-left" = 2)))
        cli::cli_alert_info(
          paste0("Reading recruitment model ",
                 "{.field {self$recruit_model_num_list[[recruit]]}} ..."))
        #Read in inp lines to set recruitment model data values
        nline <-
          self$model_collection_list[[recruit]]$read_inp_lines(inp_con, nline)

        cli::cli_end(li_nested)

      }

      return(nline)

    },

    #' @description
    #' Returns the values for the RECRUIT keyword parameter formatted
    #' to the AGEPRO input file format.
    inplines_recruit = function(delimiter = " ") {

      # Set recruitment probability as a matrix and then separate matrix by
      # (year) rows as assign rows as separate list objects representing
      # AGEPRO input data file line. "unname" to remove data.frame labeling.
      # Collapse multi-recruit prob values as a single inpline list string.
      list_recruit_probability <-
        lapply(
          unname(as.list(
            data.frame(t(sapply(self$recruit_probability, matrix)))
          )),
          paste,
          collapse = delimiter)

      # Unlist each recruitment model's "inplines_recruit_data", via rapply:
      # returning a flat string vector of input data lines. Relist vector,
      # via "as.list", to convert to list of input data line elements.

      list_recruit_data <-
        as.list(unlist(rapply(
          self$model_collection_list,
          f = function(X) {X$inplines_recruit_data(delimiter)},
          how = "list")))

      return(c(list(
        self$inp_keyword,
        paste(
          self$recruit_scaling_factor,
          self$ssb_scaling_factor,
          self$max_recruit_obs,
          sep = delimiter
        ),
        # Append recruit list of recruit models as a single line
        paste(self$recruit_model_num_list, collapse = delimiter)),
        # Append Recruit probability
        list_recruit_probability,
        # Append recruit data
        list_recruit_data
      ))
    }

  ), active <- list(

    #' @field recruit_scaling_factor
    #' The multiplier to convert recruitment submodel's recruitment units to
    #' absolute numbers of fish
    recruit_scaling_factor = function(value) {
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      private$.recruit_scaling_factor
    },

    #' @field ssb_scaling_factor
    #' The multiplier to convert recruitment submodel's SSB to absolute
    #' spawning weight of fish in kilograms (kg)
    ssb_scaling_factor = function(value) {
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      private$.ssb_scaling_factor
    },


    #' @field max_recruit_obs
    #' Recruitment submodel's maximum number of observations
    max_recruit_obs = function(value) {
      if(isFALSE(missing(value))){
          stop("active binding is read only", call. = FALSE)
      }
      private$.max_recruit_obs
    },

    #' @field observation_years Sequence of projected years
    observation_years = function(value){
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }
      private$.observation_years
    },

    #' @field recruit_probability
    #' The Recruitment Probabilities.
    recruit_probability = function() {
      return(private$.recruit_probability)
    },


    #TODO: Create a active field function for showing model_collection_list

    #' @field model_collection_list
    #' List of recruitment models. Use this field
    #' to access a specific recruitment models field.
    model_collection_list = function(value) {
      if(missing(value)){
        return(private$.model_collection_list)
      } else{
        checkmate::assert_list(value, .var.name = "model_collection_list")
        private$.model_collection_list <- value
      }
    },

    #' @field recruit_model_num_list
    #' Helper Function To View Recruitment Model Collection Data
    recruit_model_num_list = function(value) {
      if(missing(value)){
        return(private$.recruit_model_num_list)
      } else{
        checkmate::assert_list(value, types = c("numeric","null"),
                               .var.name = "recruit_model_num_list")
        private$.recruit_model_num_list <- value
      }
    },

    #' @field num_recruit_models
    #' Returns number of recruitment models
    num_recruit_models = function() {
      return(private$.number_recruit_models)
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
        maxRecObs = private$.max_recruit_obs,
        type = unlist(self$recruit_model_num_list),
        prob = self$recruit_probability,
        recruitData = recruit_model_data_list))

    },

    #' @field keyword_name
    #' AGEPRO keyword parameter name
    keyword_name = function() {
      private$.keyword_name
    },

    #' @field inp_keyword
    #' Returns AGEPRO input-file formatted Parameter
    inp_keyword = function() {
      paste0("[",toupper(private$.keyword_name),"]")
    }

  )

)
