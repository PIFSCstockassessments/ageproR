


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
    .qty_seq_years = NULL,
    .qty_rec_models = NULL,
    .req_prob_years = NULL,
    .max_rec_prob = 1000,

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

    assert_seq_years = function(seq_years) {
      #Handle seq_years as a single int or a vector of sequential values
      self$seq_yrs <- seq_years

      if (test_int(self$seq_yrs)) {
        #single
        private$.qty_seq_years <- self$seq_yrs
        private$.req_prob_years <- 1:self$seq_yrs

      } else {
        private$.qty_seq_years <- length(self$seq_yrs)
        private$.req_prob_years <- self$seq_yrs
      }


    }
  ), public = list(

    #' @field recruit_probability The Recruitment Probabilities.
    recruit_probability = NULL,

    #' @field recruit_model_num_list Recruitment Type
    recruit_model_num_list = NULL,

    #' @field model_collection_list List of recruitment models
    model_collection_list = NULL,

    #' @field seq_yrs Sequence of projected years
    seq_yrs = NULL,

    #' @description
    #' Initializes the Recruitment Class
    #'
    initialize = function(model_num, seq_years) {

      self$set_recruit_data(model_num, seq_years)
      self$recruit_scaling_factor <- 1000
      self$ssb_scaling_factor <- 0
      self$print()

    },


    #' @description
    #' Creates Recruitment Model Data
    set_recruit_data = function(model_num, seq_years) {

      # Handle seq_years as a single int or a vector of sequential values
      private$assert_seq_years(seq_years)

      #Setup vectors based on number of recruitment models.
      private$.qty_rec_models <- length(model_num)
      #Recruitment Model Number list
      self$recruit_model_num_list <- vector("list", private$.qty_rec_models)
      #Recruitment Probability list
      self$recruit_probability <- vector("list", private$.qty_rec_models)
      #Recruitment Model Data List
      self$model_collection_list <- vector("list", private$.qty_rec_models)

      #Set recruitment probability and model data for each recruitment model.
      for (recruit in 1:private$.qty_rec_models) {

        # Recruitment Probability: Fill the time series with a recruitment
        # probability sums equal to unity
        # TODO: Check validity
        # TODO: Refactor to function
        self$recruit_probability[[recruit]] <-
          format(
            round(rep(1, private$.qty_seq_years) / private$.qty_seq_years, 4),
            nsmall = 4)

        names(self$recruit_probability[[recruit]]) <- private$.req_prob_years
        self$recruit_model_num_list[[recruit]] <- model_num[[recruit]]

        #Add Recruitment Data
        self$model_collection_list[[recruit]] <-
          self$set_recruit_model(self$recruit_model_num_list[[recruit]],
                                 self$seq_yrs)
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
    #' Prints out Recruitment
    #'
    print = function(...) {

      #verify private fields are numeric
      assert_numeric(private$.qty_rec_models)
      assert_numeric(private$.qty_seq_years)

      cli_alert_info(c("{private$.qty_rec_models} recruitment model{?s}",
                     " for {private$.qty_seq_years} year{?s}."))
      cli_ul()
      cli_li("Recruitment Scaling Factor: {.val {self$recruit_scaling_factor}}")
      cli_li("SSB Scaling Factor: {.val {self$ssb_scaling_factor}}")
      cli_par()
      cli_end()
      cli_alert_info("Recruitment Probability:")
      assert_list(self$recruit_probability) #verify recruit_prob list
      cat_print(self$recruit_probability)
      for (recruit in 1:private$.qty_rec_models){
        cli_par()
        cli_alert_info(paste0("Recruit {recruit} of ",
                              "{private$.qty_rec_models} : ",
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


    #'@description
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
        recFac = self$rec_fac,
        ssbFac = self$ssb_fac,
        maxRecObs = self$max_rec_obs,
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

    }

  ), active <- list(

    #' @field max_recruit_obs
    #' Recruitment submodel's maximum number of observations
    max_recruit_obs = function(value) {
      if(missing(value)) {
        return(private$.max_rec_obs)
      }else {
        assert_int(value)
        private$.max_rec_obs <- value
      }
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
