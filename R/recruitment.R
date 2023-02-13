


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
#' @importFrom collections dict
#'
recruitment <- R6Class(
  "recruitment",

  public = list(

    #' @field max_rec_obs Recruitment submodel's maximum number of observations
    max_rec_obs = 1000,

    #' @field rec_model_num Recruitment Type
    rec_model_num = NULL,

    #' @field rec_prob recruitment probabilities
    rec_prob = NULL,

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
      private$qty_rec_models <- length(model_num)
      #Recruitment Model Number list
      self$rec_model_num <- vector("list", private$qty_rec_models)
      #Recruitment Probability list
      self$rec_prob <- vector("list", private$qty_rec_models)
      #Recruitment Model Data List
      self$model_collection_list <- vector("list", private$qty_rec_models)

      #Set recruitment probability and model data for each recruitment model.
      for (recruit in 1:private$qty_rec_models) {

        # Recruitment Probability: Fill the time series with a recruitment
        # probability sums equal to unity
        # TODO: Check validity
        # TODO: Refactor to function
        self$rec_prob[[recruit]] <-
          format(
            round(rep(1, private$qty_seq_years) / private$qty_seq_years, 4),
            nsmall = 4)

        names(self$rec_prob[[recruit]]) <- private$req_prob_years
        self$rec_model_num[[recruit]] <- model_num[[recruit]]

        #Add Recruitment Data
        self$model_collection_list[[recruit]] <-
          self$set_recruit_model(self$rec_model_num[[recruit]], self$seq_yrs)
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
      assert_numeric(private$qty_rec_models)
      assert_numeric(private$qty_seq_years)
      assert_numeric(private$qty_rec_models)

      cli_alert_info(c("{private$qty_rec_models} recruitment model{?s}",
                     " for {private$qty_seq_years} year{?s}."))
      cli_ul()
      cli_li("Recruitment Scaling Factor: {.val {self$recruit_scaling_factor}}")
      cli_li("SSB Scaling Factor: {.val {self$ssb_scaling_factor}}")
      cli_par()
      cli_end()
      cli_alert_info("Recruitment Probability:")
      assert_list(self$rec_prob) #verify recruit_prob list
      cat_print(self$rec_prob)
      for (recruit in 1:private$qty_rec_models){
        cli_par()
        cli_alert_info(c("Recruit {recruit} of {private$qty_rec_models} : ",
                         "Recruitment Model #{self$rec_model_num[[recruit]]} "))
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
      model_data_list <- vector("list", length(self$rec_model_num))
      for (recruit in seq_along(self$rec_model_num)){

        model_data_list[[recruit]] <-
          self$model_collection_list[[recruit]][["recruit_data"]]
      }

      recruit_json <- list(
        recFac = self$rec_fac,
        ssbFac = self$ssb_fac,
        maxRecObs = self$max_rec_obs,
        type = self$rec_model_num,
        prob = self$rec_prob,
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

      cli_alert_info("Recruitment Model{?s}: {.field {self$rec_model_num}} ")

    }

  ), active <- list(

    #' @field recruit_scaling_factor Sets the Recruitment Scaling factor.
    #' Multiplier to convert recruitment submodel's recruitment units to
    #' absolute numbers of fish
    recruit_scaling_factor = function(value) {
      if (missing(value)) {
        return(private$.recruit_scaling_factor)
      }else {
        assert_numeric(value)
        private$.recruit_scaling_factor <- value
      }

    },

    #' @field ssb_scaling_factor Sets the SSB Scaling Factor. Multiplier to
    #' convert recruitment submodel's SSB to absolute spawning weight of fish
    #' in kilograms (kg)
    ssb_scaling_factor = function(value) {
      if (missing(value)) {
        return(private$.ssb_scaling_factor)
      }else {
        assert_numeric(value)
        private$.ssb_scaling_factor <- value
      }

    }




  ), private = list(
    qty_seq_years = NULL,
    qty_rec_models = NULL,
    req_prob_years = NULL,

    .recruit_scaling_factor = NULL,
    .ssb_scaling_factor = NULL,


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
        private$qty_seq_years <- self$seq_yrs
        private$req_prob_years <- 1:self$seq_yrs

      } else {
        private$qty_seq_years <- length(self$seq_yrs)
        private$req_prob_years <- self$seq_yrs
      }


    }
  )

)
