


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
#'
Recruitment <- R6Class(
  "Recruitment",

  public = list(

    #' @field rec_fac Recruitment Scaling Factor. Multiplier to convert
    #' recruitment submodel's recruitment units to absolute numbers of fish
    rec_fac = 0,

    #' @field ssb_fac Spawning Biomass (SSB) Scaling Factor. Multiplier to
    #' convert recruitment submodel's SSB to absolute spawning weight of fish
    #' in kilograms (kg)
    ssb_fac = 0,

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
    initialize = function (model_num, seq_years) {

      self$set_recruit_data(model_num, seq_years)
      self$recruit_scaling_factor <- 1000
      self$ssb_scaling_factor <- 0
      self$print()

    },


    #' @description
    #' Creates Recruitment Model Data
    set_recruit_data = function(model_num, seq_years){

      private$cli_recruit_rule()
      cli_alert("Recruitment Data Setup")

      # Handle seq_years as a single int or a vector of sequential values
      private$assert_seq_years(seq_years)

      #Setup vectors based on number of recruitment models.
      private$qty_rec_models <- length(model_num)
      self$rec_model_num <- vector("list", private$qty_rec_models) #Recruitment Model Number list
      self$rec_prob <- vector ("list", private$qty_rec_models) #Recruitment Probability list
      self$model_collection_list <- vector ("list", private$qty_rec_models) #Recruitment Model Data List


      #TODO: Assert num_rec_models & private$qty_seq_years vector are valid
      cli_alert_info("{private$qty_rec_models} recruitment model{?s} for {private$qty_seq_years} year{?s}.")

      #Set recruitment probability and model data for each recruitment model.
      for (recruit in 1:private$qty_rec_models) {

        # Recruitment Probability: Fill the timeseries with a recruitment probability sums equal to unity
        # TODO: Check validity
        # TODO: Refactor to function
        self$rec_prob[[recruit]] <-
          format(round(rep(1, private$qty_seq_years)/private$qty_seq_years,  4), nsmall=4)

        names(self$rec_prob[[recruit]]) <- private$req_prob_years
        self$rec_model_num[[recruit]] <- model_num[[recruit]]

        #Add Recruitment Data
        cli_par()
        cli_alert_info("Recruit {recruit} of {private$qty_rec_models} : Recruitment Model #{model_num[[recruit]]} ")
        self$model_collection_list[[recruit]] <- self$get_recruit_data(self$rec_model_num[[recruit]], self$seq_yrs)
        cli_end()

      }
      cli_alert_info("Recruitment Probability:")
      cat_print(self$rec_prob)


    },


    #' @description
    #' Gets Recruitment Data
    get_recruit_data = function(model_num, seq_years){

      if(model_num == 3){
        return(EmpiricalDistributionModel$new(seq_years))

      }else if(model_num == 5){
        return(BevertonHoltCurveModel$new())

      }else if(model_num == 6){
        return(RickerCurveModel$new())

      }else if(model_num ==7) {
        return(ShepherdCurveModel$new())

      }else if(model_num == 14) {
        return(EmpiricalCDFModel$new(seq_years))
      }
      else{
        return(NullRecruitModel$new())
      }

    },

    #' @description
    #' Prints out Recruitment
    #'
    print = function(...){

      #verify private fields are numeric
      assert_numeric(private$qty_rec_models)
      assert_numeric(private$qty_seq_years)
      assert_numeric(private$qty_rec_models)

      cli_alert_info("{private$qty_rec_models} recruitment model{?s} for {private$qty_seq_years} year{?s}.")
      cli_ul()
      cli_li("Recruitment Scaling Factor: {.val {self$rec_fac}}")
      cli_li("SSB Scaling Factor: {.val {self$ssb_fac}}")


      cli_alert_info("Recruitment Probability:")
      assert_list(self$rec_prob) #verify recruit_prob list
      cat_print(self$rec_prob)
      cli_par()
      for (recruit in 1:private$qty_rec_models){
        cli_par()
        cli_alert_info("Recruit {recruit} of {private$qty_rec_models} : Recruitment Model #{self$rec_model_num[[recruit]]} ")
        assert_r6(self$model_collection_list[[recruit]],"RecruitModel") #Verify class inherits from "RecruitModel"
        cat_print(self$model_collection_list[[recruit]])
        cli_end()
      }
      cli_par()

    },


    #'@description
    #' Prints out Recruitment object data to console, with an option to display
    #' recruit object data to JSON format.
    #'
    #' @param print_json Option to print recruitment object as written in JSON
    #' format into console
    print_recruit = function (print_json = TRUE) {

      #Gather Recruit Model Data
      model_data_list <- vector("list", length(self$rec_model_num))
      for(recruit in 1:length(self$rec_model_num)){
        model_data_list[[recruit]] <- self$model_collection_list[[recruit]][["recruit_data"]]
      }

      recruit_json <- list(
        recFac=self$rec_fac,
        ssbFac=self$ssb_fac,
        maxRecObs=self$max_rec_obs,
        type=self$rec_model_num,
        prob=self$rec_prob,
        modelData=model_data_list)

      if(print_json){
        toJSON(recruit_json,
               pretty =TRUE,
               auto_unbox =TRUE)
      }else{
        return(recruit_json)
      }

    },
    #TODO: Create a active field function for showing model_collection_list

    #' @description
    #' Helper Function To View Recruitment Model Collection Data
    view_recruit_data = function () {

      cli_alert_info("Recruitment Model{?s}: {.field {self$rec_model_num}} ")

    }

  ), active = list (
    #' @field recruit_scaling_factor Sets the Recruitment Scaling factor
    recruit_scaling_factor = function(x) {
      assert_numeric(x)
      self$rec_fac <- x
    },

    #' @field ssb_scaling_factor Sets the SSB Scaling Factor
    ssb_scaling_factor = function(x) {
      assert_numeric(x)
      self$ssb_fac <- x
    }




  ), private = list (
    qty_seq_years = NULL,
    qty_rec_models = NULL,
    req_prob_years = NULL,

    cli_recruit_rule = function() {
      d <- cli_div(theme= list(rule= list(
        color = "cyan",
        "line-type" = "double")))
      cli_rule("Recruitment")
      cli_end(d)
    },

    assert_seq_years = function(seq_years) {
      #Handle seq_years as a single int or a vector of sequential values
      self$seq_yrs <- seq_years

      if(test_int(self$seq_yrs)){
        #single
        private$qty_seq_years <- self$seq_yrs
        private$req_prob_years <- 1:self$seq_yrs
      }
      else{
        private$qty_seq_years <- length(self$seq_yrs)
        private$req_prob_years <- self$seq_yrs
      }


    }
  )

)







