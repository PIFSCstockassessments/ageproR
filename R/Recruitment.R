


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
#'
#' @export
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#' @importFrom checkmate test_int
#'
Recruitment <- R6Class(
  "Recruitment",

  public = list(

    #' @field rec_fac multiplier to convert recruitment submodel's recruitment
    #' units to absolute numbers of fish
    rec_fac = 0,

    #' @field ssb_fac multiplier to convert recruitment submodel's Spawning
    #' Biomass (SSB) to absolute spawning weight of fish in kilograms
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

    },

    #' @description
    #' Creates Recruitment Model Data
    set_recruit_data = function(model_num, seq_years){

      private$cli_recruit_rule()
      cli_alert("Recruitment Data Setup")

      #Handle seq_years as a single int or a vector of sequential values
      self$seq_yrs <- seq_years
      if(test_int(self$seq_yrs)){
        #single
        num_rec_seq <- self$seq_yrs
        seq_yr_array <- 1:self$seq_yrs
      }
      else{
        num_rec_seq <- length(self$seq_yrs)
        seq_yr_array <- self$seq_yrs
      }

      #Number of Recruitment Models
      num_rec_models <- length(model_num)

      #Recruitment Model Number list
      self$rec_model_num <- vector("list", num_rec_models)

      #TODO: Assert num_rec_models & rec_model_num (model_num) vector are valid
      cli_alert_info("{num_rec_models} recruitment model{?s} for {num_rec_seq} year{?s}.")

      self$rec_prob <- vector ("list", num_rec_models)
      self$model_collection_list <- vector ("list", num_rec_models)

      seq_rec_models <- 1:num_rec_models
      for (recruit in seq_rec_models) {

        # Fill rec_prob
        # TODO: Check validity
        self$rec_prob[[recruit]] <- rep("1", num_rec_seq)
        names(self$rec_prob[[recruit]]) <- seq_yr_array
        self$rec_model_num[[recruit]] <- model_num[[recruit]]

        #Add Recruitment Data
        cli_par()
        cli_alert_info("Recruit {recruit} of {num_rec_models} : Recruitment Model #{model_num[[recruit]]} ")
        self$model_collection_list[[recruit]] <-
          self$get_recruit_data(self$rec_model_num[[recruit]], self$seq_yrs)
        cli_end()

      }
      cli_alert_info("Recruitment Probability:")
      cat_print(self$rec_prob)


    },


    #' @description
    #' Gets Recruitment Data
    get_recruit_data = function(model_num, seq_years){

      # TODO: 'Switch' if statement to switch statement
      if (model_num == 14 || model_num == 3) {
        return(EmpiricalRecruitModel$new(model_num,
                                  seq_years,
                                  with_ssb = FALSE))

      }else if(model_num == 5){
        return(BevertonHoltCurveModel$new())

      }else if(model_num == 6){
        return(RickerCurveModel$new())

      }else{
        return(NullRecruitModel$new())
      }

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

  ), private = list (
    cli_recruit_rule = function() {
      d <- cli_div(theme= list(rule= list(
        color = "cyan",
        "line-type" = "double")))
      cli_rule("Recruitment")
      cli_end(d)
    }
  )

)







