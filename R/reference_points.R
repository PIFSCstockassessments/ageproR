
#' @title
#' Values to output optional agepro model's reference point threshold report.
#'
#' @description
#' Class Structure that defines biomass thresholds for spawaning biomass
#' \eqn{(B_{S,\ THRESHOLD})}, mean biomass \eqn{(\bar B_{S,\ THRESHOLD})},
#' and total stock biomass\eqn{(B_{T,\ THRESHOLD})}, including the threshold
#' rate for fishing mortality \eqn{(F_{THRESHOLD})}.
#'
#' The logical flag `enable_reference_points` allows the user to
#' set values to this class fields. The flag will also notify `agepro_model`
#' if this keyword parameter is allowed to be written to input file.
#'
#' If this class is initialized with default values, it is presumed that this
#' keyword parameter is not used in the agepro_model. Therefore,
#' `enable_reference_points` is flagged as FALSE. Valid non-default values will
#' set this flag to TRUE.
#'
#' @details
#' The reference_points class (`BOUNDS`) is recognized as a keyword
#' parameter used in the AGEPRO input file format, but it is optional.
#'
#' @include optional_options_flags.R
#'
#' @export
#' @importFrom R6 R6Class
#'
reference_points <- R6Class(
  "reference_points",
  public = list(

    #' @field flag
    #' R6class containing option_flags
    flag = options_flags$new(),

    #' @description
    #' Initializes the class
    #'
    #' @param x Projected year value or vector
    #'
    initalize = function(x) {



    }

  ),
  active = list(

    #' @field ssb_threshold
    #' Threshold of spawning biomass
    ssb_threshold = function(value) {
      if(missing(value)){
        if(is.null(private$.ssb_threshold)){
          warning("ssb_threshold is NULL", call. = FALSE)
        }
        return(private$.ssb_threshold)
      }else{

      }
    },

    #' @field stock_biomass_threshold
    #' Threshold of total stock biomass
    stock_biomass_threshold = function(value) {
      if(missing(value)){
        if(is.null(private$.stock_biomass_threshold)){
          warning("stock_biomass_threshold is NULL", call. = FALSE)
        }
        return(private$.stock_biomass_threshold)
      }else{

      }
    },


    #' @field mean_biomass_threshold
    #' Threshold for mean biomass
    mean_biomass_threshold = function(value) {
      if(missing(value)){
        if(is.null(private$.mean_biomass_threshold)){
          warning("mean_biomass_threshold is NULL", call. = FALSE)
        }
        return(private$.mean_biomass_threshold)
      }else{

      }
    },

    #' @field fishing_mortality_threshold
    #' Threshold for the fishing mortality rate for annual total fishing
    #' mortality calculated across all fleets.
    fishing_mortality_threshold = function(value) {
      if(missing(value)) {
        if(is.null(private$.fishing_mortality_threshold)){
          warning("fishing_mortality_threshold is NULL", call. = FALSE)
        }
        return(private$.fishing_mortality_threshold)
      }else {

      }
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

  ),
  private = list(

    .keyword_name = "refpoint",

    .ssb_threshold = NULL,
    .stock_biomass_threshold = NULL,
    .mean_biomass_threshold = NULL,
    .fishing_mortality_threshold = NULL,


    reset_options_flags = function() {
      #Reset option_flag to NULL at initialization
      if(isFALSE(is.null(self$flag$op$enable_max_bounds))){
        cli::cli_alert("Reset enable_reference_points")
        self$flag$op$enable_max_bounds <- NULL
      }
    }


  )
)
