
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
    #' @param ssb_threshold
    #' Spawning Biomass threshold express in biomass output units (MT).
    #'
    #' @param stockbio_threshold
    #' Stock biomass threshold expressed in biomass output units(MT).
    #'
    #' @param meanbio_threshold
    #' Mean biomass threshold expressed in biomass output units (MT)
    #'
    #' @param fmort_threshold
    #' Fishing mortality threshold
    #'
    initalize = function(ssb_threshold = 0,
                         stockbio_threshold = 0,
                         meanbio_threshold = 0,
                         fmort_threshold = 0) {

      div_keyword_header(private$.keyword_name)

      # When agepro_model is reinitialized, reset the value for this class's
      # option_flag to NULL to cleanup any values it retained previously.
      private$reset_options_flags()

      self$ssb_threshold <- ssb_threshold
      self$stock_biomass_threshold <- stockbio_threshold
      self$mean_biomass_threshold <- meanbio_threshold
      self$fishing_mortality_threshold <- fmort_threshold

      if(all(c(all.equal(ssb_threshold, 0),
               all.equal(stockbio_threshold, 0),
               all.equal(meanbio_threshold, 0),
               all.equal(fmort_threshold,0))))  {
        cli::cli_alert(paste0("Default values set, ",
                              "options_flag enable_refernce_points to FALSE"))
        suppressMessages(self$set_enable_refernce_points(FALSE))
      }else{
        cli::cli_alert(paste0("Values for reference_points set. ",
                              "Enable options_flag enable_reference_points ",
                              "as TRUE"))
        self$set_enable_reference_points(TRUE)
        self$print()
      }

    },

    #' @description
    #' Formatted to print out reference_points values
    #'
    print = function(){

      cli::cli_alert_info(
        paste0("reference_points: ",
               "Enable Reference Point Threshold Report ",
               "{.emph (enable_reference_points)}: ",
               "{.val {self$enable_reference_points}}"))
      cli::cli_ul(id = "reference_points_fields")
      cli::cli_li("ssb_threshold: {.val {self$ssb_threshold}}")
      cli::cli_li(paste0("stock_biomass_threshold: ",
                         "{.val {self$stock_biomass_threshold}}"))
      cli::cli_li(paste0("stock_biomass_threshold: ",
                         "{.val {self$stock_biomass_threshold}}"))
      cli::cli_li(paste0("fishing_mortality_threshold: ",
                         "{.val {self$fishing_mortality_threshold}}"))
      cli::cli_end()

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

        if(isFALSE(self$enable_reference_points)) {
          stop(paste0("enable_reference_points flag is FALSE. ",
                      "Set flag to TRUE to set value.") )
        }

        checkmate::assert_numeric(value)

        private$.ssb_threshold <- value

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

        if(isFALSE(self$enable_reference_points)) {
          stop(paste0("enable_reference_points flag is FALSE. ",
                      "Set flag to TRUE to set value.") )
        }

        checkmate::assert_numeric(value)

        private$.stock_biomass_threshold <- value


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

        if(isFALSE(self$enable_reference_points)) {
          stop(paste0("enable_reference_points flag is FALSE. ",
                      "Set flag to TRUE to set value.") )
        }

        checkmate::assert_numeric(value)

        private$.mean_biomass_threshold <- value


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

        if(isFALSE(self$enable_reference_points)) {
          stop(paste0("enable_reference_points flag is FALSE. ",
                      "Set flag to TRUE to set value.") )
        }

        checkmate::assert_numeric(value)

        private$.fishing_mortality_threshold <- value


      }
    },

    #' @field enable_reference_points
    #' Logical field that flags if fields can be edited. This class will not
    #' accept new values to its fields or allow it to be exported to input file
    #' until this option flag is TRUE.
    enable_reference_points = function(value) {
      if(isTRUE(missing(value))){
        return(self$flag$op$enable_reference_points)
      } else {
        self$set_enable_reference_points(value)
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

    # Wrapper Function to toggle enable_reference_points options_flag.
    set_enable_reference_points = function(x) {

      checkmate::assert_logical(x, null.ok = TRUE)

      #Set value to options flags field reference "flag"
      self$flag$op$enable_reference_points <- x

      cli::cli_alert(
        paste0("enable_reference_points : ",
               "{.val ",
               "{self$flag$op$enable_reference_points}}"))


    },

    reset_options_flags = function() {
      #Reset option_flag to NULL at initialization
      if(isFALSE(is.null(self$flag$op$enable_max_bounds))){
        cli::cli_alert("Reset enable_reference_points")
        self$flag$op$enable_max_bounds <- NULL
      }
    }


  )
)
