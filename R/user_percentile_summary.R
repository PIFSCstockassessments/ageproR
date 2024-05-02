

#' @title
#' Percentile summary of the key results of AGEPRO projection output
#'
#' @description
#' Class Structure that includes user-defined options for setting
#' a specific percentile for the distributions of outputs.
#'
#' @details
#' The user_percentile_summary class (or `PERC`) is recognized as a keyword
#' parameter used in the AGEPRO input file format, but it is optional.
#' Initialized agepro_models flags the class  as FALSE until  a will be initialize with NULL
#' report_percentile.
#'
#'
#' @export
#'
user_percentile_summary <- R6Class(
  "user_percentile_summary",
  public = list(

    #' @field flag
    #' A R6class containing
    #' [flags for optional AGEPRO options][ageproR::options_flags]
    flag = options_flags$new(),

    #' @description
    #' Initializes the class
    #'
    #' When agepro_model is reinitialized, reset the value for this class's
    #' option_flag enable_user_percentile_summary to NULL to cleanup any values
    #' it retained previously.
    #'
    #' @param perc User-defined percentile of projected distributions
    #'
    initialize = function(perc = 0){

      div_keyword_header(private$.keyword_name)
      #Reset enable_user_percentile_summary option_flag to NULL
      private$reset_options_flags()

      # Presume default if perc is 0.
      if(isTRUE(all.equal(perc,0))){
        cli::cli_alert("Set default report_percentile value to 0 ..")
      }else{
        cli::cli_alert("Set report_percentile to {.val {perc}} ..")
      }

      checkmate::assert_numeric(perc, lower = 0, upper = 100, len = 1)

      self$report_percentile <- perc

    },


    #' @description
    #' Formatted to print out output_option values
    #'
    print = function(){

      cli::cli_alert_info("report_percentile: {.val {self$report_percentile}}")
    },

    #' @description
    #' Reads in the values from the keyword parameter PERC from the
    #' AGEPRO Input file
    #'
    #' @template inp_con
    #' @template nline
    #'
    read_inp_lines = function(inp_con, nline) {

      cli::cli_alert_info("Reading {.strong {private$.keyword_name}}")

      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      self$report_percentile <- inp_line

      cli::cli_alert(paste0("Line {nline} : ",
                            "report_percentile: ",
                            "{.val {self$report_percentile}}"))

      return(nline)
    },

    #' @description
    #' Returns values from the class to the PERC AGEPRO keyword parameter
    #' formatted as AGEPRO input file lines.
    #'
    #' @template delimiter
    #'
    get_inp_lines = function(delimiter = " ") {

      # Re-check fields before formatting.
      # In this case, do not allow NULL values to be passed.
      checkmate::assert_numeric(self$report_percentile,
                                lower = 0, upper = 100)

      return(list(
        self$inp_keyword,
        self$report_percentile
      ))

    }

  ),
  active = list(

    #' @field report_percentile
    #' User-defined percentile for reporting the percentile of the projected
    #' distribution of the following quantities of interest by year:
    #' spawning stock biomass, stock biomass on January 1st, mean biomass,
    #' combined catch biomass, landings, fishing mortality, and stock
    #' numbers at age
    #'
    report_percentile = function(value) {
      if(missing(value)){
        if(is.null(private$.report_percentile)){
          warning("report_percentile is NULL", call. = FALSE)
        }
        return(private$.report_percentile)
      }else {

        checkmate::assert_numeric(value, null.ok = TRUE, len = 1,
                                  lower = 0, upper = 100)
        if(isFALSE(self$flag$op$enable_user_percentile_summary)) {
          stop(paste0("enable_user_percentile_summary flag is FALSE. ",
                        "Set flag to TRUE to set value.") )
        }

        private$.report_percentile <- value
      }

    },


    #' @field json_list_object
    #' Returns JSON list object of containing output_options values
    json_list_object = function() {
      return(list(
        report_percentile_value = self$report_percentile
      ))
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

    .keyword_name = "perc",

    .report_percentile = NULL,

    reset_options_flags = function() {
      #Reset option_flag to NULL at initialization
      if(isFALSE(is.null(self$flag$op$enable_user_percentile_summary))){
        cli::cli_alert("Set enable_user_percentile_summary to FALSE")
        self$flag$op$enable_user_percentile_summary <- NULL
      }
    }

  )
)
