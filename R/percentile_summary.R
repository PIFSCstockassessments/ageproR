

#' @title
#' Percentile summary of the key results of AGEPRO projection output
#'
#' @description
#' Class Structure that includes user-defined options for setting
#' a specific percentile for the distributions of outputs.
#'
#' @template inp_con
#' @template nline
#'
#' @export
#'
percentile_summary <- R6Class(
  "percentile_summary",
  inherit = agepro_options_flags,
  public = list(

    #' @description
    #' Initialize the class
    #'
    #' @param perc User-defined percentile of projected distributions
    #'
    initialize = function(perc = 0){

      div_keyword_header(private$.keyword_name)
      cli_alert("Setting default report percentile value ...")

      self$report_percentile <- perc
      private$.enable_percentile_summary <- TRUE
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
        return(private$.report_percentile)
      }else {
        checkmate::assert_numeric(value, lower = 0, upper = 100)
        private$.report_percentile <- value
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

    .keyword_name = "perc",

    .report_percentile = NULL

  )
)
