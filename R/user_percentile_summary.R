

#' @title
#' Percentile summary of the key results of AGEPRO projection output
#'
#' @description
#' Class Structure that includes user-defined options for setting
#' a specific percentile for the distributions of outputs.
#'
#' @export
#'
user_percentile_summary <- R6Class(
  "user_percentile_summary",
  public = list(

    #' @description
    #' Initialize the class
    #'
    #' @param perc User-defined percentile of projected distributions
    #'
    initialize = function(perc = NULL){

      div_keyword_header(private$.keyword_name)
      checkmate::assert_numeric(perc, lower = 0, upper = 100,
                                null.ok = TRUE, len = 1)

      self$report_percentile <- perc

    },


    #' @description
    #' Formatted to print out output_option values
    #'
    print = function(){
      cli::cli_alert("report_percentile: {.val {self$report_percentile}}")
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
        return(private$.report_percentile)
      }else {
        checkmate::assert_numeric(value, null.ok = TRUE, lower = 0, upper = 100)
        private$.report_percentile <- value
      }
    },


    #' @field json_list_object
    #' Returns JSON list object of containing output_options values
    json_list_object = function() {
      return(list(
        percentile_report_value = self$report_percentile
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

    .report_percentile = NULL

  )
)
