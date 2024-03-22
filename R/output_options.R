
#' @title
#' AGEPRO projection output options.
#'
#' @description
#' Class Structure that includes user-defined options to enable auxiliary or
#' options to export AGEPRO output
#'
#' @import cli
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#'
#' @export
#'
output_options <- R6Class(
  "output_options",
  public = list(

    #' @description
    #' Initializes the class
    #'
    #' @param summary_report
    #' description
    #'
    #' @param process_error_aux_files
    #' Logical flag to
    #'
    initialize = function(summary_report = FALSE,
                          process_error_aux_files = FALSE,
                          export_r = TRUE) {

      div_keyword_header(private$.keyword_name)
      cli_alert(paste0("Setting up values for total proportion of mortality ",
                       "prior to spawn ..."))

    }

  ),
  active = list(

    #' @field output_stock_summary
    #' [Logical][base::logical] flag to output stock summary information
    output_stock_summary = function(value) {
      if(missing(value)) {
        return(private$.output_stock_summary)
      }else{
        private$.output_stock_summary <- validate_logical_parameter(value)
      }
    },

    #' @field output_process_error_auxiliary_data
    #' [Logical][base::logical] flag to output population and fishery processes
    #' simulated with lognormal process error (process_error parameters) to
    #' auxiliary output files
    output_process_error_auxiliary_data = function(value) {
      if(missing(value)) {
        return(private$.output_process_error_auxiliary_data)
      }else {
        private$.output_process_error_auxiliary_data <-
          validate_logical_parameter(value)
      }
    },

    #' @field export_r
    #' [Logical][base::logical] flag to output AGEPRO calculation engine
    #' projection results to R [data.frame][base::data.frame]. Default is
    #' `1` (or TRUE) at initialization.
    #'
    export_r = function(value) {
      if(missing(value)){
        return(private$.export_r)
      }else{
        private$.export_r <- validate_logical_parameter(value)
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

    .keyword_name = "options",

    .output_stock_summary = NULL,
    .output_process_error_auxiliary_data = NULL,
    .export_r = NULL




  )

)
