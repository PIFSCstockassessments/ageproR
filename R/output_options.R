
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
    #' Logical flag to enable stock summary output.
    #'
    #' @param process_error_aux_files
    #' Logical flag to enable output of process_error auxiliary files
    #'
    #' @param export_r_data_frame
    #' Logical flag to enable AGEPRO ouyput to data.frame
    #'
    initialize = function(summary_report = FALSE,
                          process_error_aux_files = FALSE,
                          export_r_data_frame = TRUE) {

      div_keyword_header(private$.keyword_name)
      cli_alert("Setting AGEPRO projection output options ...")

      self$output_stock_summary <- summary_report
      self$output_process_error_aux_files <- process_error_aux_files
      self$output_data_frame <- export_r_data_frame

      self$print()

    },

    #' @description
    #' Formatted to print out output_option values
    #'
    print = function() {
      cli::cli_par()
      cli::cli_li(paste0("output_stock_summary: ",
                    "{.val {private$.output_stock_summary}} ",
                    "{.emph ({as.logical(private$.output_stock_summary)})}"))
      cli::cli_li(paste0("output_process_error_aux_files: ",
                    "{.val {private$.output_process_error_aux_files}} ",
                    "{.emph ({as.logical(private$.output_process_error_aux_files)})}"))
      cli::cli_li(paste0("output_data_frame (export AGEPRO output as data.frame): ",
                    "{.val {private$.output_data_frame}} ",
                    "{.emph ({as.logical(private$.output_data_frame)})}"))
      cli::cli_end()

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

    #' @field output_process_error_aux_files
    #' [Logical][base::logical] flag to output population and fishery processes
    #' simulated with lognormal process error (process_error parameters) to
    #' auxiliary output files
    output_process_error_aux_files = function(value) {
      if(missing(value)) {
        return(private$.output_process_error_aux_files)
      }else {
        private$.output_process_error_aux_files <-
          validate_logical_parameter(value)
      }
    },

    #' @field output_data_frame
    #' [Logical][base::logical] flag to output AGEPRO calculation engine
    #' projection results to R [data.frame][base::data.frame]. Default is
    #' `1` (or TRUE) at initialization.
    #'
    output_data_frame = function(value) {
      if(missing(value)){
        return(private$.output_data_frame)
      }else{
        private$.output_data_frame <- validate_logical_parameter(value)
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
    .output_process_error_aux_files = NULL,
    .output_data_frame = NULL




  )

)
