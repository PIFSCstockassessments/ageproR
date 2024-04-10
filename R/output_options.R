
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
#' @template inp_con
#' @template nline
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
    #' [Logical][base::logical] flag to enable stock summary output.
    #'
    #' @param process_error_aux_files
    #' [Logical][base::logical] flag to enable output of process_error
    #' auxiliary files
    #'
    #' @param export_r_data_frame
    #' [Logical][base::logical] flag to enable AGEPRO output to data.frame
    #'
    #' @param enable_percentile
    #' [Logical][base::logical] flag to enable percentile summary of the key
    #' results in the output file.
    #'
    initialize = function(summary_report = FALSE,
                          process_error_aux_files = FALSE,
                          export_r_data_frame = TRUE,
                          enable_percentile = FALSE) {

      div_keyword_header(private$.keyword_name)
      cli_alert("Setting AGEPRO projection output options ...")

      self$output_stock_summary <- summary_report
      self$output_process_error_aux_files <- process_error_aux_files
      self$output_data_frame <- export_r_data_frame
      self$enable_percentile_distributions <- enable_percentile

    },

    #' @description
    #' Formatted to print out output_option values
    #'
    print = function() {
      cli::cli_ul(id = "output_options_fields")
      cli::cli_li(paste0("output_stock_summary: ",
                    "{.val {private$.output_stock_summary}} ",
                    "{.emph ({as.logical(private$.output_stock_summary)})}"))
      cli::cli_li(paste0("output_process_error_aux_files: ",
                    "{.val {private$.output_process_error_aux_files}} ",
                    "{.emph ({as.logical(private$.output_process_error_aux_files)})}"))
      cli::cli_li(paste0("output_data_frame (export AGEPRO output as data.frame): ",
                    "{.val {private$.output_data_frame}} ",
                    "{.emph ({as.logical(private$.output_data_frame)})}"))
      cli::cli_end(id = "output_options_fields")

    },

    #' @description
    #' Reads in the values from the keyword parameter OPTIONS from the
    #' AGEPRO Input file
    #'
    read_inp_lines = function(inp_con, nline) {

      cli::cli_alert_info("Reading {.strong {private$.keyword_name}}")

      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      self$output_stock_summary <- inp_line[1]
      self$output_process_error_aux_files <- inp_line[2]
      self$output_data_frame <- inp_line[3]

      cli::cli_alert(paste0("Line {nline} : ",
                            "Reading AGEPRO projection output options ..."))

      cli::cli_div(theme= list(ul = list(`margin-left` = 2, before = "")))
      self$print()

      return(nline)
    },

    #' @description
    #' Returns values from the output_options (OPTIONS)
    #' AGEPRO keyword parameter formatted as AGEPRO input file lines.
    #'
    #' @template delimiter
    #'
    get_inp_lines = function(delimiter = " "){
      return(list(
        self$inp_keyword,
        paste(self$output_stock_summary,
              self$output_process_error_aux_files,
              self$output_data_frame,
              sep = delimiter)
      ))
    }

  ),
  active = list(

    #' @field output_stock_summary
    #' [Logical][base::logical] flag to output stock summary information
    output_stock_summary = function(value) {
      if(missing(value)) {
        return(private$.output_stock_summary)
      }else{

        # Calling Handler to wrap field name w/ validate_logical_parameter
        # message
        withCallingHandlers(
          message = function (cnd) {
            cli::cli_alert(
              paste0("output_summary_report: ",
                     "{sub('\u2192 ', '', conditionMessage(cnd))}"))

            rlang::cnd_muffle(cnd)
          },

          private$.output_stock_summary <- validate_logical_parameter(value)
        )
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

        # Calling Handler to wrap field name w/ validate_logical_parameter
        # message
        withCallingHandlers(
          message = function(cnd) {
            cli::cli_alert(
              paste0("output_process_error_aux_files: ",
                     "{sub('\u2192 ', '', conditionMessage(cnd))}"))

            rlang::cnd_muffle(cnd)
          },
          private$.output_process_error_aux_files <-
            validate_logical_parameter(value)
        )
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

        # Calling Handler to wrap field name w/ validate_logical_parameter
        # message
        withCallingHandlers(
          message = function(cnd) {
            cli::cli_alert(
              paste0("output_data_frame: ",
                     "{sub('\u2192 ', '', conditionMessage(cnd))}"))
            rlang::cnd_muffle(cnd)
          },

          private$.output_data_frame <- validate_logical_parameter(value)
        )

      }
    },

    #' @field enable_percentile_distributions
    #' [Logical][base::logical] flag to allow percentile summary of the key
    #' results in the output file.
    #'
    enable_percentile_distributions = function(value){
      if(missing(value)){
        return(private$.enable_percentile_distributions)
      }else{

        # Calling Handler to wrap field name w/ validate_logical_parameter
        # message
        withCallingHandlers(
          message = function(cnd) {
            cli::cli_alert(
              paste0("enable_perenctile_distributions: ",
                     "{sub('\u2192 ', '', conditionMessage(cnd))}"))
            rlang::cnd_muffle(cnd)
          },
          private$.enable_percentile_distributions <-
            validate_logical_parameter(value)
        )

      }
    },

    #' @field json_list_object
    #' Returns JSON list object of containing output_options values
    json_list_object = function() {
      return(list(
        stock_summary_flag = self$output_stock_summary,
        process_error_aux_data_flag = self$output_process_error_aux_files,
        export_R_flag = self$output_data_frame
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

    .keyword_name = "options",

    .output_stock_summary = NULL,
    .output_process_error_aux_files = NULL,
    .output_data_frame = NULL,
    .enable_percentile_distributions = NULL




  )

)
