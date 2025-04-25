

#' @title
#' Percentile summary of the key results of AGEPRO projection output
#'
#' @description
#' Class Structure that includes user-defined options for setting
#' a specific percentile for the distributions of outputs.
#'
#' The logical flag `enable_percentile_summary` allows the user to
#' set values to this class field: report_percentile. The flag will also notify
#' `agepro_model` if this keyword parameter is allowed to be written to
#' input file.
#'
#' If percentile_summary is initialized with default values, it is
#' presumed that this keyword parameter is not used in the agepro_model.
#' Therefore, `enable_percentile_summary` is flagged as FALSE. Valid non-
#' default values will set this flag to TRUE.#'
#'
#' @details
#' The percentile_summary class (or `PERC`) is an optional keyword
#' parameter used in the AGEPRO input file format.
#'
#' @export
#'
percentile_summary <- R6Class(
  "percentile_summary",
  public = list(

    #' @field flag
    #' A R6class containing
    #' [flags for optional AGEPRO options][ageproR::options_flags]
    flag = options_flags$new(),

    #' @description
    #' Initializes the class
    #'
    #' @param perc User-defined percentile of projected distributions
    #'
    initialize = function(perc = 0){

      div_keyword_header(private$.keyword_name)

      #' When agepro_model is reinitialized, reset the value for this class's
      #' option_flag to NULL to cleanup any values it retained previously.
      private$reset_options_flags()

      # Presume default if perc is 0.
      if(isTRUE(all.equal(perc,0))){
        cli::cli_alert(paste0("percentile_summary fields ",
                              "(report_percentile) ",
                              "are default: "))
        cli::cli_alert_info("{private$.name_options_flag} to {.val {FALSE}}")
        self$report_percentile <- perc

        suppressMessages(self$set_enable_percentile_summary(FALSE))

        return()

      }

      cli::cli_alert(paste0("Setting percentile_summary values: ",
                            "{symbol$info} {private$.name_options_flag} ",
                            "to {.val {TRUE}}"))
      cli::cli_alert("Set report_percentile to {.val {perc}} ..")

      self$report_percentile <- perc
      self$set_enable_percentile_summary(TRUE)
      self$print()

    },


    #' @description
    #' Formatted to print out output_option values
    #'
    print = function(){

      cli::cli_alert_info(
        paste0("enable_percentile_summary ",
               "{.emph (Request Percentile Report)}: ",
               "{.val {self$enable_percentile_summary}}"))
      cli::cli_alert_info("report_percentile: {.val {self$report_percentile}}")
    },

    #' @description
    #' Wrapper Function to toggle enable_percentile_summary options_flag.
    #'
    #' The percentile_summary class will not accept values until it is
    #' enable_percentile_summary is TRUE.
    #'
    #' @param x
    #' Logical value for enable_percentile_summary options_flag
    #'
    set_enable_percentile_summary = function(x) {

      checkmate::assert_logical(x)

      #Set value to options flags field reference "flag"
      self$flag$op$enable_percentile_summary <- x

      cli::cli_alert(
        paste0("{private$.name_options_flag} : ",
               "{.val ",
               "{self$flag$op$enable_percentile_summary}}"))


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
        if(isFALSE(self$flag$op$enable_percentile_summary)) {
          stop(paste0("{private$.name_options_flag} flag is FALSE. ",
                        "Set flag to TRUE to set value.") )
        }

        private$.report_percentile <- value
      }

    },

    #' @field enable_percentile_summary
    #' Read-only logical field that flags if fields can be edited. To set
    #' the value use `set_enable_percentile_summary` or field
    enable_percentile_summary = function(value) {
      if(isTRUE(missing(value))){
        return(self$flag$op$enable_percentile_summary)
      }else{
        #Validate and set value via set_enable_percentile_summary
        self$set_enable_percentile_summary(value)
      }

    },


    #' @field json_list_object
    #' Returns JSON list object of containing options_output values
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

    .name_options_flag = "enable_percentile_summary",

    reset_options_flags = function() {
      #Reset option_flag to NULL at initialization
      if(isFALSE(is.null(self$flag$op$enable_percentile_summary))){
        cli::cli_alert(paste0("Reset {private$.name_options_flag}",
                              "for initialization"))
        self$flag$op$enable_percentile_summary <- NULL
      }
    }

  )
)
