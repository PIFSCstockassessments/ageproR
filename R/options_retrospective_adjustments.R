
#' @title
#' Vector of retrospective bias-correction coefficients to adjust
#' to the initial population of numbers of age.
#'
#' @description
#' This is the vector of age-specific numbers at age multipliers for an
#' initial population size at age vector if retrospective bias adjustment
#' is applied.
#'
#' The logical flag `enable_retrospective_adjustments` allows the user to
#' set values to this class fields. The flag will also notify `agepro_model`
#' if this keyword parameter is allowed to be written to input file.
#'
#' If this class is initialized with default values, it is presumed that this
#' keyword parameter is not used in the agepro_model. Therefore,
#' `enable_retrospective_adjustments` is flagged as FALSE. Valid non-default
#' values will set this flag to TRUE.
#'
#' @details
#' The retrospective_adjustments class (`RETROADJUST`) is recognized as a keyword
#' parameter used in the AGEPRO input file format, but it is optional.
#'
#' @include options_flags.R
#'
#' @export
#' @importFrom R6 R6Class
#'
retrospective_adjustments <- R6Class(
  "retrospective_adjustments",
  public = list(

    #' @field flag
    #' R6class containing option_flags
    flag = options_flags$new(),

    #' @description
    #' Initializes the class
    #'
    #' @template enable_cat_print
    #' @param retro_adjust
    #' Vector for retrospective bias adjustment
    #' @param num_ages
    #' Model's number of ages derived from general_params
    #' num_ages active binding.
    #'
    initialize = function(retro_adjust,
                          num_ages,
                          enable_cat_print = TRUE) {

      div_keyword_header(private$.keyword_name)

      if(missing(retro_adjust)){
        retro_adjust <- 0
      }

      # When agepro_model is reinitialized, reset the value for this class's
      # option_flag to NULL to cleanup any values it retained previously.
      private$reset_options_flags()

      self$retro_adjust <- retro_adjust

      if(all.equal(retro_adjust,0)){
        cli::cli_alert(paste0("retrospective_adjustments fields ",
                              "(retro_adjust) ",
                              "are default: "))
        cli::cli_alert_info("{private$.name_options_flag} to {.val {FALSE}}")
        suppressMessages(private$set_enable_retrospective_adjustments(FALSE))
      }else {
        cli::cli_alert(paste0("Setting retrospective_adjustments values: ",
                              "{symbol$info} {private$.name_options_flag} ",
                              "to {.val {TRUE}}"))
        private$set_enable_retrospective_adjustments(TRUE)
        self$print(enable_cat_print)
      }

    },

    #' @description
    #' Formatted to print out retrospective_adjustments values
    #'
    #' @template enable_cat_print
    #' @template elipses
    #'
    print = function(enable_cat_print = TRUE, ...){

      cli::cli_alert_info(
        paste0("enable_retrospective_adjustments: ",
               "{.emph (Specify Retrospective Adjustment factors)}: ",
               "{.val {self$enable_retrospective_adjustments}}"))
      cli::cli_ul(id = "retrospective_adjustments_fields")
      cli::cli_alert_info("retro_adjust: ")

      #Verbose flag check
      if(enable_cat_print){
        #Allow `cli::cat_print` message
        print_parameter_table(self$retro_adjust, ...)
      }else {
        #Suppress `cli::cat_print` message
        capture.output( x <- print_parameter_table(self$retro_adjust, ...))
      }

      cli::cli_end()

    },


    #' @description
    #' Reads in the values from the keyword parameter RETROADJUST from the
    #' AGEPRO Input file
    #'
    #' Note: enable_retrospective_adjustments must be set to TRUE.
    #'
    #' @template inp_con
    #' @template nline
    #' @param num_ages Model's number of ages derived from general_params
    #' num_ages active binding.
    #'
    read_inp_lines = function(inp_con, nline, num_ages) {

      if(isFALSE(self[[private$.name_options_flag]])){
        stop(private$unenabled_options_flag_message())
      }

      cli::cli_alert_info("Reading {.strong {private$.keyword_name}}")

      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      self$retro_adjust <- inp_line
      count_ages <- length(self$retro_adjust)

      # Throw error if vector length of retro_adjust does not
      # match num_ages
      if(!isTRUE(all.equal(count_ages, num_ages))) {
        stop(paste0("Length of Retrosepctive coefficeient vector does not ",
                    "match model's number of ages (", num_ages, ")"))
      }
      names(self$retro_adjust) <- paste0("Age", 1:count_ages)

      cli::cli_alert(c("Line {nline}: ",
                       "retro_adjust: ",
                       "{.val {inp_line}} ",
                       "{.emph ({num_ages} Age{?s})}"))

      return(nline)

    },

    #' @description
    #' Returns values from the class to the RETROADJUST AGEPRO keyword parameter
    #' formatted as AGEPRO input file lines.
    #'
    #' @template delimiter
    #'
    get_inp_lines = function(delimiter = "  ") {

      return(list(
        self$inp_keyword,
        paste(self$retro_adjust, collapse = "  ")
      ))

    }



  ),
  active = list(

    #' @field retro_adjust
    #' This is the vector of age-specific numbers at age multipliers for an
    #' initial population size at age vector if retrospective bias adjustment
    #' is applied.
    retro_adjust = function(value) {
      if(isTRUE(missing(value))){
        return(private$.retro_adjust)
      }else {

        if(isFALSE(self$enable_retrospecttive_adjustments)) {
          stop(private$unenabled_options_flag_message(), call. = FALSE)
        }

        checkmate::assert_numeric(value, lower = 0)

        private$.retro_adjust <- value
      }
    },

    #' @field enable_retrospective_adjustments
    #' Logical field that flags if fields can be edited. This class will not
    #' accept new values to its fields or allow it to be exported to input file
    #' until this option flag is TRUE.
    enable_retrospective_adjustments = function(value) {
      if(isTRUE(missing(value))){
        return(self$flag$op$enable_retrospective_adjustments)
      } else {
        private$set_enable_retrospective_adjustments(value)
      }

    },

    #' @field json_list_object
    #' Returns JSON list object of containing SCALE values
    json_list_object = function() {
      return(list(
        retro_adjust = self$retro_adjust
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

    .retro_adjust = NULL,

    .keyword_name = "retroadjust",
    .name_options_flag = "enable_retrospective_adjustments",


    # Wrapper Function to toggle enable_retrospective_adjustments options_flag.
    set_enable_retrospective_adjustments = function(x) {

      checkmate::assert_logical(x, null.ok = TRUE)

      #Set value to options flags field reference "flag"
      self$flag$op[[private$.name_options_flag]] <- x

      cli::cli_alert(
        paste0("{private$.name_options_flag} : ",
               "{.val ",
               "{self$flag$op[[private$.name_options_flag]]}}"))


    },

    reset_options_flags = function() {
      #Reset option_flag to NULL at initialization

      if(isFALSE(is.null(self$flag$op[[private$.name_options_flag]]))){
        cli::cli_alert(paste0("Reset {private$.name_options_flag} ",
                              "for initalization"))
        self$flag$op[[private$.name_options_flag]] <- NULL
      }
    },

    # Error message when setting retrospective_coefficient values while
    # enable_retrospective_adjustments is FALSE
    unenabled_options_flag_message = function() {
      return(invisible(
        paste0(private$.name_options_flag,
               " is FALSE. Set flag to TRUE to set value.")
      ))
    }

  )
)
