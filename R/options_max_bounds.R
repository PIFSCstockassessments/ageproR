

#' @title
#' Sets the maximum bounds of Weight(MT) and natural mortality
#'
#' @description
#' Class Structure that defines maximum bound for weight and natural mortality.
#' The values can be used to limit these values.
#'
#' The logical flag `enable_max_bounds` allows the user to
#' set values to this class fields: max_weight, max_natural_mortality. The flag
#' will also notify `agepro_model` if this keyword parameter is allowed to be
#' written to input file.
#'
#' Setting maximum bounds is an optional option for agepro models. It will check
#' if the max_bound class was initialized with values equal to the default
#' value for `max_weight` and `max_nat_mort`. If both values match the defaults,
#' then `enable_max_bounds` is flagged as FALSE; this keyword parameter is not
#' used in the agepro_model. Setting non-default values for all of its
#' parameters will set this flag to TRUE.
#'
#' @details
#' The max_bounds class (or BOUNDS) is recognized as a keyword
#' parameter used in the AGEPRO input file format, but it is optional.
#'
#' @include options_flags.R
#'
#' @export
#' @importFrom R6 R6Class
#'
max_bounds <- R6Class(
  "max_bounds",
  public = list(

    #' @field flag
    #' R6class containing option_flags
    flag = options_flags$new(),

    #' @description
    #' Initializes the class
    #'
    #' @param max_weight
    #' Maximum bound weight (MT). Default is 10.0
    #'
    #' @param max_nat_mort
    #' Maximum bound of natural mortality. Default is 1.0.
    #'
    initialize = function(max_weight = 10.0,
                          max_nat_mort = 1.0) {

      div_keyword_header(private$.keyword_name)

      # When agepro_model is reinitialized, reset the value for this class's
      # option_flag to NULL to cleanup any values it retained previously.
      private$reset_options_flags()

      self$max_weight <- max_weight
      self$max_natural_mortality <- max_nat_mort

      #If all max_bounds parameters are non-default values set the flag
      #set_enable_max_bounds to FALSE.
      default_max_weight <- formals(self$initialize)[["max_weight"]]
      default_max_nat_mort <- formals(self$initialize)[["max_nat_mort"]]
      if(all(c(all.equal(max_weight, default_max_weight),
               all.equal(max_nat_mort, default_max_nat_mort)))) {

        cli::cli_alert(paste0("max_bounds fields ",
                              "(max_weight, max_nat_mort) are default ",
                              "({default_max_weight}, ",
                              "{default_max_nat_mort}): "))
        cli::cli_alert_info("{private$.name_options_flag} to {.val {FALSE}}")
        suppressMessages(self$set_enable_max_bounds(FALSE))
      }else{
        cli::cli_alert(paste0("Setting max_bounds values: ",
                              "{symbol$info} {private$.name_options_flag} ",
                              "to {.val {TRUE}}"))
        self$set_enable_max_bounds(TRUE)
        self$print()
      }


    },

    #' @description
    #' Formatted to print out max_bounds values
    #'
    print = function(){

      cli::cli_alert_info(
        paste0("enable_max_bounds ",
               "{.emph (Specify bounds)}: ",
               "{.val {self$enable_max_bounds}}"))
      cli::cli_alert_info("max_weight: {.val {self$max_weight}}")
      cli::cli_alert_info("max_natural_mortality: {.val {self$max_natural_mortality}}")


    },


    #' @description
    #' Wrapper Function to toggle enable_max_bounds options_flag.
    #'
    #' The class will not accept from max_weight and max_natural_mortality
    #' values until enable_max_max_bounds is TRUE.
    #'
    #' @param x
    #' Logical value for enable_max_bounds options_flag
    #'
    set_enable_max_bounds = function(x) {

      checkmate::assert_logical(x)

      #Set value to options flags field reference "flag"
      self$flag$op$enable_max_bounds <- x

      cli::cli_alert(
        paste0("enable_max_bounds : ",
               "{.val ",
               "{self$flag$op$enable_max_bounds}}"))
    },

    #' @description
    #' Reads in the values from the keyword parameter BOUNDS from the
    #' AGEPRO Input file
    #'
    #' Note: enable_max_bounds must be set to TRUE.
    #'
    #' @template inp_con
    #' @template nline
    #'
    read_inp_lines = function(inp_con, nline) {

      cli::cli_alert_info("Reading {.strong {private$.keyword_name}}")

      nline <- nline + 1
      cli::cli_alert("Line {nline}:")
      inp_line <- read_inp_numeric_line(inp_con)

      self$max_weight <- inp_line[1]
      self$max_natural_mortality <- inp_line[2]

      cli::cli_ul(id = "bounds_inp_fields")
      cli::cli_li("max_weight: {.val {self$max_weight}}")
      cli::cli_li("max_natural_mortality: {.val {self$max_natural_mortality}}")
      cli::cli_end()

      return(nline)
    },

    #' @description
    #' Returns values from the class to the BOUNDS AGEPRO keyword parameter
    #' formatted as AGEPRO input file lines.
    #'
    #' @template delimiter
    #'
    get_inp_lines = function(delimiter = " ") {

      # Re-check fields before formatting.
      # In this case, do not allow NULL values to be passed.
      checkmate::assert_numeric(self$max_weight)
      checkmate::assert_numeric(self$max_natural_mortality)

      return(list(
        self$inp_keyword,
        paste(self$max_weight,
              self$max_natural_mortality,
              sep = delimiter)
      ))
    }




  ),
  active = list(

    #' @field max_weight
    #' The maximum value of fish weight, noting that there is lognormal
    #' sampling variation for weight at age values
    #'
    max_weight = function(value) {
      if(missing(value)) {
        if(is.null(private$.max_weight)){
          warning("max_weight is NULL", call. = FALSE)
        }
        return(private$.max_weight)
      }else{

        if(isFALSE(self$flag$op$enable_max_bounds)) {
          stop(paste0("enable_max_bounds flag is FALSE. ",
                      "Set flag to TRUE to set value.") )
        }

        checkmate::assert_numeric(value, len = 1, lower = 0)

        private$.max_weight <- value

      }
    },

    #' @field max_natural_mortality
    #' The maximum natural mortality rate, noting that there is lognormal
    #' sampling variation for natural mortality at age values
    max_natural_mortality = function(value) {
      if(missing(value)) {
        if(is.null(private$.max_natural_mortality)){
          warning("max_natural_morality is NULL", call. = FALSE)
        }
        return(private$.max_natural_mortality)
      }else{

        if(isFALSE(self$flag$op$enable_max_bounds)) {
          stop(paste0("enable_max_bounds flag is FALSE. ",
                      "Set flag to TRUE to set value.") )
        }

        checkmate::assert_numeric(value, len = 1, lower = 0)

        private$.max_natural_mortality <- value

      }
    },


    #' @field json_list_object
    #' Returns JSON list object of containing BOUNDS values
    json_list_object = function() {
      return(list(
        max_weight = self$max_weight,
        max_natural_mortality = self$max_natural_mortality
      ))
    },


    #' @field enable_max_bounds
    #' Logical field that flags if fields can be edited. To set
    #' the value use `set_enable_max_bounds` or field
    enable_max_bounds = function(value) {
      if(isTRUE(missing(value))){
        return(self$flag$op$enable_max_bounds)
      } else {
        #Validate and set value via set_enable_max_bounds
        self$set_enable_max_bounds(value)
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

    .keyword_name = "bounds",

    .max_weight = NULL,
    .max_natural_mortality = NULL,

    .name_options_flag = "enable_max_bounds",

    reset_options_flags = function() {
      #Reset option_flag to NULL at initialization
      if(isFALSE(is.null(self$flag$op$enable_max_bounds))){
        cli::cli_alert(paste0("Reset {private$.name_options_flag} ",
                              "for initialization"))
        self$flag$op$enable_max_bounds <- NULL
        }
    }



  )
)
