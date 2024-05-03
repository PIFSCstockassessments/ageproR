

#' @title
#' Sets maximum bounds of Weight(MT) and natural mortality
#'
#' @description
#' Class Structure that defines maximum bound for weight and natural mortality.
#' The values can be used to limit these values.
#'
#' @details
#' The max_bounds class (or BOUNDS) is recognized as a keyword
#' parameter used in the AGEPRO input file format, but it is optional.
#' During agepro_model initialization, the option_flag enable_max_bounds
#' will be FALSE. The field enable_max_bounds must be set to TRUE to set values
#' this class.
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
      #Reset enable_max_bounds option_flag to NULL
      private$reset_options_flags()

      cli::cli_alert("Setting maximum bounds for weight and natural mortality")

      self$max_weight <- max_weight
      self$max_natural_mortality <- max_nat_mort

      suppressMessages(self$set_enable_max_bounds(FALSE))

      self$print()
    },

    #' @description
    #' Formatted to print out max_bounds values
    #'
    print = function(){

      cli::cli_alert_info(
        paste0("max_bounds: ",
               "Specify bounds {.emph (flag$op$enable_max_bounds)}: ",
               "{.val {self$flag$op$enable_max_bounds}}"))
      cli::cli_ul(id = "max_bounds_fields")
      cli::cli_li("max_weight: {.val {self$max_weight}}")
      cli::cli_li("max_natural_mortality: {.val {self$max_natural_mortality}}")
      cli::cli_end()

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

      self$flag$op$enable_max_bounds <- x

      cli::cli_alert(
        paste0("enable_max_bounds : ",
               "{.val ",
               "{self$flag$op$enable_max_bounds}}"))
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

    #' @field enable_max_bounds
    #' Read-only logical field that flags if fields can be edited. To set
    #' the value use `set_enable_max_bounds` or field
    enable_max_bounds = function(value) {
      if(isFALSE(missing(value))){
        err_msg <-
          paste0("active binding is read only. ",
                 "To set value for enable_max_bounds ",
                 "use 'set_enable_max_bounds' or ",
                 "set value to field 'flag$op$enable_max_bounds'")
        stop(err_msg, call. = FALSE)
      }
      return(self$flag$op$enable_max_bounds)
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

    reset_options_flags = function() {
      #Reset option_flag to NULL at initialization
      if(isFALSE(is.null(self$flag$op$enable_max_bounds))){
        cli::cli_alert("Reset enable_max_bounds")
        self$flag$op$enable_max_bounds <- NULL
        }
    }



  )
)
