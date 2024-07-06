

#' @title Scale Factors
#'
#' @description
#' Input information on scaling factors for biomass, recruitment, and stock size
#'
#' @include optional_options_flags.R
#'
#' @export
#' @importFrom R6 R6Class
#'
scaling_factors <-R6Class(
  "scaling_factors",
  public = list(

    #' @field flag
    #' R6class containing option_flags
    flag = options_flags$new(),

    #' @description
    #' Initializes the class
    #'
    #' @param scale_bio
    #' Output units of biomass express in thousand metric units
    #'
    #' @param scale_recruit
    #' Output Units of Recruitment Numbers
    #'
    #' @param scale_stock_size
    #' Output Units of Stock Size Numbers
    #'
    initialize = function(scale_bio = 0,
                          scale_recruit = 0,
                          scale_stock_size = 0) {

      div_keyword_header(private$.keyword_name)

      # When agepro_model is reinitialized, reset the value for this class's
      # option_flag to NULL to cleanup any values it retained previously.
      private$reset_options_flags()

      self$biomass_scale <- scale_bio
      self$recruitment_scale <- scale_recruit
      self$stock_size_scale <- scale_stock_size

      #Check for defaults
      if(all(c(all.equal(scale_bio, 0),
               all.equal(scale_recruit, 0),
               all.equal(scale_stock_size, 0)))) {
        cli::cli_alert(paste0("Default values set, ",
                              "options_flag enable_scaling_factors to FALSE"))
        suppressMessages(private$set_enable_scaling_factors(FALSE))
      } else{
        cli::cli_alert(paste0("Values for reference_points set. ",
                              "Enable options_flag enable_scaling_factors ",
                              "as TRUE"))
        private$set_enable_scaling_factors(TRUE)
        self$print()
      }
    },

    #' @description
    #' Formatted to print out scaling_factors values
    #'
    print = function(){

      cli::cli_alert_info(
        paste0("Specify Scaling Factors for Output Report File ",
               "{.emph (enable_scaling_factors)}: ",
               "{.val {self$enable_scaling_factors}}"))
      cli::cli_ul(id = "scaling_factors_fields")
      cli::cli_li("biomass_scale: {.val {self$biomass_scale}}")
      cli::cli_li("recruitment_scale: {.val {self$recruitment_scale}}")
      cli::cli_li(paste0("stock_size_scale: ",
                         "{.val {self$stock_size_scale}}"))

      cli::cli_end()

    },

    #' @description
    #' Reads in the values from the keyword parameter SCALE from the
    #' AGEPRO Input file
    #'
    #' Note: enable_scaling_factors must be set to TRUE.
    #'
    #' @template inp_con
    #' @template nline
    #'
    read_inp_lines = function(inp_con, nline) {

      if(isFALSE(self$enable_scaling_factors)){
        stop(private$unenabled_options_flag_message())
      }

      cli::cli_alert_info("Reading {.strong {private$.keyword_name}}")

      nline <- nline + 1
      inp_line <- read_inp_numeric_line(inp_con)

      self$biomass_scale <- inp_line[1]
      self$recruitment_scale <- inp_line[2]
      self$stock_size_scale <- inp_line[3]


      cli::cli_alert(paste0("Line {nline}: ",
                            "Scaling Factor values"))
      li_nested <- cli::cli_div(id = "scale_inp_fields",
                                theme = list(ul = list("margin-left" = 2)))

      cli::cli_li(paste0("biomass_scale: ",
                         "{.val {self$biomass_scale}}"))
      cli::cli_li(paste0("recruitment_scale: ",
                         "{.val {self$recruitment_scale}}"))
      cli::cli_li(paste0("stock_size_scale: ",
                         "{.val {self$stock_size_scale}}"))
      cli::cli_end("scale_inp_fields")

      return(nline)

    }



  ),
  active = list(

    #' @field biomass_scale
    #' Output units of biomass expressed in thousand metric tons
    biomass_scale = function(value){
      if(missing(value)){
        if(is.null(private$.biomass_scale)){
          warning("biomass_scale is NULL", call. = FALSE)
        }
        return(private$.biomass_scale)
      }else{
        if(isFALSE(self$enable_scaling_factors)) {
          stop(private$unenabled_options_flag_message(), call. = FALSE)
        }

        checkmate::assert_numeric(value, lower = 0, len = 1)

        private$.biomass_scale <- value
      }
    },

    #' @field recruitment_scale
    #' Output units of recruitment numbers
    recruitment_scale = function(value){
      if(missing(value)){
        if(is.null(private$.recruitment_scale)){
          warning("recruitment_scale is NULL", call. = FALSE)
        }
        return(private$.recruitment_scale)
      }else{
        if(isFALSE(self$enable_scaling_factors)) {
          stop(private$unenabled_options_flag_message(), call. = FALSE)
        }

        checkmate::assert_numeric(value, lower = 0, len = 1)

        private$.recruitment_scale <- value
      }
    },

    #' @field stock_size_scale
    #' Output Units of stock size numbers
    stock_size_scale = function(value){
      if(missing(value)){
        if(is.null(private$.stock_size_scale)){
          warning("stock_size_scale is NULL", call. = FALSE)
        }
        return(private$.stock_size_scale)
      }else{
        if(isFALSE(self$enable_scaling_factors)) {
          stop(private$unenabled_options_flag_message(), call. = FALSE)
        }

        checkmate::assert_numeric(value, lower = 0, len = 1)

        private$.stock_size_scale <- value
      }
    },

    #' @field enable_scaling_factors
    #' Logical field that flags if fields can be edited. This class will not
    #' accept new values to its fields or allow it to be exported to input file
    #' until this option flag is TRUE.
    enable_scaling_factors = function(value) {
      if(isTRUE(missing(value))){
        return(self$flag$op$enable_scaling_factors)
      } else {
        private$set_enable_scaling_factors(value)
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

    .keyword_name = "scale",

    .biomass_scale = NULL,
    .recruitment_scale = NULL,
    .stock_size_scale = NULL,

    # Wrapper Function to toggle enable_reference_points options_flag.
    set_enable_scaling_factors = function(x) {

      checkmate::assert_logical(x, null.ok = TRUE)

      #Set value to options flags field reference "flag"
      self$flag$op$enable_scaling_factors <- x

      cli::cli_alert(
        paste0("enable_scaling_factors : ",
               "{.val ",
               "{self$flag$op$enable_scaling_factors}}"))

    },

    # Error message when setting values to reference_points while
    # enable_reference_points is FALSE
    unenabled_options_flag_message = function() {
      return(invisible(
        paste0("enable_scaling_factors is FALSE. ",
               "Set flag to TRUE to set value.")
      ))
    },

    #Reset option_flag to NULL at initialization
    reset_options_flags = function() {

      if(isFALSE(is.null(self$flag$op$enable_scale_options))){
        cli::cli_alert("Reset enable_scaling_factors to NULL for initalization")
        self$flag$op$enable_scale_options <- NULL
      }
    }


  )
)
