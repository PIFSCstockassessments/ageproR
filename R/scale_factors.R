

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
scale_factors <-R6Class(
  "scale_factors",
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

      #Check for defaults
      if(all(c(all.equal(scale_bio, 0),
               all.equal(scale_recruit, 0),
               all.equal(scale_stock_size, 0)))) {
        cli::cli_alert(paste0("Default values set, ",
                              "options_flag enable_scale_factors to FALSE"))
        suppressMessages(private$set_enable_scale_factors(FALSE))
      } else{
        cli::cli_alert(paste0("Values for reference_points set. ",
                              "Enable options_flag enable_scale_factors ",
                              "as TRUE"))
        private$set_enable_scale_factors(TRUE)
        #self$print()
      }
    }



  ),
  active = list(

    #' @field biomass_scale
    #' Output units of biomass expressed in thousand metric tons
    biomass_scale = function(value){
      if(missing(value)){
        return(private$.biomass_scale)
      }
    },

    #' @field recruitment_scale
    #' Output units of recruitment numbers
    recruitment_scale = function(value){
      if(missing(value)){
        return(private$.recruitment_scale)
      }
    },

    #' @field stock_size_scale
    #' Output Units of stock size numbers
    stock_size_scale = function(value){
      if(missing(value)){
        return(private$.stock_size_scale)
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
    set_enable_scale_factors = function(x) {

      checkmate::assert_logical(x, null.ok = TRUE)

      #Set value to options flags field reference "flag"
      self$flag$op$enable_scale_factors <- x

      cli::cli_alert(
        paste0("enable_scale_factors : ",
               "{.val ",
               "{self$flag$op$enable_scale_factors}}"))


    },

    # Error message when setting values to reference_points while
    # enable_reference_points is FALSE
    unenabled_options_flag_message = function() {
      return(invisible(
        paste0("enable_reference_points is FALSE. ",
               "Set flag to TRUE to set value.")
      ))
    }


  )
)
