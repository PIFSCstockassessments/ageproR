

#' @title
#' Class container that encapsulates logical flags to enable AGEPRO
#' user-defined options
#'
#' @description
#' Encapsulates logical flags indicating that an optional AGEPRO's option can
#' be used: these options are percentile summary (PERC), scaling factors
#' (SCALE), biological reference points (REFPOINT), maximum bounds (BOUNDS), and
#' retrospective adjustment (RETROADJUST).
#'
#' @details
#' Associated with AGEPRO's output options (OPTIONS) are additional optional
#' options:
#'
#' \describe{
#'   \item{PERC}{user_percentile_summary}
#'   \item{REFPOINT}{reference_points}
#'   \item{SCALE}{scaling_factors}
#'   \item{BOUNDS}{max_bounds}
#'   \item{RETROADJUST}{retrospective_adjustment}
#' }
#'
#' The AGEPRO input file format recognizes these optional keyword parameters.
#' At initialization, all option flags will be set to FALSE. To "enable" an
#' optional option or set it to TRUE, assign a value to the optional option's
#' field. For example, if the flag to enable for percentile summary
#' (`enable_user_percentile_summary`) is FALSE: set value `report_percentile`,
#' Then it will be TRUE
#'
#' @template inp_con
#' @template nline
#'
#' @export
#'
#'
agepro_options_flags <- R6Class(
  "agepro_options_flags",
  public = list(

    #' @description
    #' Initialize the class
    #'
    #' @param enable_percentile
    #' [Logical][base::logical] flag to enable percentile summary of the key
    #' results in the output file. FALSE by default.
    #'
    initialize = function(enable_percentile = FALSE) {

      private$.enable_user_percentile_summary <- enable_percentile
    },

    #' @description
    #' Toggles the logical flag to enable user_percentile summary
    #'
    #' @param value
    #' Logical value
    #'
    set_flag_user_percentile_summary = function(value) {
      checkmate::assert_logical(value)

      private$.enable_user_percentile_summary <- value

      cli::cli_alert(paste0("enable_user_percentile_summary: ",
                            "{.val {private$.enable_user_percentile_summary}}"))

    }


  ),
  active = list(

    #' @field enable_user_percentile_summary
    #' [Logical][base::logical] flag to allow percentile summary of the key
    #' results in the output file.
    #'
    enable_user_percentile_summary = function(value){
      if(isFALSE(missing(value))){
        stop("active binding is read only", call. = FALSE)
      }

      private$.enable_user_percentile_summary

    }

  ),
  private = list(

    .enable_user_percentile_summary = NULL,
    .enable_reference_points = NULL,
    .enable_scaling_factors = NULL,
    .enable_max_bounds = NULL,
    .enable_retrospective_adjustment = NULL


  )
)
