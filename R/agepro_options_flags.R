

#' @title
#' Class container that encapsulates AGEPRO user-defined options
#'
#' @description
#' Class Structure that encapsulates AGEPRO's user-defined options including
#' output options, percentile summary, scaling factors, biological reference
#' points, maximum bounds, and retrospective adjustment.
#'
#' @template inp_con
#' @template nline
#'
#' @export
#'
agepro_options_flags <- R6Class(
  "agepro_options_flags",
  public = list(

    #' @description
    #' Initialize the class
    #'
    #' @param enable_percentile
    #' [Logical][base::logical] flag to enable percentile summary of the key
    #' results in the output file.
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
    set_enable_user_percentile_summary = function(value) {
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
