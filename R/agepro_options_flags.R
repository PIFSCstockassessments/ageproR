

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
    intialize = function(enable_percentile = FALSE) {

      self$enable_percentile_summary <- enable_percentile
    }


  ),
  active = list(

    #' @field enable_percentile_summary
    #' [Logical][base::logical] flag to allow percentile summary of the key
    #' results in the output file.
    #'
    enable_percentile_summary = function(value){
      if(missing(value)){
        return(private$.enable_percentile_summary)
      }else{
        checkmate::assert_logical(value)

        private$.enable_percentile_summary <- value

        cli::cli_alert(paste0("enable_percentile_summary: ",
                              "{.val {private$.enable_percentile_summary}}"))
      }
    }

  ),
  private = list(

    .enable_percentile_summary = NULL,
    .enable_reference_points = NULL,
    .enable_scaling_factors = NULL,
    .enable_max_bounds = NULL,
    .enable_retrospective_adjustment = NULL

  )
)
