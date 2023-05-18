
#' @title Input information for bootstrap numbers at age file
#'
#' @description
#' -The Number of data values of each row must equal to the number of age classes.
#' -The number of rows in a bootstrap file must be at least equal to the number of bootstrap
#' iterations containing the popluation of the first year in the projection
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate assert_numeric
bootstrap <- R6Class(
  "bootstrap",
  private = list (

    .num_bootstraps = NULL,
    .pop_scale_factor = NULL,
    .boostrap_file = NULL

  ), public = list (

    #' @description
    #' Initializes the Bootstrap Class
    #'
    initialize = function(){

      self$num_bootstraps <- 0
      self$pop_scale_factor <- 0

    }



  ), active = list (

    #' @field num_bootstraps
    #' Number of bootstraps
    num_bootstraps = function(value) {
      if(missing(value)){
        private$.num_bootstraps
      }else{
        assert_numeric(value, lower = 0)
        private$.num_bootstraps <- value
      }
    },

    #' @field pop_scale_factor
    #' Population Scale Factor
    pop_scale_factor = function(value) {
      if(missing(value)){
        private$.pop_scale_factor
      }else{
        assert_numeric(value, lower = 0)
        private$.pop_scale_factor <- value
      }
    }


  )

)
