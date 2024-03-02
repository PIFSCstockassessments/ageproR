

#' Recruitment Model Number Parameter validation
#'
#' @description
#' Custom validation to check
#' \href{../../ageproR/html/agepro_model.html#method-set_recruit_model}{\code{agepro_model$set_recruit_model()}}
#' arguments to see if multiple recruit numbers is passed as a single vector
#' or seen as a list of multiple arguments.
#'
#' If the input value is passed as a list of multiple arguments,
#' this function will "throw" a message of the issue and possible resolution.
#'
#' [ageproR::assert_model_num_vector_args] wraps
#' [ageproR::check_model_num_vector_args] as a custom checkmate assertion via [checkmate::makeAssertion]
#'
#' @param x object to check
#'
check_model_num_vector_args <- function(x) {

  # Catch "Empty" argument
  if(isTRUE(all.equal(length(x), 0))){
    return(paste0("No recruitment model numbers passed"))
  }

  # Catch Multiple parameters and return validation message
  if(!isTRUE(all.equal(length(x),1))){
    return(paste0("Multiple parameters detected, ",
                  "please pass multiple recruitment models as a single vector"))
  }

  return(TRUE)

}


#' @rdname check_model_num_vector_args
#'
#' @template assert
assert_model_num_vector_args <- function(x,
                                      .var.name = checkmate::vname(x),
                                      add = NULL) {

  res = check_model_num_vector_args(x)
  checkmate::makeAssertion(x, res, .var.name, add)

}
