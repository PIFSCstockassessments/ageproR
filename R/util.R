
#' power1
#'
#' Custom power function
#'
#' @param exp exp
#'
#' @export
power1 <- function(exp) {
  #force(exp)
  function(x){
    x^exp
  }
}


#' @inherit power1 title description
#' @param x x
#' @export
isquare <- power1(2)

#' @inherit power1 title description
#' @param x x
#'@export
icube <- power1(3)
