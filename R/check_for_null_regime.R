#' @title Check for the number
#'
#' @param regprobs list, a list of the xts regime probabilities
#'
#' @return Boolean TRUE if a regime is deamed neglected

check_for_null_regime <- function(regprobs) {

  reg_mat <- do.call(cbind, regprobs)
  sum_prob <- colSums(reg_mat)

  if(any(sum_prob <= 1e-3)) {
    return(TRUE)
  }

  FALSE
}
