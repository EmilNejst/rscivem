check_for_null_regime <- function(regprobs) {

  reg_mat <- do.call(cbind, regprobs)
  sum_prob <- colSums(reg_mat)

  if(any(sum_prob <= 1e-3)) {
    return(TRUE)
  }

  FALSE
}
