#' @title Cacluate regime specific residuals
#'
#' @param Phi list, a list of least squares paramters for each regime
#' @param beta matrix, a matrix with the cointegration relations
#' @param Z list, a list of data matrices constructed with the build function.
#'
#' @return a list of matrices with regime residuals

calculate_regime_residuals <- function(Phi, beta, Z) {

  n_regs <- length(Phi)

  if(!is.null(Z$Z2)) {
    U <- cbind(Z$Z1 %*% beta, Z$Z2)
  }else {
    U <- Z$Z1 %*% beta
  }
  E <- lapply(
    X = Phi,
    FUN = function(x) { tcrossprod(U, x) })

}
