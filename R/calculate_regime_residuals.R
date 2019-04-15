#' @title Cacluate regime specific residuals
#'
#' @param Phi list, a list of least squares paramters for each regime
#' @param beta matrix, a matrix with the cointegration relations
#' @param Z list, a list of data matrices constructed with the build function.
#'
#' @return a list of matrices with regime residuals

calculate_regime_residuals <- function(Phi, beta, Z) {

  n_regs <- length(Phi)
  U <- get_U(Z, beta)
  E <- lapply(
    X = Phi,
    FUN = function(x) {Z$Z0 - tcrossprod(U, x) })

}
