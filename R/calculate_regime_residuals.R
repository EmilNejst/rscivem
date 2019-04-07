#' @title Cacluate regime specific residuals
#'
#' @param Phi list, a list of least squares paramters for each regime
#' @param beta matrix, a matrix with the cointegration relations
#' @param Z list, a list of data matrices constructed with the build function.
#'
#' @return a list of matrices with regime residuals

calculate_regime_residuals <- function(Phi, beta, Z) {

  n_regs <- length(Phi)

  U <- cbind(Z$Z1 %*% beta, Z$Z2)
  E <- lapply(
    X = Phi,
    FUN = function(x) { U %*% x })

}
