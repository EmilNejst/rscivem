#' @title Estimate the covariance matrices given filtered probabilities and
#'        other model parameters.
#'
#' @param Phi list, a list with the regime specific least squares matrices
#' @param beta matrix, a matrix with the cointegration vectors
#' @param Z list, a list with the data matrices
#' @param regprobs list, a list with vectors regime probabilities
#' @param H matrix, the restriction matrix
#'
#' @return the vectorized version of Omega

estimate_error_covariances <- function(Phi, beta, Z, regprobs, H, h) {

  res <- calculate_regime_residuals(Phi, beta, Z)
  p <- ncol(Z$Z0)
  n <- length(Phi)
  nobs <- nrow(Z$Z0)
  iota <- matrix(1, 1, p)
  p_res <- purrr::map2(
    .x = res,
    .y = regprobs,
    .f = ~ .x * (.y %*% iota) )
  E <- do.call(
    what = cbind,
    args = purrr::map2(
      .x = p_res,
      .y = res,
      .f = ~ crossprod(.x, .y) ))
  vecE <- as.vector(E)

  P <- 0
  I <- diag(p^2)
  for(i in 1:nobs) {
    p <- regprobs[[1]][i] * I
    for(j in 2:n) {
      p <- Matrix::bdiag(p, regprobs[[j]][i] * I)
    }
    P <- P + p
  }

  P <- as.matrix(P)
  o <- solve(crossprod(H, P) %*% H, crossprod(H, vecE - P %*% h))
  vecOmega <- as.vector(H %*% o + h)

}
