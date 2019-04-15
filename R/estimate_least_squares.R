#' @title Estimate least squares parameters alpha and Gamma
#'
#' @param Z list, the standard matrices Z0, Z1 and Z2
#' @param beta matrix, the matrix of cointegration relations
#' @param Omega list, a list of matrices corresponding to the error covariance
#'              matrices by regime
#' @param regprobs list, a list with vectors of regime probabilities
#' @param H matrix, a matrix with the linear restrictions
#' @param h vector, the normalizing vector for linear restrictions
#'
#' @return the vector Phi with estimated least squares parameters.

estimate_least_squares <- function(Z, beta, Omega, regprobs, H, h) {

  nregs <- length(regprobs)
  iOmega <- lapply(Omega, solve)
  U <- get_U(Z, beta)
  nU <- ncol(U)
  iota <- matrix(1, 1, nU)

  Mm <- purrr::pmap(
    .l = list(.o = iOmega, .p = regprobs),
    .f = function(.o, .p) {
      Ust <- U * (.p %*% iota)
      kronecker(.o, crossprod(U, Ust)) })
  M <- as.matrix(Matrix::bdiag(Mm))

  Y <- purrr::pmap(
    .l = list(.o = iOmega, .p = regprobs),
    .f = function(.o, .p) {
      Ust <- U * (.p %*% iota)
      as.vector(crossprod(Ust, Z$Z0) %*% .o)  })
  Y <- do.call(c, Y)

  free_pars <- solve(crossprod(H, M) %*% H, crossprod(H, Y - M %*% h))
  vecPhi <- as.vector(H %*% free_pars + h)

}
