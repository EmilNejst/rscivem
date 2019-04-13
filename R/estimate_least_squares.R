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
  Z1 <- Z$Z1
  Z2 <- Z$Z2

  iOmega <- lapply(Omega, solve)
  U <- ifelse(is.null(Z2), Z1 %*% beta, cbind(Z1 %*% beta, Z2))
  nU <- ncol(U)
  Mm <- purrr::pmap(
    .l = list(.o = iOmega, .p = regprobs),
    .f = function(.o, .p) {
      Ust <- U * (.p %*% diag(nU))
      kronecker(.o, crossprod(U, Ust)) })
  M <- Matrix::bdiag(Mm)

  if(!is.null(Z2)) {
    Y <- purrr::pmap(
      .l = list(.o = iOmega, .p = regprobs),
      .f = function(.o, .p) {
        Ust <- U * (.p %*% diag(nU))
        as.vector(crossprod(Z2, Ust) %*% .o)  })
    Y <- do.call(rbind, Y)
  }

  if(!is.null(Z2)) {
    free_pars <- solve(crossprod(H, M) %*% H, Y %*% H - crossprod(h, M) %*% H)
  }else {
    free_pars <- solve(crossprod(H, M) %*% H, crossprod(h, M) %*% H)
  }

  vecPhi <- as.vector(H %*% free_pars + h)

}
