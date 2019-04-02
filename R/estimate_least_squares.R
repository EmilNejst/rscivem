#' @title Estimate least squares parameters alpha and Gamma
#'
#' @param Z list, the standard matrices Z0, Z1 and Z2
#' @param beta matrix, the matrix of cointegration relations
#' @param omega list, a list of matrices corresponding to the error covariance
#'              matrices by regime
#' @param regprobs vector, regime probabilities
#' @param H matrix, a matrix with the linear restrictions
#' @param h vector, the normalizing vector for linear restrictions
#'
#' @return the vector Phi with estimated least squares parameters.

estimate_least_squares <- function(Z, beta, omega, regprobs, H, h) {

  nregs <- length(regprobs)
  Z1 <- Z$Z1
  Z2 <- Z$Z2

  iomega <- lapply(omega, solve)
  U <- ifelse(is.null(Z2), Z1 %*% beta, cbind(Z1 %*% beta, Z2))
  Mm <- purrr::pmap(
    .l = list(.o = iomega, .p = regprobs),
    .f = function(.o, .p) { kronecker(.o, t(U) %*% (U*.p)) })
  M <- Matrix::bdiag(Mm)

  if(!is.null(Z2)) {
    Y <- purrr::pmap(
      .l = list(.o = iomega, .p = regprobs),
      .f = function(.o, .p) { as.vector((t(Z2) %*% U*.p) %*% .o)  })
    Y <- do.call(rbind, Y)
  }

  if(!is.null(Z2)) {
    free_pars <- solve(t(H) %*% M %*% H, Y %*% H - t(h) %*% M %*% H)
  }else {
    free_pars <- solve(t(H) %*% M %*% H, t(h) %*% M %*% H)
  }

  Phi <- as.vector(H*free_pars + h)

}
