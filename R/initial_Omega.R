#' @title Estimate initial parameters for Phi and Omega.
#'
#'
#' @param nreg integer, the number of regimes
#' @param Z list, the standard data structures Z0, Z1 and Z2
#' @param beta vector, a vector of probability paramters
#' @param regprobs list, a list with an xts time series of regime probabilities
#'        for each regime.
#' @param H matrix, the selection matric for linear restrictions
#' @param h vector, the normalizing vector for linear restrictions
#' @param Phi list, a list with initial values for Phi.
#'
#' @return a list with initial regime Omega estimates.

initial_Omega <- function(nreg,
                          Z,
                          beta,
                          regprobs,
                          H,
                          h,
                          Phi = NULL) {

  if(is.null(Phi)) {
    Omega <- lapply(seq_len(nreg), function(x) { cov(Z$Z0) })
  }else {
    vecOmega <- estimate_error_covariances(Phi, beta, Z, regprobs, H, h)
    Omega <- vec_Omega_2_list_Omega(vecOmega, nrow(Phi), nreg)
  }

  Omega

}
