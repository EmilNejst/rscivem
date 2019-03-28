#' @title Internal calculation of the loglikelihood.
#'
#' @description a logliklihood function that works internally and avoids
#'              rebuilding data structions at each evaluation.
#'
#' @param pars an rsci_pars object
#' @param Z list, a list holding the data structures Z0, Z1, Z2
#' @param regprobs vector, the vedtor of regime probabilities
#'
#' @return a likelihood number

int_loglik <- function(pars, Z, regprobs) {

}
