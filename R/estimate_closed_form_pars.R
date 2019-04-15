#' @title Estimate model parameters
#'
#' @param pars vector, a vector of non linear parameters
#' @param model rsci_model object.
#' @param ds rsci_data object.
#' @param regprobs list, a list with regime probabilities.
#'
#' @return model

estimate_closed_form_pars <- function(pars, model, ds, regprobs) {

  # Model dimensions ----
  dim <- model$dim
  lags <- model$lags
  rank <- model$rank
  nreg <- model$nreg

  # Estimate Phi for given probabilities and beta ----
  vecPhi <- estimate_least_squares(
    Z = ds$Z,
    beta = model$beta,
    Omega = model$Omega,
    regprobs = regprobs,
    H = model$linres_Phi$H,
    h = model$linres_Phi$h)
  Phi <- vec_Phi_2_List_Phi(vecPhi, dim, rank, lags, nreg)
  model$Phi <- Phi

  # Estimate Omega for given probabilities and beta ----
  vecOmega <- estimate_error_covariances(
    Phi = model$Phi,
    beta = model$beta,
    Z = ds$Z,
    regprobs = regprobs,
    H = model$linres_Omega$H,
    h = model$linres_Omega$h)
  Omega <- vec_Omega_2_list_Omega(vecOmega, dim, nreg)
  model$Omega <- Omega
  model

}
