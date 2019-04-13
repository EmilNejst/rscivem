#' @title Fit a regime switching cointegration vector error correction model to
#'        data.
#'
#' @description This function takes data
#'
#' @param model rsci_model object, the object containing the initial model.
#' @param init_pars vector or number, initial parameters for the freely varying
#'        elements in beta and the parameters in the probability function.
#' @param fn_update function, a function that takes the values from a vector
#'        pars and allocates the parameters correctly to the model object.
#'        Returns a model object. The function must take the following arguments
#'        fn_update(pars, model). These are the  free parameters in beta and
#'        the probability functions.
#' @param data_exo xts, exogenous data in case the regime switching depends on
#'        such.
#'
#' @param ... additional information to be passed to optim().
#'
#' @export
#' @return an rsci_fit object

rsci_fit <- function(model,
                     data,
                     init_pars,
                     fn_update,
                     data_exo = NULL,
                     ...) {

  # Model dimensions -----------------------------------------------------------
  dim <- model$dim
  lags <- model$lags
  rank <- model$rank
  nreg <- model$nreg

  # Data structures ------------------------------------------------------------
  ds <- rsci_data(rank, lags, data, data_exo)

  # Estimate Initial Values for Omega and Phi if not provided ------------------
  if(is.null(model$Omega)) {
    rp <- model$fn_prob(init_pars, ds)
    Omega <- initial_Omega(nreg, ds$Z, model$beta, ds$Z, model$Phi, model)
  }

  # Function to optimize ----
  opt_fn <- function(pars) {
    # Update model with new parameters ----
    model <- fn_update(pars, model)

    # Update regime probabilities ----
    rp <- model$fn_prob(pars, ds)

    # Estimate Phi for given probabilities and beta ----
    vecPhi <- estimate_least_squares(
      Z = data_struct$Z,
      beta = model$beta,
      Omega = model$Omega,
      regprobs = rp)
    Phi <- vec_Phi_2_List_Phi(vecPhi, dim, rank, lags, nreg)
    model$Phi <- Phi

    # Estimate Omega for given probabilities and beta ----
    vecOmega <- estimate_error_covariance(
      Phi, beta, data_struct$Z, rp, H, h)
    Omega <- vec_Omega_2_list_Omega(vecOmega, dim, nreg)
    model$Omega <- Omega

    # Calculate the negative likelihod ----
    - rsci_loglik(model, data_struct)
  }

  # Optimize the concentrated likelihood ---------------------------------------
  opt_res <- optim(init_pars, opt_fn, ...)

  # Build and return the fit object --------------------------------------------
  model_fit <- fn_update(opt_res$par, model)
  fit <- list(
    model = model_fit,
    loglik = opt_res$value,
    data = data,
    data_exo = data,
    fn_prob = fn_prob)

  class(fit) <- 'rsci_fit'
  rsci_fit
}
