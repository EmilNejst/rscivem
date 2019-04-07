#' @title Fit a regime switching cointegration vector error correction model to
#'        data.
#'
#' @description This function takes data
#'
#' @param model rsci_model object, the object containing the initial model.
#' @param init_pars vector or number, initial parameters for the freely varying
#'        elements in beta and the parameters in the probability function.
#' @param fn_update function, a function that takes the values from init_pars
#'        and allocates the parameters correctly to the model object. Returns a
#'        model object. The function must take the following arguments
#'        fn_update(vec_pars, model). These are the  free parameters in beta and
#'        the probability functions.
#' @param fn_prob function, a function that takes at least two arguments
#'        \code{ fn_prob(pars, data, data_exo = NULL) } where pars is a
#'        rsci_pars object and data is an xts object with the time series data
#'        used in rsci_fit. The function must return a list of simple vectors of
#'        regime of probabilities that has length corresponding to the number of
#'        observations in the data. The regime probabilities at time t must
#'        naturally sum to one.
#' @param data_exo xts, exogenous data that enters the regime probabilities.
#'        default is NULL as the standard model using endogenous data for the
#'        regime probabilities.
#' @param ... additional information to be passed to optim().
#'
#' @export
#' @return an rsci_fit object

rsci_fit <- function(model,
                     data,
                     init_pars,
                     fn_update,
                     fn_prob,
                     data_exo,
                     ...) {

  # Model dimensions -----------------------------------------------------------
  dim <- model$dim
  lags <- model$lags
  rank <- model$rank
  nreg <- model$nreg

  # Function to optimize -------------------------------------------------------
  opt_fn <- function(pars) {

    model <- fn_update(pars, model)
    rp <- fn_prob(model$pars, data, data_exo)
    vecPhi <- estimate_least_squares(
      Z = data_struct$Z,
      beta = model$beta,
      Omega = model$Omega,
      regprobs = rp)
    Phi <- vec_Phi_2_List_Phi(vecPhi, dim, rank, lags, nreg)
    model$Phi <- Phi
    vecOmega <- estimate_error_covariance(
      Phi, beta, data_struct$Z, rp, H, h)
    Omega <- vec_Omega_2_list_Omega(vecOmega, dim, nreg)
    model$Omega <- Omega

    - rsci_loglik(model, data)
  }

  # Optimized the concentrated likelihood --------------------------------------
  opt_res <- optim(init_pars, opt_fn, ...)

  # Build and return the fit object --------------------------------------------
  model_fit <- update(opt_res$par, model)
  fit <- list(
    model = model_fit,
    loglik = opt_res$value,
    data = data,
    data_exo = data,
    fn_prob = fn_prob
  )

}
