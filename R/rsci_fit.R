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
    model$Omega <- initial_Omega(
      nreg,
      ds$Z,
      model$beta,
      ds$Z,
      model$linres_Omega$H,
      model$linres_Omega$h,
      model$Phi)

  }

  # Function to optimize ----
  opt_fn <- function(pars) {
    # Update the model with the given parameters
    mod <- fn_update(pars, model)

    # Update regime probabilities ---
    rp <- model$fn_prob(pars, ds)

    # Check for null regime ----
    nul_reg <- check_for_null_regime(rp)

    if(nul_reg) {
      cat('Parameters are: \n')
      print(pars)
      stop("One regime has zero probability. Try to reformulate the model.")
    }

    # Update the model with the parameters ----
    mod <- estimate_closed_form_pars(pars, mod, ds, rp)

    # Calculate the negative likelihod ----
    - rsci_loglik(mod, rp, ds)
  }

  # Optimize the concentrated likelihood ---------------------------------------
  opt_res <- optim(init_pars, opt_fn, ...)

  # Build and return the fit object --------------------------------------------
  model <- fn_update(opt_res$par, model)
  rp <- model$fn_prob(opt_res$par, ds)
  model_fit <- estimate_closed_form_pars(opt_res$par, model, ds, rp)
  fit <- list(
    model = model_fit,
    loglik = opt_res$value,
    data = data,
    data_exo = data_exo)

  class(fit) <- 'rsci_fit'
  fit
}
