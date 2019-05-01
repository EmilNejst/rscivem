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
#'
#' @param ... additional information to be passed to optim().
#'
#' @export
#' @return an rsci_fit object

rsci_fit <- function(model,
                     data,
                     init_pars,
                     fn_update,
                     ...) {

  # Model dimensions -----------------------------------------------------------
  dim <- model$dim
  lags <- model$lags
  rank <- model$rank
  nreg <- model$nreg

  # Data structures ------------------------------------------------------------
  ds <- rsci_data(rank, lags, data)

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
      cat('Non-linear parameters are:\n')
      print(pars)
      str<- paste0('A regime has zero probability. Try to reformulate the model')
      stop(str)
    }

    # Update the model with the parameters ----
    mod <- estimate_closed_form_pars(pars, mod, ds, rp)

    # Calculate the negative likelihod ----
    - rsci_loglik(mod, rp, ds)
  }

  # Optimize the concentrated likelihood ---------------------------------------
  opt_res <- optim(init_pars, opt_fn, ...)

  # Build and return the fit object --------------------------------------------
  model$pars <- opt_res$par
  model <- fn_update(opt_res$par, model)
  rp <- model$fn_prob(opt_res$par, ds)
  model_fit <- estimate_closed_form_pars(opt_res$par, model, ds, rp)
  rp <- lapply(rp, function(x) { xts::xts(x, index(ds$Z$Z0)) })
  fit <- list(
    model = model_fit,
    loglik = opt_res$value,
    pars = opt_res$pars,
    regprobs = rp,
    data = data)

  class(fit) <- 'rsci_fit'
  fit
}
