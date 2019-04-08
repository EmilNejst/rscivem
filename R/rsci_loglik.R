#' @title Calculate the logliklihood value some data given a model.
#'
#' @param model rsci_model, a model object with the model in question
#' @param data xts, the data to be fitted
#' @param data_regprob xts, a matrix with the data used in the fn_prob
#'        function. If NULL, then data is used as input for calulating regime
#'        probabilities through fn_probs and the fn_probs should reflext
#'        that.
#'
#' @export
#' @return the log-likelihood value

rsci_loglik <- function(model, data_struct, data_regprob = NULL) {

  residuals <- calculate_regime_residuals(
    Phi = model$pars$Phi,
    beta = model$pars$beta,
    Z = data_struct$Z)
  regprob <- model$fn_prob(model$pars, data, data_regprob)

  ll <- sum(
    purrr::pmap2_dbl(
      .l = list(.e = residuals, .o = model$pars$Omega, .p = regprob),
      .f = function(.e, .o, .p) {
        oe <- .x %*% solve(.o)
        nd <- rowSums(.p * (-.5*log(determinant(.o)) - 0.5*colSums(e * oe))) }))

}
