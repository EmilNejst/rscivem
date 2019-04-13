#' @title Calculate the logliklihood value some data given a model.
#'
#' @param model rsci_model, a model object with the model in question
#' @param regprobs list, a list with regimes probabilities
#' @param data rsci_data, the data to be fitted
#'
#' @export
#' @return the log-likelihood value

rsci_loglik <- function(model, regprobs, data_struct) {

  residuals <- calculate_regime_residuals(
    Phi = model$Phi,
    beta = model$beta,
    Z = data_struct$Z)

  ll <- sum(
    purrr::pmap_dbl(
      .l = list(.e = residuals, .o = model$Omega, .p = regprobs),
      .f = function(.e, .o, .p) {
        oe <- .e %*% solve(.o)
        nd <- rowSums(.p * (-.5*log(determinant(.o)) - 0.5*colSums(e * oe))) }))

}
