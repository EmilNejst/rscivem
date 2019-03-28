#' @title Create a regime switching cointegrated vector error correction
#'
#' @param spec rsci_spec, an rsci_spec object with the model specification
#' @param pars rsci_pars, an rsci_pars object with the model parameters
#' @param fn_regprob function, a function that takes two arguments
#'        \code{ fn_regprob(pars, data) } where pars is a rsci_pars
#'        object and data is an xts object with the time series data used in
#'        rsci_fit. The function must return a simple vector of regime
#'        probabilities that has length corresponding to the number of columns
#'        in the regimes tibble in the rsci_spec object.
#'
#' @export
#' @return an rsci_model object holding the model
rsci_model <- function(spec, pars, fn_regprob) {

  model <- list(
    spec = spec,
    pars = pars,
    fn_regprob = fn_regprob)

}