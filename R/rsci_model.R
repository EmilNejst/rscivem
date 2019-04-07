#' @title Create a regime switching cointegrated vector error correction
#'
#' @param lags integer, the number of lags in levels in the model.
#' @param rank integer, the cointegration rank.
#' @param nreg integer, the number of regimes.
#' @param pars rsci_pars, an rsci_pars object with the model parameters
#'
#' @export
#' @return an rsci_model object holding the model
rsci_model <- function(dim, rank, lags, pars) {

  model <- list(
    dim = dim,
    rank = rank,
    lags = lags,
    pars = pars,
    data_exo = data_exo)

  class(model) <- "rsci_model"

  model
}
