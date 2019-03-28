#' @title Create a regime switching cointegrated vector error correction model
#'        specification object.
#'
#' @param lags integer, the number of lags in levels in the model.
#' @param rank integer, the cointegration rank.
#' @param regimes tibble, a table with regime names and groups.
#'
#' @export
#' @return an rsci_spec object holding the model specification without parameter
#'         values.
rsci_spec <- function(lags, rank, regimes) {

  spec <- list(
    lags = lags,
    rank = rank,
    regimes = regimes)

  class(spec) <- 'rsci_spec'

  spec
}
