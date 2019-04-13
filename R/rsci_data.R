#' @title Build the classic cointegration data structures Z0, Z1 and Z2.
#'
#' @param data xts, a matrix with the data that should be fitted.
#'
#' @export
#' @return a list with a list of S matrices and a list of Z matrices.

rsci_data <- function(rank, lags, data, data_exo = NULL) {
  Z <- list(Z0 = NULL, Z1 = NULL, Z2 = NULL)

  p <- ncol(data)
  q <- lags
  r <- rank
  n <- nrow(data)
  m <- n - q
  dates <- zoo::index(data)

  Z$Z1 <- xts::lag.xts(data, 1)[-(1:q),]
  Z$Z0 <- data[-(1:q),] - Z$Z1
  if(q > 1) {
    Z$Z2 <- xts::lag.xts(data - xts::lag.xts(data, 1), seq_len(q-1))[-(1:q),]
  }
  rsci_data <- list(Z = Z, data_exo = data_exo)
  class(rsci_data) <- 'rsci_data'
  rsci_data
}
