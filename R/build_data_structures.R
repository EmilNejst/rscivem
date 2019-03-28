#' @title Build the classic cointegration data structures, S and Z
#' 
#' @param spec rsci_spec, an rsci specification object.
#' @param data xts, a matrix with the data that should be fitted.
#' 
#' @return a list with a list of S matrices and a list of Z matrices.
build_data_structures <- function(spec, data) {
  Z <- list(Z0 = NULL, Z1 = NULL, Z2 = NULL)
  
  p <- ncol(data)
  q <- spec$lags
  r <- spec$rank
  n <- nrow(data)
  m <- n - q
  dates <- zoo::index(data)
  
  Z$Z1 <- xts::lag.xts(data, 1)[-(1:q),]
  Z$Z0 <- data[-(1:q),] - Z$Z1
  if(q > 1) {
    Z$Z2 <- xts::lag.xts(data - xts::lag.xts(data, 1), seq_len(q-1))[-(1:q),]
  }
  data_structures <- list(Z = Z)
}
