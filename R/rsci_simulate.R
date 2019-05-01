#' @title Simulate a data series from a regime switching error correction model
#'
#' @param model rsci_model obj
#' @param sample_size integer, the size of the sample.
#' @param burn integer, a number of periods to burn from the begining of the
#'        sample
#'
#' @export
#' @return a matrix with simulated values for the model

rsci_simulate <- function(model, sample_size, burn = 0) {

  q <- model$lags
  p <- model$dim
  r <- model$nreg
  n <- sample_size + burn
  X0 <- matrix(0, q, p)
  X <- matrix(NA, n, p)
  X <- rbind(X0, X)

  pars <- model$pars
  beta <- model$beta
  Phi <- model$Phi
  Omega <- model$Omega
  sqOmega <- lapply(Omega, expm::sqrtm)
  fnp <- model$fn_prob

  E <- matrix(rnorm((sample_size + q)*p, 0, 1), sample_size + q, p)

  for(i in q + 1:n) {
    ds <- rsci_data(r, q, X[(i-q):i,])
    pt <- fnp(pars, ds)
    pt <- tail(do.call(cbind, pt), 1)
    st <- sample(seq_len(r), 1, prob = pt)
    U <- get_U(ds$Z, beta)
    X[i,] <- X[i - 1,] + tcrossprod(U, Phi[[st]]) + E[i,] %*% sqOmega[[st]]
  }

  if(burn != 0) {
    X <- X[-(1:burn),]
  }
  X
}
