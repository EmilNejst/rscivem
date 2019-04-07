#' @title Simulate a data series from a regime switching error correction model
#'
#' @param model rsci_model obj
#' @param fn_prob function, a function that provides the regime probabilities
#'        given a model object and data.
#' @param sample_size integer, the size of the sample.
#' @param data_exo xts, if fn_prob depends on exogenous variables, they are
#'        provided here.
#' @param burn integer, a number of periods to burn from the begining of the
#'        sample
#'
#' @regime a matrix with simulated values for the model

rsci_simulate <- function(model, fn_prob, sample_size, data_exo = NULL,
                          burn = NULL) {

  if(!is.null(data_exo)) {
    stopifnot(sample_size == nrow(data_exo) - model$lags)
  }

  q <- model$lags
  p <- model$dim
  r <- model$nreg
  X0 <- matrix(0, q, p)
  X <- matrix(NA, sample_size - burn, p)
  X <- rbind(X0, X)

  beta <- model$pars$beta
  Phi <- model$pars$Phi
  Omega <- model$pars$Omega

  E <- matrix(rnorm((sample_size + q)*p, 0, 1), sample_size, p)

  if(!is.null(data_exo)) {
    P <- fn_prob(model, NULL, data_exo)
    P <- do.call(cbind, P)
  }

  for(i in q + 1:simple_size) {
    if(!is.null(data_exo)) {
      pt <- P[i,]
    }else {
      data_t <- X[(i-q):i,]
      pt <- fn_prob(model, data_t)
      pt <- unlist(pt)
    }
    st <- sample(seq_len(r), 1, prob = pt)


    X[i,] <- X[i-1,] + (X[i-1,] %*% beta)  Phi[[st]]

  }
}
