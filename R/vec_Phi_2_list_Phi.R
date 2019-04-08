#' @title Trandsform the vector version of Phi to the list version
#'
#' @param vecPhi vector, the vectorized version of Phi
#' @param dim integer, the dimension of the system
#' @param rank integer, the cointegration rank
#' @param lags integer, the number of lags in the model
#' @param nreg integer, the number of regimes
#'
#' @return the list of Phi by regime
vec_Phi_2_List_Phi <- function(vecPhi, dim, rank, lags, nreg) {

  lPhi <- list()
  for(i in 1:nreg) {

    lPhi[[i]] <- matrix(
      data = vecPhi[(i-1)*(dim*rank + dim^2*(lags-1)) +
                      1:(dim*rank + dim^2*(lags-1))],
      nrow = dim,
      ncol = dim*(rank + lags-1))

  }
  lPhi
}
