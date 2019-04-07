#' @title Transform the vectorized version of Omega into the list version
#'
#' @param vecOmega vector, the vectorized version of Omega
#' @param dim integer, the dimension of the system
#' @param regs integer, the number of regimes
#'
#' @return

vec_Omega_2_list_Omega <- function(vecOmega, dim, regs) {

  lOmega <- list()
  for(i in 1:regs) {

    lOmega[[i]] <- matrix(vecOmega[(i-1)*(dim^2) + 1:(dim^2)], dim, dim)

  }
  lOmega
}
