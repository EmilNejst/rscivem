#' @title Construct U
#'
#' @param Z list, the list with Z0, Z1 and Z2
#' @param beta matrix, the cointegration relations
#'
#' @return matrix with U

get_U <- function(Z, beta) {

  if(is.null(Z$Z2)) {
    U <- Z$Z1 %*% beta
  }else {
    U <- cbind(Z$Z1 %*% beta, Z$Z2)
  }
  U

}
