#' @title Construct linear restriction matrices for a set of common
#'        specifications.
#'
#' @param dim integer, dimension of the system
#' @param nreg integer, the number of regimes
#' @param regimes tibble, a table with the regime names and groups. Only needed
#'        if the option "fixed_group" is chosen.
#' @param name string, name of the specification.
#'
#' @export
#' @return a list with the restriction matrices H_ec and h_ec for the chosen
#'         standard specification.
rsci_build_linres_Omega <- function(dim, nreg, regimes = NULL, name = "none") {

  stopifnot(name %in% c("fixed", "fixed_group", "none"))

  if(name == "fixed") {

    H <- do.call(
      what = rbind,
      args = lapply(
        X = seq_len(nreg),
        FUN = function(x) { matrixcalc::duplication.matrix(dim) }))

  }else if(name == "fixed_group") {



  }else {

    H <- Matrix::bdiag(
      lapply(
        X = seq_len(nreg),
        FUN = function(x) { matrixcalc::duplication.matrix(dim) }))

  }
  h <- rep(0, nrow(H))

}
