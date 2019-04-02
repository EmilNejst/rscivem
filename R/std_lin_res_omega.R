#' @title Construct linear restriction matrices for a set of common
#'        specifications
#'
#' @param dim integer, dimension of the system
#' @param regimes tibble, a table with the regime names and groups
#' @param name string, name of the specification.
#'        Options are
#'        \itemize{
#'         \item{"fixed"}{Fix short run parameters across regimes}
#'         \item{"fixed_group"}{Fix short run parameters across regimes
#'                                    belonging to a regime group.}
#'         \item{"none"}{The value of lambda changes with each regime.}
#'        }
#' @param
#'
#' @return a list with the restriction matrices H_ec and h_ec for the chosen
#'         standard specification.
std_lin_res_omega <- function(dim, regimes, name) {

  stopifnot(name %in% c("fixed", "fixed_group", "none"))

  nregs <- nrow(regimes)
  if(name == "fixed") {

    H <- do.call(
      what = rbind,
      args = lapply(
        X = seq_len(nregs),
        FUN = function(x) { matrixcalc::duplication.matrix(dim) }))

  }else if(name == "fixed_group") {



  }else {

    H <- Matrix::bdiag(
      lapply(
        X = seq_len(nregs),
        FUN =function(x) { matrixcalc::duplication.matrix(dim) }))

  }
  h <- rep(0, nrow(H))

}
