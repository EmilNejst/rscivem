#' @title Construct linear restriction matrices for a set of common
#'        specifications
#'
#' @param dim integer, dimension of the system
#' @param lags integer, the number of lags in the model
#' @param name string, name of the specification.
#'        Options are
#'        \itemize{
#'         \item{"fixed"}{Fix short run parameters across regimes}
#'         \item{"fixed_group"}{Fix short run parameters across regimes
#'                                    belonging to a regime group.}
#'         \item{"fixed"}{Fix error correction parameters across regimes}
#'         \item{"fixed_group}{Fix error correction parameters across
#'                                   regimes belonging to a regime group.}
#'         \item{"none"}{The value of lambda changes with each regime.}
#'        }
#' @return a list with the restriction matrices H_ec and h_ec for the chosen
#'         standard specification.
std_lin_res_omega <- function(dim, lags, name) {

}
