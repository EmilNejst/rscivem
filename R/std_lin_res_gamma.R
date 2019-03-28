#' @title Construct linear restriction matrices for a set of common
#'        specifications
#'
#' @param dim integer, dimension of the system
#' @param lags integer, the number of lags in the model
#' @param name string, name of the specification.
#'        Options are
#'        \itemize{
#'         \item{"fixed_gamma"}{Fix short run parameters across regimes}
#'         \item{"fixed_gamma_group"}{Fix short run parameters across regimes
#'                                    belonging to a regime group.}
#'         \item{"fixed_alpha"}{Fix error correction parameters across regimes}
#'         \item{"fixed_alpha_group}{Fix error correction parameters across
#'                                   regimes belonging to a regime group.}
#'         \item{"fixed"}{Fix all least squares parameters across regimes.}
#'         \item{"fixed_group"}{Fix all least squares parameters across groups.}
#'         \item{"none"}{The value of gamma changes with each regime}
#'        }
#'
#' @return a list with the restriction matrices H_sr and h_sr
std_lin_res_gamma <- function(dim, lags, name = "fixed") {

}
