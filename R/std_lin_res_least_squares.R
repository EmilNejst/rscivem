#' @title Construct linear restriction matrices for a set of common
#'        specifications for alpha
#'
#' @param dim integer, dimension of the system
#' @param rank integer, the number of cointegration relations
#' @param lags integer, the number of lags in the model
#' @param regimes tibble, a table with the regime names and groups
#' @param name string, name of the specification.
#'        Options are
#'        \itemize{
#'         \item{"fixed_alpha"}{Fix error correction parameters across regimes}
#'         \item{"fixed_alpha_group}{Fix error correction parameters across
#'                                   regimes belonging to a regime group.}
#'         \item{"zero_alpha_group"}{}
#'         \item{"fixed_gamma}{Fix short run parameters across regimes}
#'         \item{"fixed_gamma_group}{Fix short run parameters across groups}
#'         \item{"zero_gamma_group"}{}
#'         \item{"fixed"}{}
#'         \item{"fixed_group"}{}
#'         \item{"zero_group}{}
#'         \item{"none"}{The value of gamma changes with each regime}
#'        }
#'
#' @return a list with the restriction matrices Ha and ha
std_lin_res_least_squares <- function(dim, rank, regimes, restriction = "fixed") {

  res_options <-
    c("fixed_alpha",
      "fixed_alpha_group",
      "fixed_gamma",
      "fixed_gamma_group",
      "fixed",
      "fixed_group",
      "none")

  stopifnot(restriction %in% res_options)

  n_regs <- nrow(regimes)
  if(restriction = "none") {

    H <- Matrix::bdiag(
      lapply(seq_len(n_regs), function(x) {diag(dim*rank + dim^2*(lags - 1))}))
    h <- rep(0, n_regs*dim*rank + dim^2*(lags - 1))

  }else if(restriction = "fixed_gamma") {

    Hul <- diag(dim*rank + dim^2*(lags - 1))
    Hll <- cbind(
      matrix(0, (n_regs - 1)*(dim*rank + dim^2*(lags - 1)), dim*rank),
      do.call(
        what = rbind,
        args = lapply(
          X = seq_len(n_regs - 1),
          FUN = function(x) {
            rbind(matrix(0, dim*rank, dim^2*(lags - 1)),
                  diag(dim^2*(lags-1)))})))
    Hur <- rbind(
      matrix(0,
             dim*rank + dim^2*(lags - 1),
             (n_regs - 1)*(dim*rank)))
    Hlr <- Matrix::bdiag(
      lapply(
        X = seq_len(n_regs - 1),
        FUN = function(x) {
          rbind(diag(dim*rank),
                matrix(0, dim^2*(lags - 1), dim*rank))}))

    H <- cbind(rbind(Hul, Hll), rbind(Hur, Hlr))

  }else if(restriction = "fixed_gamma_group") {

  }else if(restriction = "fixed_alpha") {

    Hl <- do.call(
      what = rbind,
      args = lapply(
        X = seq_len(n_regs),
        FUN = function(x) {
          rbind(diag(dim*rank), matrix(0, dim^2*(lags - 1), dim*rank)) }))
    Hr <- Matrix::bdiag(lapply(
        X = seq_len(n_regs),
        FUN = function(x) {
          rbind(matrix(0, dim*rank, dim^2*(lags - 1)),
                diag(dim^2*(lags - 1)))}))
    H <- cbind(Hl,Hr)

  }else if("fixed_alpha_group") {

  }else if("fixed"){
    Hl <- do.call(
      what = rbind,
      args = lapply(
        X = seq_len(n_regs),
        FUN = function(x) {
          rbind(diag(dim*rank), matrix(0, dim^2*(lags - 1), dim*rank)) }))
    Hr <- do.call(rbind, lapply(
      X = seq_len(n_regs),
      FUN = function(x) {
        rbind(matrix(0, dim*rank, dim^2*(lags - 1)),
              diag(dim^2*(lags - 1)))}))
    H <- cbind(Hl,Hr)

  }else if("fixed_group") {

  }

  h <- rep(0, nrow(H))
  list(H, h)
}
