#' @title Construct linear restriction matrices for a set of common
#'        specifications for alpha,
#'
#' @param dim integer, dimension of the system
#' @param rank integer, the number of cointegration relations
#' @param lags integer, the number of lags in the model
#' @param n_regimes integer, the number of regimes
#' @param regimes tibble, a table with the regime names and groups. Only needed
#'        if fixed_group is used.
#' @param name string, name of the specification.
#'        Options are
#'        \itemize{
#'         \item{"fixed"}{}
#'         \item{"fixed_group"}{}
#'         \item{"none"}{The value of gamma changes with each regime}
#'        }
#'
#' @export
#' @return a list with the restriction matrices Ha and ha
rsci_build_linres_Phi <- function(dim,
                                  rank,
                                  lags,
                                  n_regimes,
                                  regimes = NULL,
                                  restriction = "none") {

  res_options <-
    c("fixed",
      "fixed_group",
      "none")

  stopifnot(restriction %in% res_options)

  n_regimes <- nrow(regimes)
  if(restriction = "none") {

    H <- Matrix::bdiag(
      lapply(
        X = seq_len(n_regimes),
        FUN = function(x) {diag(dim*rank + dim^2*(lags - 1))}))
    h <- rep(0, n_regimes*dim*rank + dim^2*(lags - 1))

  }else if("fixed"){
    Hl <- do.call(
      what = rbind,
      args = lapply(
        X = seq_len(n_regimes),
        FUN = function(x) {
          rbind(diag(dim*rank), matrix(0, dim^2*(lags - 1), dim*rank)) }))
    Hr <- do.call(rbind, lapply(
      X = seq_len(n_regimes),
      FUN = function(x) {
        rbind(matrix(0, dim*rank, dim^2*(lags - 1)),
              diag(dim^2*(lags - 1)))}))
    H <- cbind(Hl,Hr)

  }else if("fixed_group") {

  }

  h <- rep(0, nrow(H))
  list(H, h)
}
