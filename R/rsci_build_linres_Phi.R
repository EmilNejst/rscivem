#' @title Construct linear restriction matrices for a set of common
#'        specifications for alpha,
#'
#' @param dim integer, dimension of the system
#' @param rank integer, the number of cointegration relations
#' @param lags integer, the number of lags in the model
#' @param nreg integer, the number of regimes
#' @param regimes tibble, a table with the regime names and groups. Only needed
#'        if fixed_group is used.
#' @param name string, name of the specification.
#'        Options are
#'
#' @export
#' @return a list with the restriction matrices Ha and ha
rsci_build_linres_Phi <- function(dim,
                                  rank,
                                  lags,
                                  nreg,
                                  regimes = NULL,
                                  restriction = "none") {

  res_options <-
    c("fixed",
      "fixed_group",
      "none")

  stopifnot(restriction %in% res_options)

  if(restriction == "none") {

    H <- Matrix::bdiag(
      lapply(
        X = seq_len(nreg),
        FUN = function(x) {diag(dim*rank + dim^2*(lags - 1))}))
    h <- rep(0, nreg*dim*rank + dim^2*(lags - 1))

  }else if(restriction == "fixed"){
    Hl <- do.call(
      what = rbind,
      args = lapply(
        X = seq_len(nreg),
        FUN = function(x) {
          rbind(diag(dim*rank), matrix(0, dim^2*(lags - 1), dim*rank)) }))
    Hr <- do.call(
        what = rbind,
        args = lapply(
          X = seq_len(nreg),
          FUN = function(x) {
            rbind(matrix(0, dim*rank, dim^2*(lags - 1)),
                  diag(dim^2*(lags - 1)))}))
    H <- cbind(Hl,Hr)

  }else if(restriction == "fixed_group") {

  }

  h <- rep(0, nrow(H))
  list(H = as.matrix(H), h = as.matrix(h))
}
