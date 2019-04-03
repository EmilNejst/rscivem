#' @title Build parameter object
#'
#'
#'
#'
#'
#'
#' @return

rsci_pars <- function(Phi,
                      Omega,
                      beta,
                      lambda,
                      linres_Phi = NULL,
                      linres_Omega = NULL) {

  pars <- list(
    Phi = Phi,
    Omega = Omega,
    beta = beta,
    lambda = lambda,
    linres_Phi = linres_Phi,
    linres_Omega = linres_Omega
  )

  class(pars) <- "rsci_pars"

  pars
}
