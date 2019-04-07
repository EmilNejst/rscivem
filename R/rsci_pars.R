#' @title Build parameter object
#'
#' @param Phi list, a list with Phi matrices for all regimes
#' @param Omega list, a list with the Omega matrices for all regimes
#' @param beta matrix, the matrix of cointegration relations
#' @param lambda vector, the vector of probability parameters
#' @param linres_Phi list, a list of an H matrix and an h vector for imposing
#'        linear restrictions on a vector given by,
#'        \deqn{ vec(\Phi) = vec((Phi_1, Phi_2,..., Phi_m))  }
#'        such that
#'        \deqn{ vec(\Phi) = H\rho + h}
#'        where m refers to the number of regimes and \eqn{\rho} .
#'        This use of linear restrictions allows the user to impose cross regime
#'        restrictions. That is useful if one wishes certain elements to be
#'        fixed across regimes.
#'        NULL is default resulting in an unrestricted system.
#' @param linres_Omega list, a list of an H matrix and an h vector for imposing
#'        linear restriction on a vector given by,
#'        \deqn{vec(\Omega) = (vec(\Omega_1), vec(\Omega_2),...,vec(\Omega_m))
#'        such that
#'        \deqn{ vec(\Omega) = H\nu + h}
#'        where \eqn{\nu} is the vector of freely varying parameters in Omega.
#'        NULL is default and will result in an unrestricted system.
#'
#' @return an object of type rsci_pars

rsci_pars <- function(Phi,
                      Omega,
                      beta,
                      lambda,
                      linres_Phi = NULL,
                      linres_Omega = NULL) {

  if(is.null(linres_Phi)) {
    linres_Phi <- rsci_build_linres_Phi(
      dim = nrow(Phi[[1]]),
      rank = ncol(beta),
      lags = ifelse(
        test = ncol(Phi) == ncol(beta),
        yes = ncol(beta),
        no = (ncol(Phi) - ncol(beta))/nrow(beta)),
      n_regimes = length(Phi))
  }

  if(is.null(linres_Omega)) {
    linres_Omega <- rsci_build_linres_Omega(nrow(Omega[[1]]), length(Omega))
  }

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
