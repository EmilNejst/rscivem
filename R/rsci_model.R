#' @title Create a regime switching cointegrated vector error correction
#'
#' @param dim integer, the number of dimentions in the data
#' @param lags integer, the number of lags in levels in the model.
#' @param rank integer, the cointegration rank.
#' @param nreg integer, the number of regimes.
#' @param pars vector, parameters that enter the probability function
#'        to generate regime probabilities.
#' @param fn_prob function, a function that takes at least two arguments
#'        \code{ fn_prob(pars, data_struct) } where pars is a the vector of
#'        cointegration and probability parameters. data_struct is a rsci_data
#'        object.
#' @param beta matrix, the matrix of cointegration relations
#' @param lambda vector, the vector of probability parameters
#' @param Phi list, a list with Phi matrices for all regimes. Default is NULL
#'        in which case a standardized procedure is used for estimating initial
#'        values.
#' @param Omega list, a list with the Omega matrices for all regimes. Default is
#'        NULL in which case a standardized procedure is used.
#' @param linres_Phi list, a list of an H matrix and an h vector for imposing
#'        linear restrictions on a vector given by,
#'        \deqn{ vec(\Phi) = vec((\Phi_1, \Phi_2,..., \Phi_m))  }
#'        such that
#'        \deqn{ vec(\Phi) = H\rho + h}
#'        where m refers to the number of regimes and \eqn{\rho} .
#'        This use of linear restrictions allows the user to impose cross regime
#'        restrictions. That is useful if one wishes certain elements to be
#'        fixed across regimes.
#'        NULL is default resulting in an unrestricted system.
#' @param linres_Omega list, a list of an H matrix and an h vector for imposing
#'        linear restriction on a vector given by,
#'        \deqn{vec(\Omega) = (vec(\Omega_1), vec(\Omega_2),...,vec(\Omega_m))}
#'        such that
#'        \deqn{ vec(\Omega) = H\nu + h}
#'        where \eqn{\nu} is the vector of freely varying parameters in Omega.
#'        NULL is default and will result in an unrestricted system.
#'
#' @export
#' @return an rsci_model object holding the model
rsci_model <- function(dim,
                       rank,
                       lags,
                       nreg,
                       pars,
                       fn_prob,
                       beta = NULL,
                       lambda = NULL,
                       Phi = NULL,
                       Omega = NULL,
                       linres_Phi = rsci_build_linres_Phi(dim, rank, lags, nreg),
                       linres_Omega = rsci_build_linres_Omega(dim, nreg)) {

  model <- list(
    dim = dim,
    rank = rank,
    lags = lags,
    nreg = nreg,
    pars = pars,
    Phi = Phi,
    Omega = Omega,
    beta = beta,
    lambda = lambda,
    fn_prob = fn_prob,
    linres_Phi = linres_Phi,
    linres_Omega = linres_Omega)

  class(model) <- "rsci_model"

  model
}
