#' @title Fit a regime switching cointegration vector error correction model to
#'        data.
#'
#' @param model rsci_model object, the object containing the initial model.
#' @param init_pars vector or number, initial parameters for the freely varying
#'        elements in beta and the parameters in the probability function.
#' @param fn_update function, a function that takes the values from init_pars
#'        and allocates the parameters correctly to the model object.
#' @param data xts, a matrix with the endogenous data.
#' @param linres_Phi list, a list with the linear restriction matrix H and
#'        the normalizing vector h for the least squares parameters.
#' @param linres_Omega list, a list with the linear restriction matrix H and
#'        the normalizing vector h for the error covariance parameters.
#' @param ... additional information to be passed to optim().
#'
#' @export
#' @return an rsci_fit object

rsci_fit <- function(model,
                     data,
                     init_pars,
                     fn_update,
                     linres_Phi = list(H = NULL, h = NULL),
                     linres_Omega = list(H = NULL, h = NULL),
                     ...) {



}
