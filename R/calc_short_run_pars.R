#' @title Calculate short run parameters given data and other parameters
#'
#' @param beta matrix, the cointegration relations
#' @param lambda vector, parameters for the cointegration relations
#' @param omega list, a list with the covariance matrices for each regime
#' @param spec rsci_spec, a specification object.
#' @param data xts, the data the model should fit to.
#' 
#' @return a list with the short run parameters, alpha and gamma for each regime 
calc_short_run_pars <- function(beta, 
                                omega,
                                spec,
                                data, 
                                regime_prob,
                                H_sr, 
                                h_sr) {
  
}