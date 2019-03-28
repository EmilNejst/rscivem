#' @title Calculate error covariances given data and other parameters
#'
#' @param alpha tibble, a table with regime names and matrices
#' @param beta matrix, the cointegration paramters
#' @param gamma tibble, a table with regime names and matrices
#' @param lambda vector, the parameter vector that goes into the 
#' 
#' @return a list with covariance matrices for each regime
calc_error_covariance <- function(alpha, beta, gamma, lambda, H_ec, h_ec) {
  
}