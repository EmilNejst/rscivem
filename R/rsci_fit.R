#' @title Fit a regime switching cointegration vector error correction model to
#'        data.
#'
#' @param spec rsci_spec, a specification object
#' @param init_lambda vector a vector of
#' @param init_b vector, a vector of initial values for the estimated parts
#'        of beta.
#' @param fn_b2beta function, a function that takes the values from b and
#'        produces a matrix of size (p x r) coresponding to the long run
#'        cointegration vectors.
#' @param fn_regprob function, a function that takes two arguments
#'        \code{ fn_regprob(model, data) } where model is a rsci_model
#'        object and data is an xts object with the time series data used in
#'        rsci_fit. The function must return a simple vector of regime
#'        probabilities that has length corresponding to the number of columns
#'        in the regimes tibble in the rsci_spec object.
#' @param data xts, a matrix with the data.
#' @param data_regprob xts, a matrix with the data used in the fn_regprob
#'        function. If NULL, then data is used as input for calulating regime
#'        probabilities through fn_regprobs and the fn_regprobs should reflext
#'        that. In terms of optimization, it can make sense to provide this data
#'        even if it is just a manipulation of data in the data input, since the
#'        manipulation step can the be avoided in the evaluation of the
#'        likelihood function.
#' @param restrict_alpha list, standard restriction setup - see details.
#' @param restrict_gamma list, standard restriction setup - see details.
#' @param restrict_omega list, standard restriction setup - see details.
#' @param zero_restrictions list, give instructions for imposing zero
#'        restrictions on particular parameters - see details.
#'
#' @details For standard restrictions on alpha, gamma or omega, use the name
#'          attribute in the list. options are
#'          \itemize{
#'           \item{"fixed"}{Fix short run parameters across regimes}
#'           \item{"fixed_group"}{Fix short run parameters across regimes
#'                                belonging to a regime group.}
#'           \item{"fixed"}{Fix error correction parameters across regimes}
#'           \item{"fixed_group}{Fix error correction parameters across
#'                               regimes belonging to a regime group.}
#'           \item{"none"}{The value of lambda changes with each regime.}}
#'
#'           If you really know what you are doing, you can provide H and h
#'           matrices for imposing generalized linear restrictions on the
#'           stacked vectors of the freely varying parameters for alpha, gamma
#'           or omega across regimes.
#'
#'           \code{zero_restrictions} are imposed using lists. For example,
#'           imposing on the second row of the first column on the alpha in
#'           regime one, one writes list(alpha_1 = matrix(0, NA, 0, 0), 2, 2)
#'
#' @export
#' @return an rsci_fit object

rsci_fit <- function(spec,
                     data,
                     init_lambda,
                     init_b,
                     fn_b2beta,
                     fn_regprob,
                     data_regprob = NULL,
                     restrict_alpha = c(name = "none", Ha = NULL, ha = NULL),
                     restrict_gamma = c(name = "none", Hg = NULL, hg = NULL),
                     restrict_omega = c(name = "none", Ho = NULL, ho = NULL),
                     zero_restrictions = NULL,
                     ...) {



}
