#' @title Do a monte carlo simulation of the estimation procedure.
#'
#' @param model rsci_model, the model that we wish to simulate from and
#'        subsequently estimate parameters on.
#' @param sample_size integer, the number of observations to simulate.
#' @param replications integer, the number of replications of the experiment.
#' @param burn integer, a number of periods to burn from the beginning of
#'        the sample to avoid dependence on initial values.
#'
#' @return an rsci_mc object with all information on the experiment.

rsci_monte_carlo <- function(model, sample_size, replications) {

}
