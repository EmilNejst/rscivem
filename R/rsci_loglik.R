#' @title Calculate the logliklihood value some data given a model.
#'
#' @param model rsci_model, a model object with the model in question
#' @param data xts, the data to be fitted
#' @param data_regprob xts, a matrix with the data used in the fn_regprob
#'        function. If NULL, then data is used as input for calulating regime
#'        probabilities through fn_regprobs and the fn_regprobs should reflext
#'        that.
#'
#' @export
#' @return the log-likelihood value

rsci_loglik <- function(model, data, data_regprob) {


}
