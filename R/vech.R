#' @title Vech operator as in Magnus and Neudecker.
#'
#' @param mat matrix.
#'
#' @return a vector with the lower triangle elements of the input matrix,
#'         including the diagonal.

vech <- function(mat) {
  mat[lower.tri(mat, diag = TRUE)]
}
