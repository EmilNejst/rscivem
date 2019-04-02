#' @title Unvech undoes the vech operation
#'
#' @param vec vector, lower diagonal elements of a symmetric matrix.
#'
#' @return a symmetric matrix with from the vech vector

unvech <- function(vec) {

  nrows <- .5*(-1 + sqrt(1 + 8*length(vec)))
  mat <- matrix(NA, nrows, nrows)
  mat[lower.tri(mat, diag = TRUE)] <- vec
  mat[upper.tri(mat, diag = FALSE)] <- mat[lower.tri(mat, diag = FALSE)]

}
