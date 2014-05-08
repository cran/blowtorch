#' @family aggregate functions
#' @export
#' @title Aggregate constraint gradients.
#' @description Collect gradients of constraint functions into a matrix.
#' @param ... gradients of constraint functions.
#' @return a function which takes a value \code{X} and returns a matrix with
#' with dimensions \code{c(length(X), length(...))}. The (i,j) entry
#' of the return value is the i-th partial derivative of the j-th constraint,
#' evaluated at \code{X}.
#' @examples
#' Dg1 <- function(X){
#'   4 * X^3
#' }
#' 
#' Dg2 <- function(X){
#'   2 * X
#' }
#' 
#' DG <- aggregateGradients(Dg1, Dg2)
#' DG(c(1,1,1))
aggregateGradients <- function(...) {
  gradList <- list(...)
  
  gradient = NULL # remove note in R CMD check
  
  function(X) {
    foreach(gradient = iter(gradList), .combine=cbind, .final=as.matrix) %do% {
      gradient(X)
    }
  }
}