#' @family aggregate functions
#' @export
#' @title Aggregate constraints.
#' @description Collect constraint functions into a vector.
#' @param ... constraint functions. Must have take a single argument and return
#' a single value.
#' @return a function which takes a value \code{X} and returns a vector with
#' length equal to \code{length(...)}. The i-th entry of the return value is the
#' i-th constraint function evaluated at \code{X}.
#' @examples
#' g1 <- function(X) {
#' as.numeric(c(1,1,1) %*% X^4 - 1)
#' }
#' 
#' g2 <- function(X) {
#'   as.numeric(c(1,1,1) %*% X^2 - 1)
#' }
#' 
#' G <- aggregateConstraints(g1, g2)
#' G(c(1,1,1))
aggregateConstraints <- function(...) {
  constrList <- list(...)
  
  constraint = NULL # remove note in R CMD check
  
  function(X) {
    foreach(constraint = iter(constrList), .combine=c, .final=as.vector) %do% {
      constraint(X)
    }
  }
}