#' @family aggregate functions
#' @export
#' @title Aggregate constraint Hessians.
#' @description Collect Hessian matrices of constraint functions into a 3D array.
#' @param ... Hessians of constraint functions.
#' @return a function which takes a value \code{X} and returns a 3D array with
#' with dimensions \code{c(length(X), length(X), length(...))}. The (i,j,k) entry
#' of the return value is the ij-th partial derivative of the k-th constraint,
#' evaluated at \code{X}.
#' @examples
#' Hg1 <- function(X){
#' matrix(12 * c(X[1], 0, 0, 0, X[2], 0, 0, 0, X[3])^2, nrow=3, byrow=TRUE)
#' }
#' 
#' Hg2 <- function(X){
#'   matrix(c(2, 0, 0, 0, 2, 0, 0, 0, 2), nrow=3, byrow=TRUE)
#' }
#' 
#' HG <- aggregateHessians(Hg1, Hg2)
#' HG(c(0,0,0))
#' 
aggregateHessians <- function(...) {
  hessianList <- list(...)
  
  stackMatrices <- function(matList) {
    finalDims <- c(dim(matList[[1]]), length(matList))
    array(do.call(c, matList), dim=finalDims)
  }
  
  hessian=NULL # do this to clear up CMD check note.
  
  function(X) {
    foreach(hessian = iter(hessianList), .final=stackMatrices) %do% {
      hessian(X)
    }
  }
}