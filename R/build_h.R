# I suspect this function is never used.
buildLambda <- function(f, G) {
  function(X,lambda) {
    f(X) - lambda %*% G(X)
  }
}

buildDLambda <- function(G, Df, DG) {
  Df <- addDots(Df) # extend Df in the case that f does not depend on data
  function(X, lambda, data) {
    out1 <- Df(X, data) - DG(X) %*% lambda
    c(out1, -G(X))
  }
}

#' @export
#' @title Assemble h
#' @description Build h from the first derivatives of the object and constraint
#' functions.
#' @template buildFunctions
#' @return a scalar-valued function \code{h(X, lambda, data)} which is half the 
#' squared euclidean norm of the gradient of the Lagrangian created by \code{f}
#' and \code{G}. 
#' @examples
#' Df <- function(X){
#' x <- X[1]; y <- X[2]; z <- X[3]
#' c(1 + 2*x + y + z, 1 + x + 2*y + z, 1 + x + y + 2*z)
#' }
#' 
#' g1 <- function(X) {
#'   as.numeric(c(1,1,1) %*% X^4 - 1)
#' }
#' 
#' g2 <- function(X) {
#'   as.numeric(c(1,1,1) %*% X^2 - 1)
#' }
#' 
#' Dg1 <- function(X){
#'   4 * X^3
#' }
#' 
#' Dg2 <- function(X){
#'   2 * X
#' }
#' 
#' h <- build_h(G=aggregateConstraints(g1, g2),
#'              Df=Df, 
#'              DG=aggregateGradients(Dg1, Dg2))
#' 
#' h(c(1,1,1), c(1,1))
build_h <- function(G, Df, DG) {
  DLambda <- buildDLambda(G, Df, DG)
  function(X, lambda, data) {
    0.5 * sum( DLambda(X, lambda, data)^2 )
  }
}