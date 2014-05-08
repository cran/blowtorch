#' @export
#' @title Assemble the gradient of h
#' @description Assemble the gradient of h from the second and first derivatives
#' of the objective and constraint functions.
#' @template buildFunctions
#' @param Hf the Hessian matrix of the objective function, \code{f}. \code{Hf}
#' should take \code{(X,lambda)} as primary arguments, if
#' \code{\link{gradientDescent}} is being used to optimize \code{f}. If
#' \code{\link{SGD}} is being called, it must take \code{(X, lambda, data)} as
#' arguments.
#' @param HG the (aggregate) Hessian of the constrain function \code{G}. This
#' object must have the same form as the output to \code{\link{aggregateHessians}}.
#' @return a vector-valued function \code{Dh(X, lambda, data)} which is the
#' gradient of \code{h} as created in \code{\link{build_h}}. 
#' @examples
#' Df <- function(X){
#' x <- X[1]; y <- X[2]; z <- X[3]
#' c(1 + 2*x + y + z, 1 + x + 2*y + z, 1 + x + y + 2*z)
#' }
#' 
#' Hf <- function(X){
#'   matrix(c(2,1,1,1,2,1,1,1,2), nrow=3, byrow=TRUE)
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
#' Hg1 <- function(X){
#'   matrix(12 * c(X[1], 0, 0, 0, X[2], 0, 0, 0, X[3])^2, nrow=3, byrow=TRUE)
#' }
#' 
#' Hg2 <- function(X){
#'   matrix(c(2, 0, 0, 0, 2, 0, 0, 0, 2), nrow=3, byrow=TRUE)
#' }
#'
#' Dh <- buildDh(G=aggregateConstraints(g1, g2),
#'              Df=Df, 
#'              DG=aggregateGradients(Dg1, Dg2),
#'              Hf=Hf,
#'              HG=aggregateHessians(Hg1, Hg2))
#' 
#' Dh(c(1,1,1), c(1,1))
buildDh <- function(G, Df, DG, Hf, HG) {
  function(X, lambda, data=NULL) {
    collapseHessianAtX <- function(HG) {
      hessian <- HG(X)
      if (length(dim(hessian)) == 3) {
        # slice through third dimension and collapse "columns" via
        # inner product with lambda
        apply(X=hessian, MARGIN=1, FUN=function(slice) slice %*% lambda)
      } else { # scale Hessian by lambda
        lambda * hessian
      }
    }
    # extend signature of hessian and gradient
    # in the case where f doesn't depend on data.
    Hf <- addDots(Hf)
    Df <- addDots(Df)
    
    a <- Hf(X, data) - collapseHessianAtX(HG)
    b <- Df(X, data) - DG(X) %*% lambda
    
    out1 <- a %*% b + DG(X) %*% G(X) 
    out2 <- -t(DG(X)) %*% b
    c(out1,out2)
  }
}