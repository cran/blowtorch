#' @export
#' @title Gradient Descent
#' @description Minimze a function \code{h(X,lambda)} with its gradient, 
#' \code{Dh(X,lambda)} via gradient descent.
#' 
#' @template gradientMethods
#'
gradientDescent <- function(X0, lambda0, alpha, h, Dh, convTol=1e-10,
                            relTol=convTol, iterLimit=100, trace=0,
                            plotTrace=trace, rngSeed=1234, objFun=NULL, ...) {
  
  H <- function(X, lambda, data) h(X, lambda)
  DH <- function(X, lambda, data) Dh(X, lambda)
  
  SGD(X0, lambda0, alpha, h=H, Dh=DH, data=NULL, convTol=convTol, relTol=relTol,
      iterLimit=iterLimit, beta=0, trace=trace, plotTrace=plotTrace,
      rngSeed=rngSeed, objFun=objFun, ...)
}