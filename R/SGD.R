#' @template gradientMethods
#' 
#' @title Stochastic Gradient Descent
#' 
#' @description Minimize a function \code{h} via stochastic gradient descent.
#' 
#' @param data the data set whose rows (the number of which is determined by
#' \code{batchSize}) are to be fed to \code{h} and \code{Dh}. 
#' @param beta annealing constant for step size. See details. 
#' @param batchSize number of observations to feed to \code{h} and \code{Dh} at
#' each iteration. When \code{batchSize} is equal to \code{nrow(data)} SGD 
#' will perform exactly like \code{\link{gradientDescent}}.
#' 
#' @export
SGD <- function(X0, lambda0, alpha, h, Dh, data, convTol=1E-10, relTol=convTol,
                iterLimit=100, beta=0.5, batchSize=1, trace=0, plotTrace=trace,
                rngSeed=1234, objFun = NULL, ...) {
  
  dimX <- length(X0)
  dimlambda <- length(lambda0)
  
  X <- matrix(X0, ncol=1)
  rownames(X) <- paste0("X[", seq.int(dimX), "]")
  
  lambda <- matrix(lambda0, ncol=1)
  rownames(lambda) <- paste0("lambda[", seq.int(dimlambda),"]")
  
  
  annealingSeq <- alpha / (seq.int(iterLimit))^(beta)
  
  
  N <- nrow(data)
  
  set.seed(rngSeed)
  rowIndices <- rep.int(sample(N), 2)

  iter <- obsIndex <- 1
  delta <- Delta <- h(X0, lambda0, data[rowIndices[1], ])
  aboveTol <- (delta > convTol)
  converged <- !aboveTol
  
  miniTrace <- ifelse(trace > 100, 10^(floor(log(trace,10))-2), 0)
  
  while(aboveTol && iter <= iterLimit) {
    if (obsIndex > N) { # reset observation index, and reshuffle data
      obsIndex <- 1
      rowIndices <- rep.int(sample(N), 2)
    }
    
    obs <- data[rowIndices[obsIndex:(obsIndex+batchSize-1)],]
    
    X_i <- X[, iter]; lambda_i <- lambda[, iter]
    
    grad_i <- Dh(X_i, lambda_i, obs)
    
    update <- as.vector(c(X_i, lambda_i) - annealingSeq[iter] * grad_i)
    
    X_new <- head(update, n=dimX)
    lambda_new <- tail(update, n=dimlambda)
    
    X <- cbind(X, X_new)
    lambda <- cbind(lambda, lambda_new)
    
    delta_new <- h(X_new, lambda_new, obs)
    
    # check if any updates are outside relative tolerance
    aboveTol <- any(abs(c(X_i, lambda_i, delta) - c(update, delta_new)) > relTol) 
    
    # short-circuit aboveTol in the case where delta_new < convTol
    aboveTol <- ifelse(delta_new < convTol,{converged <- TRUE; FALSE}, aboveTol)
    
    if (is.na(delta) || is.nan(delta) || is.infinite(delta)) {
      cat("\n*** Error with delta ***\n")
      cat("iteration:", iter, "\n")
      cat("delta:", delta, "\n")
      cat("gradient:\n")
      print(c(gradient=grad_i))
      msg <- "-- breaking from loop -- try re-running with a smaller alpha, or a larger beta"
      if (is.infinite(delta)) {
        warning(paste("delta is Inf", msg))
      } else if (is.na(delta)) {
        warning(paste("delta is NA", msg))
      } else if (is.nan(delta)) {
        warning(paste("deltais NaN", msg))
      }
      break
    }
    
    Delta <- c(Delta, delta_new)
    
    if (trace) {
      if (iter %% trace == 0) {
        cat(paste0("\niteration: ", iter, ", median(Delta): ", median(Delta),"\n"))
        print(c(h=delta_new))
        print(c(X=X_new))
        print(c(lambda=lambda_new))
        print(c(gradient=grad_i))
      } else if (miniTrace && iter %% miniTrace == 0) {
        cat(".")
        flush.console()
      } 
      if (plotTrace && iter %% plotTrace ==0) {
        dots <- list(...)
        if (is.null(dots$cols) && is.null(dots$layout)) {
          plotParameterEstimates(X, lambda, Delta, cols=dimX)
        } else {
          plotParameterEstimates(X, lambda, Delta, ...) 
        }
      }
    }
    
    iter <- iter+1
    obsIndex <- obsIndex + 1
    delta <- delta_new
  }
  
  out <- list(X_final=unname(X[,iter]),
              lambda_final=unname(lambda[,iter]),
              X_initial=X0,
              lambda_initial=lambda0,
              X=X,
              lambda=lambda,
              delta=Delta,
              alpha=alpha,
              beta=beta,
              batchSize=batchSize,
              iter=iter,
              delta=delta,
              convTol=convTol,
              objFun = objFun,
              converged=converged,
              aboveTol=aboveTol)
  
  if (!converged) {
    print("Algorithm failed to converge delta below convTol")
  }
  
  class(out) <- c("SGD", class(out))
  return(out)
}