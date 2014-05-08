#' @export
#' @rdname SGD
#' @param x an object of class \code{SGD}.
print.SGD <- function(x, ...) {
  cat("\n")
  if (x$converged) {
    output <- "Algorithm converged after "
  } else {
    output <- "Algorithm failed to converge in "
  }
  
  output <- paste0(output, x$iter, " iterations with learning rate: ",
                   x$alpha, "...\n")
  cat(output)
  cat("\n")
  cat(paste0("X_initial: ", paste0(x$X_initial, collapse=", "),"\n"))
  cat(paste0("lambda_initial: ", paste0(x$lambda_initial, collapse=", "), "\n"))
  cat("\n")
  cat(paste0("X_final: ", paste0(x$X_final, collapse=", "),"\n"))
  cat(paste0("lambda_final: ", paste0(x$lambda_final, collapse=", "), "\n"))
  
  if (!is.null(x$objFun) && inherits(x$objFun, "function")) {
    cat("\n")
    cat(paste0("objective function at X_initial: ", x$objFun(x$X_initial), "\n"))
    cat(paste0("objective function at X_final: ", x$objFun(x$X_final), "\n"))
  }
}