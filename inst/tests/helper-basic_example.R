# f(x,y) = x^2 + x*y + y^2
# g(x,y) = x^2 + y^2 - 1
# Df(x,y) = (2*x + y, x + 2*y)^{T}
# Dg(x,y) = (2*x, 2*y)^{T}
# Hf(x,y) = (2, 1 \\ 1, 2)
# Hg(x,y) = (2, 0 \\ 0, 2)

# maximum value at (x,y) = 2^{-1/2}(1,1) <=> f = 1.5; lambda = 3/2
if (FALSE) {
  optimal_x <- c(1,1)/sqrt(2)
  optimal_lambda <- 3/2
  optimal_f <- 1.5
  
  f <- function(X) {
    x <- X[1]; y <- X[2]
    x^2 + x*y + y^2
  }
  
  g <- function(X) {
    x <- X[1]; y <- X[2]
    x^2 + y^2 - 1
  }
  
  Df <- function(X) {
    x <- X[1]; y <- X[2]
    c(2*x + y, x + 2*y)
  }
  
  Dg <- function(X) {
    x <- X[1]; y <- X[2]
    c(2*x, 2*y) 
  }
  
  Hf <- function(X) {
    matrix(c(2,1,1,2), nrow=2)
  }
  
  Hg <- function(X) {
    matrix(c(2,0,0,2), nrow=2)
  }
}
