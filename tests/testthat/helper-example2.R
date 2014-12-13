f1 <- function(X) {
  x <- X[1]; y <- X[2]; z <- X[3]
  x^2 + y^2 + z^2 + x*y + x*z + y*z + x + y + z
}

g11 <- function(X) {
  as.numeric(c(1,1,1) %*% X^2 - 1)
}

Lambda1 <- function(X,lambda) {
  f1(X) - lambda * g11(X)
}

DLambda1 <- function(X,lambda) {
  x <- X[1]; y <- X[2]; z <- X[3]
  c(1 + 2*x - 2*lambda*x + y + z,
   1 + x + 2*y - 2*lambda*y + z,
   1 + x + y + 2*z - 2*lambda*z,
   1 - x^2 - y^2 - z^2)
}

h1 <- function(X, lambda) {
  0.5 * sum( DLambda1(X, lambda)^2 )
}

Dh1 <- function(X, lambda) {
  x <- X[1]; y <- X[2]; z <- X[3]
  c(4- 2*lambda + 4*x - 8*lambda*x + 4*lambda^2*x + 2*x^3 + 5*y - 4*lambda*y + 2*x*y^2 + 5*z - 4*lambda*z + 2*x*z^2,
    4- 2*lambda + 5*x - 4*lambda*x + 4*y - 8*lambda*y + 4*lambda^2*y + 2*x^2*y + 2*y^3 + 5*z - 4*lambda*z + 2*y*z^2,
    4- 2*lambda + 5*x - 4*lambda*x + 5*y - 4*lambda*y + 4*z - 8*lambda*z + 4*lambda^2*z + 2*x^2*z + 2*y^2*z + 2*z^3,
    -2*x - 4*x^2 + 4*lambda*x^2 - 2*y - 4*x*y - 4*y^2 + 4*lambda*y^2 - 2*z - 4*x*z - 4*y*z - 4*z^2 + 4*lambda*z^2)
}

optimal_X <- c(1,1,1)/sqrt(3)