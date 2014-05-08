# f(x,y,z) = x^2 + y^2 + z^2 + x*y + x*z + y*z + x + y + z
# g1(x,y,z) = x^4 + y^4 + z^4 - 1
# g2(x,y,z) = x^2 + y^2 + z^2 - 1

# optimum: {2.00005, {x -> 0.0000130461, y -> 0.0000130461, z -> 1.}}
optimal_x <- c(0.0000130461, 0.0000130461, 1)
optimal_f <- 2.00005

f <- function(X) {
  x <- X[1]; y <- X[2]; z <- X[3]
  x^2 + y^2 + z^2 + x*y + x*z + y*z + x + y + z
}

Df <- function(X){
  x <- X[1]; y <- X[2]; z <- X[3]
  c(1 + 2*x + y + z, 1 + x + 2*y + z, 1 + x + y + 2*z)
}

Hf <- function(X){
  matrix(c(2,1,1,1,2,1,1,1,2), nrow=3, byrow=TRUE)
}

g1 <- function(X) {
  as.numeric(c(1,1,1) %*% X^4 - 1)
}

Dg1 <- function(X){
  4 * X^3
}

Hg1 <- function(X){
  matrix(12 * c(X[1], 0, 0, 0, X[2], 0, 0, 0, X[3])^2, nrow=3, byrow=TRUE)
}

g2 <- function(X) {
  as.numeric(c(1,1,1) %*% X^2 - 1)
}

Dg2 <- function(X){
  2 * X
}

Hg2 <- function(X){
  matrix(c(2, 0, 0, 0, 2, 0, 0, 0, 2), nrow=3, byrow=TRUE)
}

G <- function(X) {
  c(g1(X), g2(X))
}

DG <- function(X) {
  cbind(Dg1(X), Dg2(X))
}

HG <- function(X) {
  array(c(Hg1(X), Hg2(X)), dim=c(3,3,2)) # dim=c(dim(Hg1(X), # of H's))
}

Lambda <- function(X, lambda) {
  x <- X[1]; y <- X[2]; z <- X[3]; lambda1 <- lambda[1]; lambda2 <- lambda[2]
  x + x^2 + y + x * y + y^2 + z + x * z + y * z + z^2 - 
    lambda2*(-1 + x^2 + y^2 + z^2) - lambda1*(-1 + x^4 + y^4 + z^4)
}

DLambda <- function(X, lambda) {
  x <- X[1]; y <- X[2]; z <- X[3];
  lambda1 <- lambda[1]; lambda2 <- lambda[2]

  c(1 + 2*x - 2*lambda2*x - 4*lambda1*x^3 + y + z,
    1 + x + 2*y - 2*lambda2*y - 4*lambda1*y^3 + z,
    1 + x + y + 2*z - 2*lambda2*z - 4*lambda1*z^3,
    1 - x^4 - y^4 - z^4,
    1 - x^2 - y^2 - z^2)
}

h <- function(X, lambda) {
  0.5 * sum( DLambda(X,lambda)^2 )
}

Dh <- function(X, lambda) {
  x <- X[1]; y <- X[2]; z <- X[3];
  lambda1 <- lambda[1]; lambda2 <- lambda[2]
  
  0.5 * c(8 - 4 * lambda2 + 8 * x - 16 * lambda2 * x + 8 * lambda2^2 * x 
    - 24 * lambda1 * x^2 - 4 * x^3 - 64 * lambda1 * x^3 
    + 64 * lambda1 * lambda2 * x^3 + 96 * lambda1^2 * x^5 
    + 8 * x^7 + 10 * y - 8 * lambda2 * y - 24 * lambda1 * x^2 * y 
    + 4 * x * y^2 - 8 * lambda1 * y^3 + 8 * x^3 * y^4 + 10 * z 
    - 8 * lambda2 * z - 24 * lambda1 * x^2 * z + 4 * x * z^2 
    - 8 * lambda1 * z^3 + 8 * x^3 * z^4, 
    8 - 4 * lambda2 + 10 * x - 8 * lambda2 * x - 8 * lambda1 * x^3 + 8 * y 
    - 16 * lambda2 * y + 8 * lambda2^2 * y + 4 * x^2 * y 
    - 24 * lambda1 * y^2 - 24 * lambda1 * x * y^2 - 4 * y^3 
    - 64 * lambda1 * y^3 + 64 * lambda1 * lambda2 * y^3 + 8 * x^4 * y^3 
    + 96 * lambda1^2 * y^5 + 8 * y^7 + 10 * z - 8 * lambda2 * z 
    - 24 * lambda1 * y^2 * z + 4 * y * z^2 - 8 * lambda1 * z^3 + 8 * y^3 * z^4,
    8 - 4 * lambda2 + 10 * x - 8 * lambda2 * x - 8 * lambda1 * x^3 
    + 10 * y - 8 * lambda2 * y - 8 * lambda1 * y^3 + 8 * z - 16 * lambda2 * z
    + 8 * lambda2^2 * z + 4 * x^2 * z + 4 * y^2 * z - 24 * lambda1 * z^2 
    - 24 * lambda1 * x * z^2 - 24 * lambda1 * y * z^2 - 4 * z^3 - 64 * lambda1 * z^3
    + 64 * lambda1 * lambda2 * z^3 + 8 * x^4 * z^3 + 8 * y^4 * z^3 
    + 96 * lambda1^2 * z^5 + 8 * z^7,
    -8 * x^3 - 16 * x^4 + 16 * lambda2 * x^4 + 32 * lambda1 * x^6 
    - 8 * x^3 * y - 8 * y^3 - 8 * x * y^3 - 16 * y^4 + 16 * lambda2 * y^4 
    + 32 * lambda1 * y^6 - 8 * x^3 * z - 8 * y^3 * z - 8 * z^3 - 8 * x * z^3 
    - 8 * y * z^3 - 16 * z^4 + 16 * lambda2 * z^4 + 32 * lambda1 * z^6,
    -4 * x - 8 * x^2 + 8 * lambda2 * x^2 + 16 * lambda1 * x^4 - 4 * y 
    - 8 * x * y - 8 * y^2 + 8 * lambda2 * y^2 + 16 * lambda1 * y^4 - 4 * z 
    - 8 * x * z - 8 * y * z - 8 * z^2 + 8 * lambda2 * z^2 + 16 * lambda1 * z^4)
}

# setup test points:
set.seed(1234)
testPts <- matrix(rnorm(25), nrow=5)