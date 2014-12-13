set.seed(1234)
df <- mixtureModel[, -1]
df$Y <- rnorm(n=nrow(df)) + 3*df$X1 - 7*df$X2

LS_coefs <- coefficients(lm(Y ~ X1 + X2, data=df))

loss <- function(X, lambda, data) {
  beta0 <- X[1]; beta <- as.vector(X[-1])
  data <- as.matrix(data)
  y <- data[,1]; x <- data[,-1]
  sum(0.5*(x %*% beta + beta0 - y)^2)
}

Dloss <- function(X, lambda, data) {
  beta0 <- X[1]; beta <- as.vector(X[-1])
  data <- as.matrix(data)
  y <- data[,1]; x <- data[,-1]
  
  out1 <- (x %*% beta + beta0 - y)
  out2 <- x * matrix(out1, ncol=ncol(data)-1, nrow=nrow(data)) 
  colSums(cbind(out1,out2,0))
}

X0 <- c(0,0,0)
lambda0 <- 0
alpha <- 0.0001

t <- SGD(X0, lambda0, alpha=1e-5, loss, Dloss, data=df[, c(3,1,2)], iterLimit=4*2000,
    trace=50, plotTrace=0, beta=0, batchSize=15)
