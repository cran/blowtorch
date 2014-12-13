context("SGD on example2")

iterLimit <- 200
convTol <- 1E-10

X0 <- rep.int(.Machine$double.eps,3)/sqrt(3)
lambda0 <- 2 + .Machine$double.eps

# 
# print(sum( Dh1(X0, lambda0)^2 ))
alpha <- 2^(-3)#sum( Dh1(X0, lambda0)^2 )^(-3)

t <- gradientDescent(X0, lambda0, alpha, h1, Dh1, f1, convTol, iterLimit, FALSE)

print(t)