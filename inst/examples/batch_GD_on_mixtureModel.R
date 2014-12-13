context("batch GD on full mixtureModel")

runTest <- FALSE

############
# build h and Dh from full likelihood functions;
# shouldn't need more than 1 constraint for optim()
########### 
numConstraints <- 1

if (numConstraints == 2) {
  h <- build_h(G=aggregateConstraints(constrs, constrs2),
               Df=mixtureModelDLogLikelihood,
               DG=aggregateGradients(Dconstrs, Dconstrs2))
  
  Dh <- buildDh(G=aggregateConstraints(constrs, constrs2),
                Df=mixtureModelDLogLikelihood,
                DG=aggregateGradients(Dconstrs, Dconstrs2),
                Hf=mixtureModelHLogLikelihood,
                HG=aggregateHessians(Hconstrs, Hconstrs2)) 
} else if (numConstraints == 1) {
  h <- build_h(G=aggregateConstraints(constrs),
               Df=mixtureModelDLogLikelihood,
               DG=aggregateGradients(Dconstrs))
  
  Dh <- buildDh(G=aggregateConstraints(constrs),
                Df=mixtureModelDLogLikelihood,
                DG=aggregateGradients(Dconstrs),
                Hf=mixtureModelHLogLikelihood,
                HG=aggregateHessians(Hconstrs)) 
}
##########
# build optim wrappers
##########
fn <- function(x) {
  X <- x[1:3]
  lambda <- ifelse(numConstraints == 1, x[4], x[4:5])
  h(X,lambda, data=mixtureModel[1:500,-1])
}

gr <- function(x) {
  X <- x[1:3]
  lambda <- ifelse(numConstraints == 1, x[4], x[4:5])
  Dh(X,lambda, data=mixtureModel[1:500,-1])
}
##############
# test with optim's BFGS and CG
##############
if (runTest) {
  t <- optim(c(q$X_final, q$lambda_final), fn, gr=gr, method="BFG",
             control=list(trace=1, maxit=200))
  
  s <- optim(c(q$X_final, q$lambda_final), fn, gr=gr, method="CG",
             control=list(trace=1, maxit=100))
}