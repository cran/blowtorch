library(blowtorch)
library(mixtureModel)

###########
# build h and Dh from likelihood functions
###########

numConstraints <- 2

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

############
# test with SGD
############

library(doMPI)

cl <- startMPIcluster()
registerDoMPI(cl)

.Last <- function() {
  cat("shutting cluster down...")
  closeCluster(cl)
  mpi.finalize()   
}

nHosts <- as.numeric(Sys.getenv('NHOSTS'))
nCores <- as.numeric(Sys.getenv('NSLOTS'))

cat("running script across", nHosts, "nodes with", nCores, "cores via MPI...\n")

batchSizes <- c(1, 50, 100, 200, 500)
iterLimits <- c(10*2000, 10*400, 4*500, 2*500, 500)
learningRates <- c(0.004, 0.03, 0.03, 0.05, 0.02)

library(MCMCpack)
set.seed(1234)
initial_X <- as.vector(rdirichlet(n=1, alpha=c(4,4,4)))
initial_lambda <- rnorm(n=numConstraints)

# iterLimits <- 2*c(1,1,1,1,1)

experiment_parameters <- rbind(learningRates, iterLimits, batchSizes)
exportDoMPI(cl, varlist=ls())

SGD_results <- foreach(experiment_parameter = iter(experiment_parameters,
                                                   by="column"),
                       .packages=c("blowtorch", "mixtureModel"), .verbose=TRUE) %dopar% {
                         alpha <- experiment_parameter[1]
                         iterLimit <- experiment_parameter[2]
                         batchSize <- experiment_parameter[3]
                         
                         SGD(initial_X, initial_lambda, alpha=alpha, h, Dh, data=mixtureModel[,-1],
                             relTol=1E-7, convTol=1E-3, iterLimit=iterLimit, beta=0, batchSize=batchSize) 
                       }

save(SGD_results, file="sgd_paths.rda")
