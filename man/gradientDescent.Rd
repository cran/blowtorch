% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/gradientDescent.R
\name{gradientDescent}
\alias{gradientDescent}
\title{Gradient Descent}
\usage{
gradientDescent(X0, lambda0, alpha, h, Dh, convTol = 1e-10,
  relTol = convTol, iterLimit = 100, trace = 0, plotTrace = trace,
  rngSeed = 1234, objFun = NULL, ...)
}
\arguments{
\item{X0}{initial value for X}

\item{lambda0}{initial value for lambda}

\item{alpha}{learning rate -- either a constant of a vector whose size is
equal to \code{c(X0, lambda0)}.}

\item{h}{function whose signature is \code{(X,lambda, data)} which is
vectorized over \code{data}. \code{h} is assumed to be non-negative, and SGD
will attempt to minimize it.}

\item{Dh}{gradient of \code{h} as a function of \code{X}, \code{lambda}, and
\code{data}. Also assumed to be vectorized over \code{data}.}

\item{convTol}{positive value which signals \code{h}'s convergence.}

\item{relTol}{positive value. If all parameter estimates and \code{h} do not
change more than \code{relTol} in the course of one iteration, SGD will stop.}

\item{iterLimit}{the maximum number of iterations.}

\item{trace}{non-negative integer. At every \code{trace} iteration,
\code{SGD} will output the current parameter estimates to stdout. A value
of zero indicates no desired output.}

\item{plotTrace}{non-negative integer. At every \code{plotTrace} iteration,
\code{SGD} will graphically output the parameter estimates up to that
iteration. A value of zero indicates no desired output}

\item{rngSeed}{the random number generation seed value.}

\item{objFun}{the objective function in the Lagrangian to be optimized.}

\item{...}{layout arguments to be passed to \code{\link{multiplot}}.}
}
\value{
an '\code{SGD}' object with methods \code{\link{print.SGD}} and
\code{\link{plot.SGD}}. An \code{SGD} object is just a glorified list with
the following entries
\item{X_final}{the last iteration's \code{X} values}
\item{lambda_final}{the last iteration's \code{lambda} values}
\item{X_initial}{the initial \code{X} value}
\item{lambda_initial}{the initia \code{lambda} value}
\item{X}{the matrix whose i-th column contains the i-th iteration's \code{X}
 values }
\item{lambda}{the matrix whose i-th column contains the i-th iteration's
  \code{lambda}  values }
\item{delta}{the vector whose i-th entry contains the value of \code{h} at
the i-th iteration}
\item{alpha}{the given learning rate}
\item{beta}{the given annealing rate}
\item{batchSize}{the given batchSize}
\item{iter}{the number of iterations taken}
\item{convTol}{the given convergence tolerance}
\item{objFun}{the original objective function}
\item{converged}{a boolean indicating if \code{h} was driven below
\code{convTol}}
\item{aboveTol}{a boolean indicating if any \code{X} or \code{lambda} values
stabilized below \code{relTol}}
}
\description{
Minimze a function \code{h(X,lambda)} with its gradient,
\code{Dh(X,lambda)} via gradient descent.
}
\details{


Follows the gradient method described in
\code{system.file("final_project", "final_paper", "final_paper.pdf",
 package="blowtorch")}. All files associated with the creation of the .pdf's
 in \code{inst/final_project/} are available through the github repository,
 \url{https://github.com/stevenpollack/blowtorch}
}
\seealso{
Other gradient methods: \code{\link{SGD}},
  \code{\link{print.SGD}}
}

