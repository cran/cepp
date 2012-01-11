\name{pp}
\alias{pp.2}
\alias{pp.3}

\title{Creates the projection pursuit function.}

\description{These functions encapsulate everything, that is, the data, the benchmark and the index parameters, needed to compute the projection index.}

\usage{
pp.2(r = 1, n, data, oth,given_norm=penorms2)
pp.3(r = 1, n, data, oth,given_norm=penorms3)
}

\arguments{
  \item{r}{ The radius multiplier. Values between 0.5 and 3 seem to work well.}

  \item{n}{Number of Monte-Carlo Evaluations to approximate the integral. Values as low as 25 can be used.}

  \item{data}{The data for which structure needs to be found.}

  \item{oth}{The benchmark dataset.}

  \item{given_norm}{Determines how the norm of the integrand is computed. By default, set to the L2 norm (\code{penorms2} for 2-D projections and \code{penorms3} for 3-D projections) You may set it any other norm function. Note that even ordered norms are rotationally invariant.}
}

\details{
\code{pp.2} is for 2-D projection pursuit, while \code{pp.3} is for 3-D projection pursuit.
}

\value{
The actual index function, which takes a single matrix argument, and returns the index value for that projection.
}

\author{Mohit Dayal}

\examples{
##Exploring structure in the RANDU data
##Or using the MINSTD generator
randu <- as.matrix(randu)

setSeed(570)
w <- congruRand(1200)
dim(w) <- c(3,400)
w <- t(w)

m <- 'geodesic'
a <- 0.50

ranif1 <- pp.2(r=1,n=50,data=randu,oth=w)

set.seed(50)
F1 <- basis_random(3)
o1 <- optim(par=F1,fn=ranif1,gr=basis_nearby,method='SANN',control=list(fnscale=-1,maxit=200,trace=1))
plot(randu \%*\% o1$par)

##How accurate are the values?
ranif1hi <- pp.2(r=1,n=500,data=randu,oth=w)
ranif1hi(o1$par)
}
