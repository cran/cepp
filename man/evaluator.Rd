\name{evaluator}
\alias{evaluator.3}
\alias{evaluator.2}

\title{Functions to evaluate spatial quantiles}

\description{
These are objective functions whose minimization yields the spatial quantiles.}

\usage{
evaluator.2(samp)
evaluator.3(samp)
}

\arguments{
  \item{samp}{The data for which quantiles are required.}
}

\details{
Either function returns another function suitable for passing to an optimizer like \code{nlm} or \code{trust}.
}

\value{A function that should be passed to an optimizer.}

\references{
P. Chaudhuri. On a geometric notion of quantiles for multivariate data. Journal of the American Statistical Association,
91(434):862-872, 1996.}

\author{
Mohit Dayal
}

\examples{
x <- rnorm(500)
dim(x) <- c(250,2)
ev <- evaluator.2(x)
##The Spatial Median
trust(ev,parinit=c(median(x[1,]),median(x[2,])),u=c(0,0),rinit=0.5,rmax=2e5)
##Quantile for vector (0.2,0.3)
trust(ev,parinit=c(median(x[1,]),median(x[2,])),u=c(0.2,0.3),rinit=0.5,rmax=2e5)
}
