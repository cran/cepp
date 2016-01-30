\name{evaluator}
\alias{evaluator}

\title{Function to evaluate spatial quantiles}

\description{
This provides an objective function whose minimization yields the spatial quantiles.}

\usage{
evaluator(n, p)
}

\arguments{
  \item{n}{The number of rows in the data}
  \item{p}{The number of columns in the data}
}

\details{
Returns another function suitable for passing to an optimizer like \code{nlm} or \code{trust}.
}

\value{A function that should be passed to an optimizer.}

\references{
P. Chaudhuri. "On a geometric notion of quantiles for multivariate data." Journal of the American Statistical Association,
91(434):862-872, 1996.}

\author{
Mohit Dayal
}

\examples{
x <- rnorm(500)
dim(x) <- c(250,2)
ev <- evaluator(250,2)
##The Spatial Median
trust(ev, parinit=c(median(x[1,]), median(x[2,])), u=c(0,0),
      rinit=0.5, rmax=2e5, samp = x)
##Quantile for vector (0.2,0.3)
trust(ev, parinit=c(median(x[1,]), median(x[2,])), u=c(0.2,0.3),
      rinit=0.5, rmax=2e5, samp = x)
}
