\name{pp}
\alias{pp}

\title{Creates the projection pursuit function.}

\description{These functions encapsulate everything, that is, the data, the benchmark and the index parameters, needed to compute the projection index.}

\usage{
pp(r = 0.8, n, data, oth, k)
}

\arguments{
  \item{r}{ The radius multiplier. Values between 0.5 and 3 seem to work well.}

  \item{n}{Number of Monte-Carlo Evaluations to approximate the integral. Values as low as 25 can be used.}

  \item{data}{The data for which structure needs to be found.}

  \item{oth}{The benchmark dataset.}
  
  \item{k}{The target dimension.}
}

\details{
\code{pp} is for projection pursuit.
}

\value{
The actual index function, which takes a single matrix argument, and returns the index value for that projection.
}

\author{Mohit Dayal}

\examples{
##Exploring structure in the RANDU data
##Or using the MINSTD generator
randu <- as.matrix(randu)

randtoolbox::setSeed(570)
w <- randtoolbox::congruRand(1200)
dim(w) <- c(3, 400)
w <- t(w)

m <- 'geodesic'
a <- 0.50

ranif1 <- pp(r=1, n=50, data=randu, oth=w, k=2)

set.seed(50)
F1 <- basis_random(3)
o1 <- optim(par=F1, fn=ranif1, gr=basis_nearby(), method='SANN',
            control=list(fnscale=-1, maxit=100, trace=1))
plot(randu \%*\% o1$par)

##How accurate are the values?
ranif1hi <- pp(r=1, n=500, data=randu, oth=w, k=2)
ranif1hi(o1$par)
}
