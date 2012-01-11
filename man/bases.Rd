\name{bases}
\alias{basis_nearby}
\alias{basis_random}

\title{Create random bases}

\description{Generate bases.}

\usage{
basis_random(n, d = 2)
basis_nearby(current, alpha = ALPHA, method = METHOD) 
}

\arguments{
  \item{n}{The number of rows.}
  \item{d}{The number of columns.}
  \item{current}{The current matrix.}
  \item{alpha}{How "far" should the new matrix be?}
  \item{method}{How should be new matrix be found? One of \code{linear} or \code{geodesic}.}
}

\details{
\code{basis_random} returns a new orthonormal matrix of specified dimensions.

\code{basis_nearby} generates a new orthonormal matrix, hybridizes it with the \code{current} matrix and returns it.
}

\value{A matrix of specified dimensions.}

\author{Taken from the \code{tourr} package}

\seealso{
\code{\link{ALPHA}}, \code{\link{METHOD}}
}
