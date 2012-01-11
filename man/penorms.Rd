\name{penorms}
\alias{penorm2}
\alias{penorms2}
\alias{penorm21}
\alias{penorm2infi}
\alias{penorm3}
\alias{penorms3}
\alias{penorm31}
\alias{penorm3infi}

\title{Parallelized functions for different norms}

\description{These functions compute the L-1, L-2 and L-infinity norms for 2 and 3 dimensional vectors. These functions are parallelized, that is you can pass several vectors at once.}

\usage{
penorm21(x, y)
penorm2infi(x, y)
penorm2(x, y)
penorms2(x, y)
penorm31(x, y, z)
penorm3infi(x, y, z)
penorm3(x, y, z)
penorms3(x, y, z)
}

\arguments{
  \item{x}{ The vector of the first coordinates. }
  \item{y}{ The vector of the second coordinates. }
  \item{z}{ The vector of the third coordinates. }
}

\details{The functions penorm21 and penorm31 return the L-1 norm for 2 and 3 dimensional vectors.

The functions penorm2infi and penorm3infi return the L-infinity norm for 2 and 3 dimensional vectors.

The functions penorm2 and penorm3 return the squared L-2 norms for 2 and 3 dimensional vectors.

The functions penorms2 and penorms3 return the L-2 norms for 2 and 3 dimensional vectors.}

\value{A vector of norms.}

\author{Mohit Dayal}

\examples{
##Compute the L-2 norm of vectors (1,3) and (2,5)
##penorms2(c(1,2),c(3,5))
}
