\name{geodesic}
\alias{search_geodesic}

\title{Functions for geodesic search}

\description{This function provides an alternative way to optimize the
  projection index, by moving along a geodesic path.}

\usage{
search_geodesic(current, alpha = 1, index, max.tries = 5, n = 5)
}

\arguments{
  \item{current}{The starting projection.}
  \item{alpha}{Maximum distance to travel (currently ignored).}
  \item{index}{The projection index.}
  \item{max.tries}{Maximum number of failed attempts before giving up.}
  \item{n}{Number of random steps to take to find best direction.}
}

\details{The function \code{search_geodesic} finds only one basis at a time. The \code{caller} is a wrapper function that calls \code{search_geodesic} \code{bases} number of times.}

\value{Returns the basis found.}

\author{
The function has been copied as is from the \code{tourr} package.
}
