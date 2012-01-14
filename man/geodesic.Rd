\name{geodesic}
\alias{caller}
\alias{search_geodesic}

\title{Functions for geodesic search}

\description{These functions are for a geodesic search.}

\usage{
search_geodesic(current, alpha = 1, index, max.tries = 5, n = 5)
caller(start,index,n,bases)
}

\arguments{
  \item{current, start}{The starting projection.}

  \item{alpha}{Maximum distance to travel (currently ignored).}

  \item{index}{The projection index.}

  \item{max.tries}{Maximum number of failed attempts before giving up.}

  \item{n}{Number of random steps to take to find best direction. Can be a vector for \code{caller}.}

  \item{bases}{Total number of bases to find.}
}

\details{The function \code{search_geodesic} finds only one basis at a time. The \code{caller} is a wrapper function that calls \code{search_geodesic} \code{bases} number of times.}

\value{
\code{search_geodesic} returns the basis found.
\code{caller} returns a list of bases. The list may be shorter than specified if no better bases can be found.
}

\author{
The function has been copied as is from the \code{tourr} package.
}
