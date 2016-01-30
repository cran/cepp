\name{bases}
\alias{basis_nearby}
\alias{basis_random}

\title{Create random bases}

\description{Generate bases.}

\usage{
basis_random(n, d = 2)
basis_nearby(alpha = 0.75, method = 'geodesic', d = 2)
}

\arguments{
  \item{n}{The number of rows.}
  \item{d}{The number of columns.}
  \item{alpha}{How "far" away should the new matrix be generated?}
  \item{method}{How should be new matrix be found? One of \code{linear} or \code{geodesic}.}
}

\details{
\code{basis_random} returns a new orthonormal matrix of specified dimensions.

\code{basis_nearby} generates a function. Calling this function with a
matrix, hybridizes it with a new (randomly generated via
\code{basis_random}) orthonormal matrix, and returns it. 
}

\value{For \code{basis_random}, a random orthonormal matrix of specified
  dimensions.

For \code{basis_nearby}, a function that can be used to generate new
matrices "near" the current matrix.}

\author{Both functions were originally taken from the \code{tourr}
  package. The \code{basis_nearby} function was modified so the
  parameters \code{alpha} and \code{method} can be set more conveniently
  during optimization.}