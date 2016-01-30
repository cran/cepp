\name{CvM}
\alias{ecdf.indices}
\alias{KS}
\alias{D1}
\alias{D2}

\title{Projection Pursuit Indices based on the bivariate empirical distribution function.}

\description{This function can be used to compute the projection pursuit
  indices described in Perisic and Posse (2005).}

\usage{
ecdf.indices(A, sphered = FALSE)
}

\arguments{
  \item{A}{The projected data.}
  \item{sphered}{Whether the data has already been sphered or not. If
  set to FALSE (default), the function will sphere the data before
  computing the indices.}
}

\details{
  The two-dimensional empirical distribution function is defined as,
  \deqn{F_n(x, y) = \frac{1}{n} \#\{(x_j, y_j): x_j \leq x \mbox{ and }
    y_j \leq y\}}

  The indices described in Perisic and Posse (2005) use this function to
  construct the following four indices.

  Cramer-von-Mises:   \deqn{\sum_i (F_n(x_i, y_i) -
    \Phi(x_i)\Phi(y_i))^2}
  
  Kolmogorov-Smirnov: \deqn{\max_i |F_n(x_i, y_i) - \Phi(x_i)\Phi(y_i)|}
  
  D2:                 \deqn{\sum_i (F_n(x_i, y_i) - F_n(y_i, x_i))^2}
  
  D-infinity:         \deqn{\max_i |F_n(x_i, y_i) - F_n(y_i, x_i)|}

  where \eqn{\Phi(.)} is the cumulative distribution function of the
  standard normal distribution.

  When using any of these indices, the original authors recommended
  rotating the data projection several times to obtain rotational
  invariance. In simulations, the indices performed well even without
  rotations.
}

\value{
A named numeric vector with the values of the following indices : the
Cramer-von-Mises index, the Kolmogorov-Smirnov index, the D2 Symmetry
index, and the D-infinity Symmetry index.
}

\author{Mohit Dayal}

\references{
Perisic, Igor, and Christian Posse. "Projection pursuit indices based on the empirical distribution function." Journal of Computational and Graphical Statistics 14.3 (2005).}
