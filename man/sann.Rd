\name{ALPHA}
\alias{METHOD}
\alias{ALPHA}

\title{Global variables}

\description{These variables need to be initialized for some of the methods to work.}

\usage{
METHOD
ALPHA
}

\details{
These global variables are automatically initialized when the package is loaded.

The variables \code{METHOD} and \code{ALPHA} are used by the \code{basis_nearby} function and are useful only if you want to optimize the index via simulated annealing.

\code{ALPHA} can be set to any number in (0,1), with smaller values implying a more localized search. By default set to 0.75.

The \code{METHOD} variable controls how a new basis is created from an old one and can be set to either one of the strings "linear" or "geodesic". You should set it to geodesic if your index is rotationally invariant. Default value is 'geodesic'.
}

\references{
E.K. Lee, D. Cook, S. Klinke, and T. Lumley. Projection pursuit for exploratory supervised classification. Journal of
Computational and Graphical Statistics, 14(4):831-846, 2005}
