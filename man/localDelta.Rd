% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/treeScaling.R
\name{localDelta}
\alias{localDelta}
\title{localDelta}
\usage{
localDelta(tree, node, delta, rescale = TRUE)
}
\arguments{
\item{tree}{A tree of class phylo.}

\item{node}{A node number describing the clade(s) to be transformed.}

\item{delta}{The value or values of lambda by which to transform the specified clade(s).}
}
\description{
A function that transforms a tree according to lambda, or a portion of a tree according to lambda.
}
\keyword{internal}
