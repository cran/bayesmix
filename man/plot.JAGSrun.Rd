\name{plot.JAGSrun}
\alias{plot.JAGSrun}
\title{Plot JAGSrun object}
\description{
  Plots \code{mcmc} chains of a \code{JAGSrun} object.
}
\usage{
\method{plot}{JAGSrun}(x, variables = NULL, trace = TRUE, density = TRUE,
     smooth = TRUE, bwf, num, xlim, auto.layout = TRUE,
     ask = interactive(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{JAGSrun} object.}
  \item{variables}{names of variables which shall be plotted. Default
    are all names of results except those with a column dimension larger
    than the number of classes \code{k}.}
  \item{trace}{plot trace of each variable.}
  \item{density}{plot density estimate of each variable.}
  \item{smooth}{draw a smooth line through trace plots.}
  \item{bwf}{bandwidth function for density plots.}
  \item{num}{if not all classes of a variable shall be plotted, a subset
    can be specified.}
  \item{xlim}{if not specified, the range of each variable over all
    classes is taken as default.}
  \item{auto.layout}{automatically generate output format.}
  \item{ask}{prompt user before each page of plots.}
  \item{\dots}{further arguments for \code{densityplot}.}
}
\details{
  Adapted from \code{plot.mcmc}.

  Currently only implemented for \code{JAGSrun} objects with model of class
  \code{BMMmodel}. Otherwise the default plot method for the \code{results}
  of the \code{JAGSrun} object is called (\code{plot.mcmc}).
}
\author{Bettina Gruen}
\seealso{\code{\link[coda]{plot.mcmc}}, \code{\link{BMMdiag}},
  \code{\link{BMMposteriori}}}
\keyword{hplot}

