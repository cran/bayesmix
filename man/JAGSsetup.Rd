\name{JAGSsetup}
\alias{JAGSsetup}
\alias{JAGSsetup.default}
\alias{JAGSsetup.BMMsetup}
\title{Creation of necessary input files for jags}
\description{
  Given the model and the control specifications the input files (.bug,
  -data.R, -inits.R, .cmd) for jags are created with \code{prefix} as
  their names.
}
\usage{
JAGSsetup(model, y, prefix, control, ...)

\method{JAGSsetup}{default}(model, y, prefix, control, ...)
\method{JAGSsetup}{BMMsetup}(model, y, prefix, control, ...)
}
\arguments{
  \item{model}{\code{JAGSmodel} object or output from \code{BMMmodel}.}
  \item{y}{a numeric vector.}
  \item{prefix}{string: prefix for .bug, -data.R, -inits.R and .cmd-file.}
  \item{control}{named list or \code{JAGScontrol} object.}
  \item{\dots}{additional parameters handed over to \code{BMMmodel}.}
}
\details{
  \code{control} needs to have a component \code{text} which is written
  into the .cmd file after inserting the \code{prefix} where necessary. 

}
\value{
  After creating the files \code{prefix}"-data.R",
  \code{prefix}"-inits.R", \code{prefix}".bug" and \code{prefix}".cmd"
  it returns a list with the following components
  \item{control}{input argument \code{control} with \code{prefix}
    inserted in the component\code{text}.}
  \item{model}{\code{JAGSmodel} object}
}
\author{Bettina Gruen}
\seealso{\code{\link{BMMmodel}}, \code{\link{JAGScontrol}}}
\keyword{utilities}
