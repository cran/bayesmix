\name{haveJAGS}
\alias{haveJAGS}
\title{Checks the availability of jags}
\description{
  Given the location of the jags executable, it is checked if it can be
  successfully called.
}
\usage{
haveJAGS(jags = getOption("jags.exe"))
}
\arguments{
  \item{jags}{string indicating location of jags executable. If
    \code{NULL} it is assumed that jags is in the search path.}
}
\value{
  Returns a logical.
}
\author{Bettina Gruen}
\keyword{utilities}

