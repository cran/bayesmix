\name{priors}
\alias{priorsFish}
\alias{priorsRaftery}
\alias{priorsUncertain}
\title{Creates list of prior specifications}
\description{Given the data values for the priors are determined.
}
\usage{
priorsFish(y)
priorsRaftery(y)
priorsUncertain(y)

}
\arguments{
  \item{y}{a numeric vector.}
}
\details{Values for the prior parameter \code{b0}, \code{B0}, \code{nu0}
  and \code{S0} are determined.
}
\value{
  There is a list returned with named components of the prior parameters.
}
\author{Bettina Gruen}
\keyword{utilities}

