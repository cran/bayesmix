\name{JAGScall}
\alias{JAGScall}
\title{Calls jags}
\description{
Given the \code{prefix} of the .cmd-file and the path to jags the program
jags is called in batch mode by inserting the .cmd-file.
}
\usage{
JAGScall(prefix, jags, quiet = FALSE)
}
\arguments{
  \item{prefix}{string: name of .cmd-file.}
  \item{jags}{string: path to jags.}
  \item{quiet}{logical indicating whether error messages should be ignored.}
}
\details{
  If \code{jags} is missing, it is assumed that the program
  jags is in the path such that it is called with jags.
}
\value{
  Returns the exit status.
}
\author{Bettina Gruen}
\seealso{\code{\link{JAGSrun}}}
\keyword{utilities}

