\name{JAGSread}
\alias{JAGSread}
\title{Reads jags output into R}
\description{
  Given an exit status of 0 when running jags the results are read in
  with \code{read.jags}
}
\usage{
JAGSread(exit, transform = TRUE)
}
\arguments{
  \item{exit}{exit status returned by \code{JAGScall}.}
  \item{transform}{logical: if \code{TRUE} and possible, the variable
    \eqn{\tau} is transformed into \eqn{\sigma^2}.}  
}
\details{
  After a successful run of jags the results are read in from the file
  "jags.out". If the exit status is positive the file "jags.dump" is
  read in and a warning is given. 
}
\value{
  A list with the following components is returned:
  \item{results}{MCMC chains.}
  \item{variables}{new variable names.}
}
\author{Bettina Gruen}
\seealso{\code{read.jags}, \code{\link{JAGSrun}}}
\keyword{utilities}

