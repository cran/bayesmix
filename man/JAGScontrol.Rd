\name{JAGScontrol}
\alias{JAGScontrol}
\alias{print.JAGScontrol}
\title{Creates text for .cmd file}
\description{
  Creates the text for the file where all the commands for jags are included.
}
\usage{
JAGScontrol(variables, draw = 1000, burnIn = 0, seed, rng =
            c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
            "base::Super-Duper", "base::Mersenne-Twister"))
}
\arguments{
  \item{variables}{names of variables which shall be monitored.}
  \item{draw}{number of monitored draws.}
  \item{burnIn}{number of discarded burn-in draws.}
  \item{seed}{integer setting the seed for the RNG.}
  \item{rng}{specification of random number generator.}
}
\details{This function creates the text for the .cmd-file. It includes
  information on the number of burn-in draws and monitored
  draws. Furthermore, it states which variables shall be monitored and
  it possibly specifies a seed. The information on the prefix of the
  files containing model, data and initial value is missing. By
  inserting the prefix between the elements of the vector containing the
  text a .cmd-file can be created which can be
  used for calling jags in batch mode. 
}
\value{
  An object of class \code{JAGScontrol} is returned which is a list
  containing the following components: 
  \item{text}{a vector of strings which contains all
    commands for running jags. For creating a .cmd-file the prefix of
    the files has to be inserted.}
  \item{variables}{a vector containing the names of the monitored
    variables.} 
}
\author{Bettina Gruen}
\seealso{\code{\link{JAGSrun}}}
\examples{
control <- JAGScontrol(variables = "mu")
control
}
\keyword{utilities}

