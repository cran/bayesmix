\name{JAGSrun}
\alias{JAGSrun}
\alias{print.jags}
\alias{jags}
\title{MCMC sampling of Bayesian models}
\description{
  Calls jags for MCMC sampling.
}
\usage{
JAGSrun(y, prefix = yname, model = BMMmodel(k = 2),
        control = JAGScontrol(variables = c("mu", "tau", "eta")), tmp = TRUE,
        cleanup = TRUE, jags = getOption("jags.exe"), ...)
}
\arguments{
  \item{y}{a numeric vector.}
  \item{prefix}{character prefix of files.}
  \item{model}{object of class \code{JAGSmodel} or output from \code{BMMmodel}.}
  \item{control}{specification of control by a \code{JAGScontrol} object.}
  \item{tmp}{logical: shall the files be written in a temporary directory.}
  \item{cleanup}{logical: shall the created files be removed.}
  \item{jags}{string indicating location of jags executable.}
  \item{yname}{a character string with the actual \code{y} argument name.}
  \item{...}{further parameters handed over to \code{BMMmodel} where it
    is used for the function specifiying the initial values, e.g.,
    \code{initsFS}.} 
}
\details{
  If an error occurs when runing jags, the created files are not
  removed. This function is a wrapper calling \code{JAGSsetup},
  \code{JAGScall} and \code{JAGSread}.
}
\value{
  Returns a \code{jags} object with components
  \item{call}{the matched call.}
  \item{results}{results read in from ``jags.out'' if run was successful
    or from ``jags.dump'' if an error occurred.}
  \item{model}{a \code{JAGSmodel} object.}
  \item{variables}{vector containing the names of the monitored variables.}
  \item{data}{a numeric vector.}
}
\author{Bettina Gruen}
\seealso{\code{\link{JAGSsetup}}, \code{\link{JAGScall}},
  \code{\link{JAGSread}}, \code{\link{BMMmodel}}, \code{\link{initsFS}}}
\examples{
data(fish)
prefix <- "fish"
variables <- c("mu","tau","eta")
k <- 3
modelFish <- BMMmodel(k = k, priors = list(kind = "independence",
                      parameter = "priorsFish", hierarchical = "tau"))
controlFish <- JAGScontrol(variables = c(variables, "S"), draw = 100, seed = 1)
## Installation of JAGS necessary for applying these functions
if (haveJAGS()) {
z1 <- JAGSrun(fish, prefix, model = modelFish, initialValues = list(S0 = 2),
              control = controlFish, cleanup = TRUE, tmp = FALSE)
zSort <- Sort(z1, "mu")
BMMposteriori(zSort)
}
data(darwin)
prefix <- "darwin"
k <- 2
modelDarwin <- BMMmodel(k = k, priors = list(kind = "independence",
                        parameter = "priorsUncertain"), aprioriWeights = c(1, 15),
                        no.empty.classes = TRUE, restrict = "tau")
## Installation of JAGS necessary for applying these functions
if (haveJAGS()) {
z2 <- JAGSrun(darwin, prefix, model = modelDarwin, control =
              JAGScontrol(variables = variables, draw = 3000, burnIn = 1000,
              seed = 1), cleanup = TRUE, tmp = FALSE)
plot(z2, variables = "mu")
}
}
\keyword{utilities}

