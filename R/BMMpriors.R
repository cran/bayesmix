priorsRaftery <- function(y) {
  para <- list()
  para$b0 <- mean(y)
  R <- diff(range(y))
  para$B0 <- 2.6/R^2
  para$nu0 <- 2.56
  para$S0 <- (length(y)-1)/length(y)*var(y)
  para
}

priorsFish <- function(y) {
  para <- list()
  para$b0 <- median(y)
  para$B0 <- 10
  para$nu0 <- 20
  para$S0 <- 0
  para
}

priorsUncertain <- function(y) {
  para <- list()
  para$b0 <- mean(y)
  para$B0 <- Inf
  para$nu0 <- 0
  para$S0 <- 0
  para
}
  
BMMpriors <- function(specification, y) {
  priors <- list()
  default <- list(kind = "independence", parameter = "priorsUncertain", hierarchical = NULL, mod = list()) 
  if (missing(specification)) specification <- default
  else {
    n <- names(specification)
    s <- names(default)
    p <- pmatch(n, s)
    if(any(is.na(p)))
      stop(paste("\nInvalid name(s) in specification :", paste(n[is.na(p)], collapse=" ")))
    names(specification) <- s[p]
    for (i in names(specification)) {
      default[[i]] <- specification[[i]]
    }
    specification <- default
  }
  priors$name <- match.arg(tolower(specification$kind), c("independence", "condconjugate"))
  y <- as.vector(y)
  parameter <- specification$parameter
  if (is.character(parameter)) {
    specification$parameter <- get(parameter)(y)
  }
  if (length(specification$mod) > 0) {
    nam <- names(specification$mod)
    for (i in 1:length(specification$mod)) {
      specification$parameter[[nam[i]]] <- specification$mod[[i]]
    }
  }
  var <- specification$parameter
  priors$var <- list(b0 = var$b0, B0inv = 1/var$B0,
                     nu0Half = var$nu0/2, nu0S0Half = var$nu0*var$S0/2)
  if (!is.null(specification$hierarchical)) {
    specification$hierarchical <- match.arg(tolower(specification$hierarchical), c("tau"))
    if (specification$hierarchical == "tau") {
      priors$name <- c(priors$name, "tau")
      names(priors$name) <- c("type", "hierarchical prior for")
      for (x in c("g0", "G0")) {
        if (!x %in% names(var)) {
          var[[x]] <- 0
        }
      }
      priors$var <- c(priors$var, list(g0Half = var$g0/2, g0G0Half = var$g0/2*var$G0))
      priors$var$S0 <- rep(NA, max(length(var$S0), length(var$g0), length(var$G0)))
      priors$var$nu0S0Half <- rep(NA, max(length(priors$var$nu0S0Half), length(priors$var$S0)))
    }
    else stop("Hierarchical method not supported")
  }
  class(priors) <- c("BMMpriors", "JAGSpriors")
  priors
}

