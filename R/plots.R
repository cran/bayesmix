BMMdiag <-
function(object, which = 1:2, variables, ask = interactive(), fct1, fct2, 
         xlim, ylim, auto.layout = TRUE, caption = NULL, main = "", ...) {  
  if (!(inherits(object, "jags") && inherits(object$model, "BMMmodel"))) 
    stop("Use only with 'jags' objects with model of class 'BMMmodel'.")
  if (!is.numeric(which) || any(which < 1) || any(which > 2)) 
    stop("`which' must be in 1:2")
  k <- object$model$data$k
  oldpar <- NULL
  on.exit(par(oldpar))
  oldpar <- par(ask = ask)
  show <- rep(FALSE, 2)
  show[which] <- TRUE
  if (missing(variables)) variables <- object$variables
  setxlim <- ifelse(missing(xlim), TRUE, FALSE)
  setylim <- ifelse(missing(ylim), TRUE, FALSE)
  vars <- variables[sapply(variables, function(x) length(grep(x, colnames(object$results))) <= k)]
  numVars <- length(vars)
  if (is.null(caption)) {
    caption <- c(sapply(vars, function(x) paste(x, "[k] versus ", vars, "[k]", sep = ""))
                 [lower.tri(matrix(nrow = numVars, ncol = numVars))],
                 paste(vars, "[k] versus ", vars ,"[l]", sep = ""))
  }
  if (show[1]) {
    if (numVars > 1) {
      if (auto.layout) oldpar <- c(oldpar, par(mfrow = c(1, numVars*(numVars-1)/2)))
      h <- 0
      for (i in 1:(numVars-1)) {
        kvar1 <- grep(vars[i], colnames(object$results))
        var1 <- matrix(object$results[,kvar1], ncol = length(kvar1))
        vars1 <- vars[i]
        if (!missing(fct1)) {
          var1 <- get(fct1)(var1)
          vars1 <- paste(fct1, "(", vars1, ")", sep = "")
        }
        if (setxlim) xlim <- range(var1)
        for (j in (i + 1):numVars) {
          h <- h + 1
          kvar2 <- grep(vars[j], colnames(object$results))
          if (length(kvar2) <= k) {
            vars2 <- vars[j]
            var2 <- matrix(object$results[,kvar2], ncol = length(kvar2))
            if (!missing(fct2)) {
              var2 <- get(fct2)(var2)
              vars2 <- paste(fct2, "(", vars2, ")", sep = "")
            }            
            if (setylim) ylim <- range(var2)
            plot(var1[,1], var2[,1], xlim = xlim,
                 ylim = ylim, xlab = vars1, ylab = vars2, main = main, ...)
            mtext(caption[h], 3, 0.25)
            h <- max(length(kvar1), length(kvar2))
            if (h > 1) {
              for (l in 2:h) points(var1[, min(l,length(kvar1))], var2[, min(l, length(kvar2))], ...)
            }
          }
        }
      }
    }
    else warning("The first plot option requires at least two variables.")
  }
  if (show[2]) {
    if (auto.layout) oldpar <- c(oldpar, par(mfrow = c(1, numVars)))
    for (l in 1:numVars) {
      varNam <- vars[l]
      kvar <- grep(varNam, colnames(object$results))
      if (length(kvar) > 1) {
        var <- matrix(object$results[,kvar], ncol = length(kvar))
        if (setxlim) xlim <- range(var)
        plot(xlim, xlim, type = "l", xlab = paste(varNam,"[k]", sep = ""), ylab = paste(varNam, "[l]", sep = ""),
             main = main, ...)
        mtext(caption[numVars*(numVars-1)/2+l], 3, 0.25)
        for (i in 1:(length(kvar)-1)) {
          for (j in (i+1):length(kvar)) {
            points(var[,i], var[,j], ...)
            points(var[,j], var[,i], ...)
          }
        }
      }
    }
  }
}

BMMposteriori <- function(object, class, caption = NULL, plot = TRUE, auto.layout = TRUE, ...) {
  if (!(inherits(object, "jags") && inherits(object$model, "BMMmodel"))) 
    stop("Use only with 'jags' objects with model of class 'BMMmodel'.")
  k <- object$model$data$k
  if (missing(class)) class <- 1:k
  if (is.null(caption)) caption <- paste("Group", class)
  S <- object$results[,grep("S", colnames(object$results))]
  if (dim(S)[2] == 0) stop("A posteriori plot not possible. Please provide class observations!")
  uniqPoints <- unique(object$data)
  n <- dim(object$results)[1]
  tab <- sapply(uniqPoints, function(x)
                table(factor(S[,object$data == x], levels = 1:k))/(n*length(which(object$data == x))))
  x <- list()
  x$post <- tab[class,]
  x$data <- uniqPoints
  class(x) <- "BMMposteriori"
  if (plot) {
    if (auto.layout) {
      oldpar <- par(mfrow = c(length(class), 1))
      on.exit(par(oldpar))
    }
    plot(x, caption, ...)
    invisible(x)
  }
  else x
}  

plot.BMMposteriori <- function(x, caption, main = "", ...) {
  if (!is.matrix(x$post)) x$post <- matrix(x$post, nrow = 1)
  for (i in 1:dim(x$post)[1]) {
    plot(x$data, x$post[i,], type = "h", xlab = "data", ylab = "a posteriori probability",
         ylim = c(0,1), main = main, ...)
    mtext(caption[i], 3, 0.25)
    points(x$data, x$post[i,], pch = 19, ...)
  }
}

# Plot method for jags objects adapted from plot.mcmc in package coda
# written by Martyn Plummer, Nicky Best, Kate Cowles, Karen Vines

plot.jags <-
function (x, variables = NULL, trace = TRUE, density = TRUE, 
                       smooth = TRUE, bwf, num, xlim, auto.layout = TRUE, ask = interactive(), ...)  
{
  if (inherits(x$model, "BMMmodel")) {
    if (is.null(variables)) {
      variables <- x$variables
      variables <- variables[sapply(variables, function(y) length(grep(y, colnames(x$results))) <= x$model$data$k)]
    }
    for (name in variables) {
      u <- x$results[,grep(name,colnames(x$results)), drop = FALSE]
      if (NCOL(u) > 0) {
        if (!missing(num)) {
          if (any(num > NCOL(u))) warning("num modified.")
          num <- num[num <= NCOL(u)]
          u <- u[,num, drop = FALSE]
        }
        oldpar <- NULL
        on.exit(par(oldpar))
        if (auto.layout) {
          mfrow <- coda:::set.mfrow(Nchains = nchain(u), Nparms = nvar(u), 
                                    nplots = trace + density)
          oldpar <- par(mfrow = mfrow)
        }
        oldpar <- c(oldpar, par(ask = ask))
        for (i in 1:nvar(u)) {
          y <- mcmc(as.matrix(u)[, i, drop = FALSE], start(u), end(u), thin(u))
          if (trace) 
            traceplot(y, smooth = smooth)
          if (density) {
            if (missing(xlim)) xl <- range(u)
            else xl <- xlim
            if (missing(bwf)) 
              densplot(y, xlim = xl, ...)
            else densplot(y, bwf = bwf, xlim = xl, ...)
          }
        }
      }
      else warning("Variable ", name, " omitted.")
    }
  }
  else plot(x$results)
}
