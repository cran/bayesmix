"JAGSrun" <-  function(y, prefix = yname,  model = BMMmodel(k = 2),
                       control = JAGScontrol(variables = c("mu", "tau", "eta")),
                       tmp = TRUE, cleanup = TRUE, jags = getOption("jags.exe"), ...) {
  yname <- deparse(substitute(y))
  if (!is.null(dim(y))) {
    if (dim(y)[1] == 1) y <- y[1,]
    else if (dim(y)[2] == 1) y <- y[,1]
    else stop("Only univariate response allowed")
  }
  y <- as.numeric(y)
  cl <- match.call()
  if (tmp) {
    dir <- getwd()
    tmpdir <- tempdir()
    if (!file.exists(tmpdir)){
      if (!dir.create(tmpdir)) stop("Error creating tmp directory")
    }
    setwd(tmpdir)
  }
  specification <- JAGSsetup(model, y, prefix, control, ...)
  exit <- JAGScall(prefix, jags)
  results <- JAGSread(exit)
  if (any(!is.finite(results$results))) {
    warning("Infinite values occured: These draws are omitted!")
    results$results <- as.mcmc(na.omit(results$results))
    if (dim(results$results)[1] == 0) results$results <- NULL
  }
  if (!exit) {
    if (cleanup) {
     unlink(c(paste(prefix, c(".cmd", ".bug","-inits.R", "-data.R", ".txt"), sep = ""), "jags.out", "jags.ind"))
    }
    if (tmp) setwd(dir)
  }
  z = list(call = cl, results = results$results, model = specification$model,
    variables = results$variables, data = y)
  class(z) <- "jags"
  z
}

"summaryShort.mcmc" <-
function (object, quantiles = c(0.025, 0.975), 
    ...) 
{
    x <- as.mcmc(object)
    statnames <- c("Mean", "SD")
    varstats <- matrix(nrow = nvar(x), ncol = length(statnames), 
        dimnames = list(varnames(x), statnames))
    if (is.matrix(x)) {
        xmean <- apply(x, 2, mean)
        xvar <- apply(x, 2, var)
        varquant <- t(apply(x, 2, quantile, quantiles))
    }
    else {
        xmean <- mean(x, na.rm = TRUE)
        xvar <- var(x, na.rm = TRUE)
        varquant <- quantile(x, quantiles)
    }
    varstats[, 1] <- xmean
    varstats[, 2] <- sqrt(xvar)
    varstats <- drop(varstats)
    varquant <- drop(varquant)
    out <- list(statistics = varstats, quantiles = varquant,
        start = start(x), end = end(x), thin = thin(x), nchain = 1)
    class(out) <- "summaryShort.mcmc"
    return(out)
}

"print.jags" <-
function(x, ...) {
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  if (inherits(x$model, "BMMmodel")) {
    if (is.null(x$results)) cat("No results!\n")
    else {
      cat("Markov Chain Monte Carlo (MCMC) output:\nStart =", start(x$results), 
          "\nEnd =", end(x$results), "\nThinning interval =", thin(x$results), 
          "\n")
      for (i in x$variables) {
        y <- x$results[,grep(i, colnames(x$results)), drop = FALSE]
        if(dim(y)[2] <=  x$model$data$k) {
          yout <- summaryShort.mcmc(y)
          class(yout) <- "summaryShort.mcmc"
          cat(paste("\n Empirical mean, standard deviation and 95% CI for", i, "\n"))
          print(yout, ...)
        }
      }
    }
  }
}

"print.summaryShort.mcmc" <-
function(x, digits = max(3, .Options$digits - 3), ...) {
  if (is.matrix(x$statistics)) {
    print(cbind(x$statistics, x$quantiles), digits = digits, ...)
  }
  else print(c(x$statistics, x$quantiles), digits = digits, ...)
}

.collapse <- function(text, prefix) {
  paste(text, collapse = prefix)
}