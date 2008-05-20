JAGScontrol <-
function(variables, draw = 1000, burnIn = 0, seed,
         rng = c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper", "base::Mersenne-Twister")) {
  rng <- match.arg(rng)
  text = NULL
  text <- paste(text,"model in \"", sep = "")
  text[2] <- ".bug\"\ndata in \""
  text[3] <- "-data.R\"\ncompile\ninits in \""
  text[4] <- "-inits.R\"\ninitialize\n"
  text[4] <- paste(text[4],"update ",burnIn,"\n", sep = "")
  text[4] <- paste(text[4], paste("monitor ",variables,"\n", sep = "", collapse = ""), sep = "")
  text[4] <- paste(text[4],"update ",draw,"\ncoda *\nexit\n", sep = "")
  z <- list()
  z$text <- text
  z$variables <- variables
  if (!missing(seed)) z$RNG <- list(".RNG.name" = rng,
                                    ".RNG.seed" = as.integer(seed))
  class(z) <- "JAGScontrol"
  z
}

print.JAGScontrol <- function(x, prefix = "jags", ...) {
  cat("Commands for JAGS:\n\n")
  cat(paste(x$text, collapse = prefix))
}
