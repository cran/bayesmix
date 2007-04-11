"JAGScontrol" <-
function(variables, draw = 1000, burnIn = 0, seed) {
  text <- if (missing(seed)) NULL else paste("seed ", seed, "\n", sep = "")
  text <- paste(text, "model in \"", sep = "")
  text[2] <- ".bug\"\ndata in \""
  text[3] <- "-data.R\"\ncompile\ninits in \""
  text[4] <- "-inits.R\"\ninitialize\n"
  if (burnIn > 0) text[4] <- paste(text[4], "update ", burnIn,"\n", sep = "")
  text[4] <- paste(text[4], paste("monitor set ", variables,"\n", sep = "", collapse = ""), sep = "")
  text[4] <- paste(text[4],"update ", draw, "\ncoda *\nexit\n", sep = "")
  z <- list()
  z$text <- text
  z$variables <- variables
  class(z) <- "JAGScontrol"
  z
}

print.JAGScontrol <- function(x, prefix = "jags", ...) {
  cat("Commands for JAGS:\n\n")
  cat(paste(x$text, collapse = prefix))
}
