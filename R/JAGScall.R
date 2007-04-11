JAGSsetup <- function(model, y, prefix, control, ...) {
  UseMethod("JAGSsetup")
}

.Dump <- function(data, file) {
  names(data) <- sapply(names(data), function(x) paste("\"", x, "\"", sep=""))
  with(data, dump(names(data), file = file, control = list("quoteExpressions", "showAttributes", "useSource", "warnIncomplete")))
  ## in R 2.6 c(1,2) is dumped as 1:2
  data <- readLines(file)
  if (length(grep(":", data))) {
    for (i in  grep(":", data)) {
      Data <- strsplit(data[i], "")[[1]]
      where <- grep(":", Data)
      data[i] <- paste(c(Data[1:(where-2)],
                         paste("c(", paste(eval(parse(text = paste(Data[where + c(-1:1)], collapse = ""))), collapse = ", "), ")", sep = ""),
                         Data[(where+2):length(Data)]), collapse = "")
    }
    writeLines(data, con = file)
  }
}

JAGSsetup.default <- function(model, y, prefix, control, ...) {
  if (!inherits(model, "JAGSmodel")) stop("Only for use with 'JAGSmodel' objects!")
  .Dump(model$data, file = paste(prefix, "-data.R", sep = ""))
  if (!is.null(control$RNG)) model$inits <- c(model$inits, control$RNG)
  .Dump(model$inits, file = paste(prefix, "-inits.R", sep = ""))
  if (length(model$bugs) > 1) 
    model$bugs <- .collapse(model$bugs, prefix)
  write(model$bugs, file = paste(prefix,".bug", sep = ""))
  if (!any(names(control) %in% "text")) stop("control not specified correctly!")
  if (length(control$text) > 1)
    control$text <- .collapse(control$text, prefix)
  write(control$text, file = paste(prefix, ".cmd", sep = ""))
  return(list(control = control, model = model))
}

JAGSsetup.BMMsetup <- function(model, y, prefix, control, ...) {
  dummy <- model
  model <- list(k = 2, priors = BMMpriors(y = y), inits = "initsFS",
                aprioriWeights = 1, restrict = "none", no.empty.classes = FALSE)
  n <- names(dummy)
  s <- names(model)
  p <- pmatch(n, s)
  if(any(is.na(p)))
    stop(paste("\nInvalid name(s) in model :", paste(n[is.na(p)], collapse=" ")))
  names(dummy) <- s[p]
  for (i in names(dummy)) {
    model[[i]] <- dummy[[i]]
  }
  model <- BMMmodel(y, model$k, model$priors, model$inits,
                    model$aprioriWeights, model$no.empty.classes, model$restrict, ...)
  if (!inherits(model, "BMMmodel")) stop("Model not specified correctly")
  JAGSsetup(model, y, prefix, control)
}

JAGScall <- function(prefix, jags, quiet = FALSE) {
  if (is.null(jags)) jags = "jags"
  if (.Platform$OS.type == "windows") exit <- system(paste(jags, " ", prefix,".cmd", sep = ""))
  else  exit <- system(paste(jags, "< ",prefix,".cmd > /dev/null", sep = ""), ignore.stderr = quiet)
  if (exit) stop("System call not successfull")
  if (file.info("jags.out")[1] == 0) exit <- 1
  exit
}

JAGSread <- function(exit, transform = TRUE) {
  if (!exit) {
    if(!all(paste("jags" ,c("out", "ind"), sep = ".") %in% list.files()))
      stop("Cannot read jags output: .out or .ind file is missing!")
    results <- read.jags(quiet = TRUE)
    index <- grep("tau", colnames(results))
    variables <- unique(sapply(colnames(results), function(x) strsplit(x, "\\[")[[1]][1]))
    if (transform & length(index) > 0) {
      results[,index] <- 1/results[,index]
      colnames(results) <- sub("tau","sigma2", colnames(results))
      variables <- sub("tau", "sigma2", variables)
    }
  }
  else{
    results <- ifelse(file.exists("jags.dump"), source("jags.dump")[[1]], NULL)
    warning("Jags has encountered an error. Files are not deleted! Dump of jags will be returned.")
  }
  return(list(results = results, variables = variables))
}



