haveJAGS <- function(jags = getOption("jags.exe")) {
  opt <- options("warn" = -1)
  on.exit(options(opt))
  dir <- getwd()
  tmpdir <- tempdir()
  if (!file.exists(tmpdir)){
    if (!dir.create(tmpdir)) stop("Error creating tmp directory")
  }
  setwd(tmpdir)
  if (file.exists("haveJAGS.cmd")) stop("Remove file 'haveJAGS.cmd' first.")
  write("exit", file = "haveJAGS.cmd")
  if (is.null(jags) || jags == "") jags <- "jags"
  if (.Platform$OS.type == "windows") exit <- system(paste(jags, "haveJAGS.cmd"))
  else  exit <- system(paste(jags, " < haveJAGS.cmd > /dev/null", sep = ""), ignore.stderr = TRUE)
  unlink("haveJAGS.cmd")
  setwd(dir)
  as.logical(!exit)
}
