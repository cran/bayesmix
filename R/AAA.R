require(coda)
jags <- ifelse(.Platform$OS.type == "windows", "jags.exe", "jags")
jags <- system.file("exec", jags, package = "bayesmix")

if (file.exists(jags)) options(jags.exe = jags)

