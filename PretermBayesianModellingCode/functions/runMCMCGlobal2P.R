runMCMCGlobal2P<-function (method, input.data, order = NULL, matern.cov = TRUE, 
                          cs.arma = NULL, cs.smoothing = TRUE, time.trend = FALSE, 
                          nserror.estimated = TRUE, nchains = 3, nburnin = 1000, niter = 2000, 
                          nthin = 1, model.file.path = NULL, model.save.file.path = "R/model.txt") 
{
  for (i in 1:length(input.data)) {
    assign(names(input.data)[i], input.data[[i]])
  }
  if (is.null(model.file.path)) {
    model.path.to.run <- model.save.file.path
  }
  else {
    model.path.to.run <- model.file.path
  }
  jags.data <- list(y.ci = y.ci, gett.ci = (gett.ci - startyear.c + 
                                              1), niso = niso, n.c = n.c, nyears.c = nyears.c, se.ci = se.ci, 
                    sigma.y = sigma.y, source.ci = source.ci, region.c = region.c, 
                    nregions = nregions, 
                    ncov = ncov, covar_array= covar_array,
                    MGA=MGA, singMult=singMult,
                    dataType=dataType
                    #, threshold=threshold
                    #,newFlag=newFlag
  )
  if (nserror.estimated) {
    jags.data$nsources <- nsources
  }
  if (method == "ar") {
    if (is.null(cs.arma)) {
      stop("Need to specify cs.arma.")
    }
    if (is.null(model.file.path)) {
      writeModelAR(cs.arma = cs.arma, cs.smoothing = cs.smoothing, 
                   time.trend = time.trend, nserror.estimated = nserror.estimated, 
                   file.name = model.save.file.path)
    }
    parnames <- c("mu.ct", "loglike.ci", "yrep.ci", "beta", 
                  "sigma", "rho", "sigma.y", "mu.beta", "sigma.beta", 
                  "mu.beta.global", "sigma.beta.global")
    if (time.trend) {
      parnames <- c(parnames, "gamma", "mu.gamma", "sigma.gamma", 
                    "mu.gamma.global", "sigma.gamma.global")
    }
    if (cs.arma) {
      parnames <- c(parnames, "mu.rho", "sigma.rho", "mu.rho.global", 
                    "sigma.rho.global")
    }
    if (cs.smoothing) {
      parnames <- c(parnames, "mu.logsigma", "sigma.logsigma", 
                    "mu.logsigma.global", "sigma.logsigma.global")
    }
  }
  if (method == "arma") {
    if (is.null(cs.arma)) {
      stop("Need to specify cs.arma.")
    }
    if (is.null(model.file.path)) {
      writeModelARMA(cs.arma = cs.arma, cs.smoothing = cs.smoothing, 
                     time.trend = time.trend, nserror.estimated = nserror.estimated, 
                     file.name = model.save.file.path)
    }
    parnames <- c("mu.ct", "loglike.ci", "yrep.ci", "beta", 
                  "eta", "rho", "theta", "sigma.y", "mu.beta", "sigma.beta", 
                  "mu.beta.global", "sigma.beta.global")
    if (time.trend) {
      parnames <- c(parnames, "gamma", "mu.gamma", "sigma.gamma", 
                    "mu.gamma.global", "sigma.gamma.global")
    }
    if (cs.arma) {
      parnames <- c(parnames, "mu.rho", "sigma.rho", "mu.rho.global", 
                    "sigma.rho.global", "mu.theta", "sigma.theta", 
                    "mu.theta.global", "sigma.theta.global")
    }
    if (cs.smoothing) {
      parnames <- c(parnames, "mu.eta", "sigma.eta", "mu.eta.global", 
                    "sigma.eta.global")
    }
  }
  if (method == "splines") {
    if (is.null(order)) {
      stop("Order of penalization must be specified.")
    }
    if (is.null(model.file.path)) {
      writeModelSplines(order = order, cs.smoothing = cs.smoothing, 
                        nserror.estimated = nserror.estimated, file.name = model.save.file.path)
    }
    if (order == 1) {
      # jags.data$Z.tkc <- Z.tkc
      # jags.data$H.c <- H.c
      # jags.data$H <- H
      jags.data$Z.tk <- Z.tk
      jags.data$H <- H
    }
    if (order == 2) {
      jags.data$Z.tkc <- Z.tkc
      jags.data$BG.tdc <- BG.tdc
      jags.data$H.c <- H.c
      jags.data$H <- H
      jags.data$D <- 2
    }
    parnames <- c("mu.ct", "loglike.ci", "yrep.ci", "beta.d", "delta.hc",
                  "sigma.delta", "sigma.y", "mu.beta", "sigma.beta", 
                  "mu.beta.global", "sigma.beta.global", "alpha", "alpha_tilde", 
                  "cov", "bias_dt_i", "sigma.y2", "Z.tk", "H","Z.tkc", "tau.delta",
                  "bias_s", "sigma1", "sigma2", "sigma3", "sigma4", "sigma5", "sigma6", "sigma7", 
                  "bias1", "bias2", "bias3", "tau.beta", "randCov", "sigmaM1", "sigmaM2", "sigmaM3", "bias1",
                  "bias2","bias3","bias4","bias5", "tBias1", "tBias2")
    if (cs.smoothing) {
      parnames <- c(parnames, "chi.delta", "psi.delta", 
                    "mu.chi.delta", "sigma.chi.delta")
    }
  }
  if (method == "gp") {
    if (is.null(model.file.path)) {
      if (matern.cov == TRUE) 
        cov.fun <- "matern"
      if (matern.cov == FALSE) 
        cov.fun <- "sqexp"
      writeModelGP(cov.fun = cov.fun, time.trend = time.trend, 
                   cs.smoothing = cs.smoothing, nserror.estimated = nserror.estimated, 
                   file.name = model.save.file.path)
    }
    if (matern.cov == TRUE) {
      jags.data$Sigma.corr <- Sigma.corr.c
    }
    if (matern.cov == FALSE) {
      jags.data$Dist <- Dist.c
      jags.data$kappa <- 2
    }
    parnames <- c("G", "loglike.ci", "yrep.ci", "beta", 
                  "sigma.g", "sigma.y", "mu.beta", "sigma.beta", "mu.beta.global", 
                  "sigma.beta.global", "alpha", "alpha_tilde", "cov")
    if (time.trend) {
      parnames <- c(parnames, "gamma", "mu.gamma", "sigma.gamma", 
                    "mu.gamma.global", "sigma.gamma.global")
    }
    if (cs.smoothing) {
      parnames <- c(parnames, "chi", "sigma.psi", "chi.global", 
                    "sigma.chi.global")
    }
    if (matern.cov == FALSE) {
      parnames <- c(parnames, "p", "mu.p", "sigma.p", 
                    "mu.p.global", "sigma.p.global")
    }
  }
  if (nserror.estimated) {
    jags.data[["sigma.y"]] <- NULL
  }
  
  inits<-list()
  for (i in 1:length(parnames)){
    name<-paste0(parnames[1])
    inits[1]<-list(name=0)
  }
  
  cat("Running model.\n")
  cat(paste0("Total iterations: ",niter))
  cat(paste0(", Burn in: ",nburnin))
  cat(paste0(", Thinning: ",nthin,"\n"))
  #Not parallel but progress shown
  # mod <- jags(data = jags.data, parameters.to.save = c(parnames), 
  #             n.chains = nchains, n.burnin = nburnin, n.iter = niter, 
  #             n.thin = nthin, model.file = model.path.to.run)
  #Parallel but no progress shown
  mod <- jags.parallel(data = jags.data, parameters.to.save = c(parnames), 
              n.chains = nchains, n.burnin = nburnin, n.iter = niter, 
              n.thin = nthin, model.file = model.path.to.run)
  if (max(mod$BUGSoutput$summary[, c("Rhat")]) > 1.1) {
    cat("Something hasn't converged HAS IT.\n")
    cat(paste("Max Rhat is", max(mod$BUGSoutput$summary[, 
                                                        c("Rhat")]), "\n"))
  }
  #return(mod)
  
  cat("saving to RDS.\n")
  saveRDS(mod, paste0("output/", 
                      substr(model.file.path, 1, 
                             as.numeric(gregexpr(".txt", model.file.path))-1),
                      ((niter-nburnin)/nthin)*nchains, ".RDS"))
  cat("returning the model \n")
  
  cat("finished modelling!")
  return(mod)

}
