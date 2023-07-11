runMCMC2P<-function (input.data, method, nyears = NULL, obs.err = TRUE, 
                    measurement.err = TRUE, cs.arma = NULL, cs.smoothing = TRUE, 
                    time.trend = FALSE, nserror.estimated = TRUE, order = NULL, 
                    I = 2.5, matern.cov = TRUE, nchains = 3, nburnin = 1000, 
                    niter = 2000, nthin = 1, model.file.path = NULL, 
                    model.save.file.path = "R/model.txt", save.pdf=TRUE, adapt=FALSE) 
{
  if (is.null(dim(input.data))) {
    mod <- runMCMCGlobal2P(method = method, input.data = input.data, 
                          order = order, matern.cov = matern.cov, cs.arma = cs.arma, 
                          cs.smoothing = cs.smoothing, time.trend = time.trend, 
                          nserror.estimated = nserror.estimated, nchains = nchains, 
                          nburnin = nburnin, niter = niter, nthin = nthin, 
                          model.file.path = model.file.path, model.save.file.path = model.save.file.path)
  }
  if (length(dim(input.data)) == 2) {
    mod <- runMCMCCountry(df = input.data, nyears = nyears, 
                          method = method, order = order, I = I, matern.cov = matern.cov, 
                          obs.err = obs.err, measurement.err = measurement.err, 
                          nchains = nchains, nburnin = nburnin, niter = niter, 
                          nthin = nthin, model.file.path = model.file.path)
  }
  
  #saveRDS(out, paste0("output/", model.file.path, ".RDS"))
  return(mod)
}
