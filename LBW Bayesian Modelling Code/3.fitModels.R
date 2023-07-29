# library(tidyverse)
# library(devtools)
# devtools::install_github("MJAlexander/distortr")
# library(distortr)
# library(rjags)
# library(R2jags)
# library(fields)
# library(splines)
# library(boot)
# library(RColorBrewer)
# library(writexl)
# library(parallel)

source("functions/runMCMC2.R")
source("functions/runMCMCGlobal2.R")
source("functions/createArrayForStan.R")
#source("0.loadPackages.R")
#source("processData.R")
#source("functions/outputDataCovs2_2.R")
#-----
#Data setup
finalData<-readRDS("output/finalData.RDS")
finalData<-finalData %>% mutate(sourceIndex=ifelse(Source=="Admin", 1, 0),
                                newFlagN=ifelse(newFlag=="Ai"| newFlag=="Aii", 1,
                                                ifelse(newFlag==1 |newFlag=="Bi", 2,
                                                       ifelse(newFlag=="Bii", 3, 
                                                              ifelse(newFlag=="Ci", 4, 
                                                                     ifelse(newFlag=="Cii", 5, NA))))))
regionCodes<-readRDS("output/regionCodes.RDS")

d<-finalData
N<-dim(d)[1]

endyear <- 2020
startyear<-1995
estyears <- seq(startyear,endyear)

input.data <- processData(d = d, iso.column = "ISO",
                          data.column = "logitY",
                          se.column = "logitSe",
                          obsyear.column = "Year",
                          region.column = "regionIndex",
                          source.column="sourceIndex",
                          #source.column = "qualityHierarchy",
                          end.year = endyear,
                          start.year = startyear)

input.data2 <- processData(d = d, iso.column = "ISO",
                           data.column = "logitY",
                           se.column = "logitSe",
                           obsyear.column = "Year",
                           region.column = "regionIndex",
                           source.column="newFlagN",
                           #source.column = "qualityHierarchy",
                           end.year = endyear,
                           start.year = startyear)
input.data$newFlag<-input.data2$source.ci

#-----------------------------
#Adding the Covariates
source("2.covarArray2.R")

input.data$covar_array<-covar_array
input.data$ncov<-dim(covar_array)[1]

#-----------------------------

for(i in 1:length(input.data)){
  assign(names(input.data)[i], input.data[[i]])
}
isos <- unique(d[["ISO"]])

#------
#Model

#model parameters
cs.smoothing <- TRUE
nserror.estimated <- TRUE
input.data$sigma.y <- NA

## JAGS parameters
nchains = 4
nburnin = 1000
niter = 1200
nthin = 1

totalIts<-(niter-nburnin)/nthin #should be 2000 for each chain

# 3c. Splines order 1 -----------------------------------------------------
# P-splines with first order penalization has intercept but no time trend
# need additional input data

#No random time slopes
source("functions/getSplines_Old.R")
splines.data <- getSplinesData(26, order = 1, degree=3, I=155)

input.data.splines <- c(input.data, splines.data)
rm("input.data", "input.data2"
   #, "stanData"
   )

file<-"models/pCCFullModel.txt"
fileName<-substr(file, 8, as.numeric(gregexpr(".txt", file))-1)

#Running model
starttime<-Sys.time()
mod<-runMCMC2(method = "splines",
                                  order = 1,
                                  input.data = input.data.splines,
                                  cs.smoothing = cs.smoothing,
                                  nserror.estimated = nserror.estimated,
                                  nchains = nchains,
                                  nburnin = nburnin,
                                  niter = niter,
                                  nthin = nthin,
                                  model.file.path = file)
Sys.time()-starttime

#Outputting model diagnostics
pdf_name <- paste0("output/" ,fileName,"_", 
                   ((niter-nburnin)/nthin)*nchains,"_tracePlots", ".pdf")
pdf(pdf_name, width = 10, height = 5)
R2jags::traceplot(mod, varname=c("beta.d", "mu.beta",
                                 "tau.beta", "mu.beta.global",
                                 "tau.beta.global", "sigma.delta", 
                                 "tau.beta", "tau.delta",
                                 "alpha", "Z.tk", "delta.hc"), mfrow=c(3,3),
                  ask=FALSE)

#autocorr.plot(mod$BUGSoutput$sims.matrix[,84:88], lag=100, ask=TRUE)

dev.off()

summary<-as.data.frame(mod$BUGSoutput$summary)
max(effectiveSize(mod))

write.csv(mod$BUGSoutput$summary,
          paste0("output/" ,fileName,"_",
                 ((niter-nburnin)/nthin)*nchains,"_modelSummary", ".csv"))



