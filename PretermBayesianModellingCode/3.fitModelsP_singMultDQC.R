library(tidyverse)
#devtools::install_github("MJAlexander/distortr")
library(distortr)
library(rjags)
library(R2jags)
library(fields)
library(splines)
library(boot)
library(RColorBrewer)
library(writexl)

source("functions/runMCMC2P.R")
source("functions/runMCMCGlobal2P.R")
source("0.loadPackages.R")

#-----
#Data setup
finalPreterm<-readRDS("output/finalPreterm_MGA.RDS")
finalPreterm<-finalPreterm %>% mutate(sourceIndex=ifelse(category %in% c("A"), 1,
                                                         ifelse(category %in% c("B"), 2,
                                                                ifelse(category %in% c("C"), 3, 
                                                                       ifelse(category=="D", 4, 
                                                                              ifelse(category=="E" & dataType %in% c(1,2), 5, 
                                                                                     ifelse(category=="E" & dataType==3,6, NA))))))) %>% 
  filter(Year>=2010 & Year<=2020) %>% 
  mutate(dataType=ifelse(is.na(dataType) | dataType==1, 0, dataType))

regionSummary<-finalPreterm %>% group_by(sourceIndex, regionName) %>% 
  summarise(n=n(), nC=length(unique(ISO)))


regionCodes<-readRDS("output/regionCodes.RDS")

d<-finalPreterm
N<-dim(d)[1]

startYear<-2010
endyear <- 2020
estyears <- seq(startYear,endyear)

input.data <- processData(d = d, iso.column = "ISO",
                          data.column = "logitY",
                          se.column = "logitSe",
                          obsyear.column = "Year",
                          region.column = "regionIndex",
                          source.column="sourceIndex",
                          end.year = endyear,
                          start.year = startYear)

input.data2 <- processData(d = d, iso.column = "ISO",
                           data.column = "singMult",
                           se.column = "logitSe",
                           obsyear.column = "dataType",
                           region.column = "regionIndex",
                           source.column="methodGAGroup",
                           #source.column = "dataType",
                           end.year = endyear,
                           start.year = 2000)
input.data$MGA<-input.data2$source.ci
input.data$singMult<-input.data2$y.ci
input.data$dataType<-input.data2$gett.ci
#-----------------------------
#Covariates
#set the covariates wanted in covarArray.R first. 
#Make sure the latest LBW model is input here!
source("functions/createArrayForStan.R")
lbwEstimates<-"inputs/finalLBWEstimates.csv"
source("2.covarArrayP.R")

input.data$covar_array<-covar_array
input.data$ncov<-1

#-----------------------------

for(i in 1:length(input.data)){
  assign(names(input.data)[i], input.data[[i]])
}
isos <- unique(d[["ISO"]])

#------
#Model
cs.smoothing <- TRUE
nserror.estimated <- TRUE
input.data$sigma.y <- NA

## JAGS parameters (can be changed if not converging - set to be relatively fast)
nchains = 4
nburnin = 100000
nthin = 25
niter = (nthin*4000)+nburnin

(niter-nburnin)/nthin

# 3c. Splines order 1 -----------------------------------------------------
# P-splines with first order penalization has intercept but no time trend
# need additional input data

#No random time slopes
source("functions/getSplines_Old.R")
splines.data <- getSplinesData(11, order = 1, degree=3, I=155)

input.data.splines <- c(input.data, splines.data)
rm("input.data"
   , "input.data2"
   #,"stanData"
)

file<-"models/pCCFullModelP_dataType1DQC1_TEST.txt"
fileName<-substr(file,8, as.numeric(gregexpr(".txt", file))-1)
starttime<-Sys.time()

#Running the model
mod<-runMCMC2P(method = "splines",
               order = 1,
               input.data = input.data.splines,
               cs.smoothing = cs.smoothing,
               nserror.estimated = nserror.estimated,
               nchains = nchains,
               nburnin = nburnin,
               niter = niter,
               nthin = nthin,
               model.file.path = file, save.pdf=FALSE, adapt=FALSE)
Sys.time()-starttime

#Outputting the model diagnostics
pdf_name <- paste0("output/" ,fileName,"_", 
                   ((niter-nburnin)/nthin)*nchains,"_tracePlots", ".pdf")
pdf(pdf_name, width = 10, height = 5)
R2jags::traceplot(mod, varname=c("beta.d", "mu.beta",
                                 "tau.beta", "mu.beta.global",
                                 "tau.beta.global", "sigma.delta", 
                                 "tau.beta", "tau.delta",
                                 "alpha", "Z.tk", "delta.hc"), mfrow=c(3,3),
                  ask=FALSE)

dev.off()

summary<-as.data.frame(mod$BUGSoutput$summary)
max(effectiveSize(mod))

write.csv(mod$BUGSoutput$summary,
          paste0("output/" ,fileName,"_",
                 ((niter-nburnin)/nthin)*nchains,"_modelSummary", ".csv"))

#Model Coeff
mod[["BUGSoutput"]][["median"]][["alpha"]]
