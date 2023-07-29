# library(tidyverse)
# #devtools::install_github("MJAlexander/distortr")
# library(distortr)
# library(rjags)
# library(R2jags)
# library(fields)
# library(splines)
# library(boot)
# library(RColorBrewer)
# library(writexl)

source("functions/runMCMC2.R")
source("functions/runMCMCGlobal2.R")
source("functions/createArrayForStan.R")
source("0.loadPackages.R")
#------------------------
#ADD IN THE CRUDE DATA
finalData<-readRDS("output/finalData.RDS")

surveys<-finalData %>% filter(Source=="Survey")
crudeSurveys<-surveys %>% 
  mutate(y=Crude_LBW_point_est/100,
         se=ifelse(is.na(Crude_LBW_se), LBW_adjusted_se, Crude_LBW_se),
         logitY=qlogis(Crude_LBW_point_est/100),
         logitSe=sqrt(se^2*(1/(y-y^2))^2),
         Source="CrudeSurvey")

#REMOVING THE LATEST CHINA POINT AS THERE IS NO CRUDE PROVIDED
allSurveys<-rbind(surveys, crudeSurveys) %>% 
  filter(isoYear!="CHN2015")

allSurveys<-allSurveys %>% mutate(sourceIndex=ifelse(Source=="Survey", 1, 0),
                                  newFlagN=ifelse(newFlag==1 | newFlag=="Ai", 1,
                                                  ifelse(newFlag=="Bi", 2,
                                                         ifelse(newFlag=="Bii", 3, 
                                                                ifelse(newFlag=="Ci", 4, 
                                                                       ifelse(newFlag=="Cii", 5, NA))))))





regionCodes<-readRDS("output/regionCodes.RDS")

d<-allSurveys
N<-dim(d)[1]

startyear<-1995
endyear <- 2020
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
#Covariates
#set the covariates wanted in covarArray.R first. 
#file.edit("2.covarArray.R")

#Rfiles <- list.files(file.path(paste0(getwd(),"/functions/")), ".R") #gets names of r files
#sapply(paste0(paste0(getwd(),"/functions/"), Rfiles), source) #gets functions from file

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
cs.smoothing <- TRUE
nserror.estimated <- TRUE
input.data$sigma.y <- NA

## JAGS parameters (can be changed if not converging - set to be relatively fast)
#a) Convergence part
nchains = 4
nburnin = 100000
niter =   6350000 #2500 x 2500 - because only keeping every 2500 of them + 100,000 burnin
nthin = 2500
#b)
# nchains = 4
# nburnin = 100000
# niter = 200000
# nthin = 50
#c)
# nchains = 4
# nburnin = 10000
# niter = 12000
# nthin = 10

# nchains = 4
# nburnin = 100000
# niter = 300000
# nthin = 100

# nchains = 4
# nburnin = 20000
# niter =   22500
# nthin = 1

# 3c. Splines order 1 -----------------------------------------------------
# P-splines with first order penalization has intercept but no time trend
# need additional input data

#No random time slopes
source("functions/getSplines_Old.R")
splines.data <- getSplinesData(26, order = 1, degree=3, I=155)

#Random time slopes
#source("functions/getSplines.R")
#splines.data <- getSplinesData(nyears.c, niso, order = 1)

input.data.splines <- c(input.data, splines.data)
rm("input.data", "input.data2",
   "stanData")

file<-"models/pCCFullModel_biasInform2.txt"
fileName<-substr(file, 8, as.numeric(gregexpr(".txt", file))-1)
mod2<-runMCMC2(method = "splines",
                                        order = 1,
                                        input.data = input.data.splines,
                                        cs.smoothing = cs.smoothing,
                                        nserror.estimated = nserror.estimated,
                                        nchains = nchains,
                                        nburnin = nburnin,
                                        niter = niter,
                                        nthin = nthin,
                                        model.file.path = file, save.pdf=FALSE, adapt=FALSE)

summary<-as.data.frame(mod2$BUGSoutput$summary)

write.csv(mod2$BUGSoutput$summary,
          paste0("output/" ,fileName,"_", 
                 ((niter-nburnin)/nthin)*nchains,"_modelSummary", ".csv"))
outputDataCovs2(mod2, finalData, "splines", 
                niter,nburnin, nchains, isos)
