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

#finalData<-readRDS("output/finalData.Rds")
finalPreterm<-readRDS("output/finalPreterm_MGA.RDS")

## JAGS parameters (can be changed if not converging - set to be relatively fast)

#Need to save an identical copy of the JAGS file with _modelVal so the models 
# dont rewrite.
model.file.path = paste0("models/",fileName, "_modelVal.txt")
#Leave out method is either random or recent
leave.out.method=c("random")
#Percentage of data left out of each country
leave.out.percent = 20
alpha.level = 0.05
alpha.n = 0.1
seed = 123
#Number of repeats the model validation runs for
nreps=1000

source("functions/runModelValidationP.R")

#Runs validation on the final model
modelValOutput<-runModelValidation2P(input.data=finalPreterm,  
                                    nchains = nchains, nburnin = nburnin, niter = niter, nthin = nthin,
                                    model.file.path = model.file.path, 
                                    leave.out.method=leave.out.method, 
                                    leave.out.percent = leave.out.percent, 
                                    nreps = nreps, 
                                    alpha.level = alpha.level, alpha.n = alpha.n, seed = seed)

#Outputs comparison statistics between the full and left out model.
table<-as.data.frame(list(reps=nreps,
                          RMSE=mean(modelValOutput$random$rmse),
                          MeanError=mean(modelValOutput$random$predE),
                          MeanAbsError=mean(modelValOutput$random$predEAbs),
                          desirableWithinPredCI=mean(modelValOutput$random$desinCI),
                          desirableAboveCI=mean(modelValOutput$random$desaboveCI),
                          desirableBelowCI=mean(modelValOutput$random$desbelowCI),
                          loWithinPredCI=mean(modelValOutput$random$inCI),
                          loAboveCI=mean(modelValOutput$random$aboveCI),
                          loBelowCI=mean(modelValOutput$random$belowCI),
                          predIn95=mean(modelValOutput$random$medInFCI),
                          predIn80=mean(modelValOutput$random$medInFCI80),
                          predIn50=mean(modelValOutput$random$medInFCI50),
                          DIC=mean(modelValOutput$random$dic)))

write.csv(table,
          paste0("output/" ,fileName,"_", 
                 ((niter-nburnin)/nthin)*nchains,"_modelValidation", nreps, ".csv"))

saveRDS(table, 
        paste0("output/" ,fileName,"_", 
               ((niter-nburnin)/nthin)*nchains,"_modelValidation", nreps, ".rds"))

