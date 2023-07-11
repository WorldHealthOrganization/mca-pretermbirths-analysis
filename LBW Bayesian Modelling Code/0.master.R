rm(list=ls(all=TRUE))

# load functions
Rfiles <- list.files(file.path(paste0(getwd(),"/functions/")), ".R") #gets names of r files
sapply(paste0(paste0(getwd(),"/functions/"), Rfiles), source) #gets functions from file

# define dataset files                                                                                                                             
source("0.fileNames.R")
source("0.loadPackages.R")

#-----------------------#
#--1. create database --#

source("1.cleanAdmin3.R")
source("1.cleanSurvey.R")
source("1.combineDataAddingRegions.R")
allDataFinal<-allDataLBW
source("1.applyingInclusionCriteria.R")
source("1.dqc5.R")

#-----getting the bias terms
#source(3.fitModels_biasInform.R)

#Check that the estimates line is sitting on the adjusted surveys
#for the DQC: Bi and DQC: Ci countries.
#source(5. Plot countries (1 model) only_biasInform.R)

#------move to the model fitting in 3. fitModels.R
#That sets up the model data in the correct form for JAGs, 
# sets up the covariates to be added into the models,
# then sets and runs the parameters for the model. 
#Before running adjust the number of iterations.
source("3.fitModels.R")

#-----4.countryRegionalAndGlobalRatesAndNumbers
#This takes the iteration values from the model to: 
# 1) outputs the country means for each country with data and
#then takes the 0.025, 0.5 and 0.975 quantiles to get the estimates
# 2) predicts for the countries without data for each iteration, 
#then takes the 0.025, 0.5 and 0.975 quantiles to get the estimates.
# 3) outputs these iteration values to their regional groups, 
# sum the numbers up to the regional/global level, then take the quantiles
#to get regional and global estimates.
source("4.countryRegionalAndGlobalRatesAndNumbers.R")


#----- 5. Plot countries 
#Takes the 195 country estimates from above and plots them alongside their
# input data.
#need to go in and run source("5. Plot countries (1 model) only.R") or the plots
# wont load.

#---6. subgroups
#This takes the latest year of each of the country's subgroup data
# and puts this into a random-effects meta-analysis. It applies
# the global pooled estimate to the regional LBW estimate numbers
# to get the subgroup estimate numbers for each region.
source("6.subgroupMeta.R")

#----7. splineModelsValidation
#Calls a function which fits the model to both the full database, 
# and leave-one-out subsection of the database. 
# It outputs comparisons between these model fits. 
# You need to make a copy of your model file and call it
# "<insert file name>_modelVal.txt".
source("7. splineModelsValidation.R")

#---8. Numbers for paper
#This outputs the tables for the paper. 
# The plots are generated for user to save. 
# Number for papers are created.
source("8.Figures for paper.R")
source("8.Numbers for paper.R")
source("8.LBWinclusionExclusionFlow.R")
