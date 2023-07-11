rm(list=ls(all=TRUE))

#setwd("C:/Users/EllenBradley/OneDrive - London School of Hygiene and Tropical Medicine/VN modelling/VN-bayesian-model")

# load functions
Rfiles <- list.files(file.path(paste0(getwd(),"/functions/")), ".R") #gets names of r files
sapply(paste0(paste0(getwd(),"/functions/"), Rfiles), source) #gets functions from file

# define dataset files                                                                                                                             
source("0.fileNames.R")
source("0.loadPackages.R")

#-----------------------#
#--1. create database --#

source("1.cleanAdminP2.R")
source("1.cleanSurveyP2.R")
source("1.combineAdminandSurvey2.R")
source("1.inclusionExclusion5.R")
source("1.dqc4.R")
source("1.groupingMGA3.R")


#------move to the model fitting in 3. fitModels.R
#That sets up the model data in the correct form for JAGs, sets up the covariates to be added into the models,
# then sets and runs the parameters for the model. Need to go in and set iterations etc.
# It calls a function which outputs the model data and the parameters from the model.
# It outputs the traceplots of the parameters.
source("3.fitModelsP_singMultDQC.R")

#-----4.countryRegionalAndGlobalRatesAndNumbers
#This takes the iteration values from the model to: 
# 1) outputs the country means for each country with data and
# then takes the 0.025, 0.5 and 0.975 quantiles to get the estimates
# 2) predicts for the countries without data for each iteration, 
# then takes the 0.025, 0.5 and 0.975 quantiles to get the estimates.
# 3) outputs these iteration values to their regional groups, 
# sum the numbers up to the regional/global level, then take the quantiles
# to get regional and global estimates.
# Outputs country, regional and global level estimates.
source("4.countryR&GratesandNumbers.R")


#----- 5. Plot countries 
#Takes the 195 country estimates from above and plots them alongside their
# input data.
# need to go in and run source("5. Plot countries (1 model) only.R") or the plots
# wont load.
source("5. Plot countries P (1 model) 1 plot.R")

#---- 6. subgroupsMetaP
#This is the meta-analysis of the subgroup data.
# Adds additional study data.
# Uses the latest year from each country, and applies the total pooled estimate
# to the regional and global point estimate of the preterm number.
# Also plots the subgroups by region.
source("6.subgroupsMetaP.R")


#---- 7. splinesModelValidation
#Reruns the full model and then with 20% less data inputs.
# Repeats this for a defined number of iterations and compares
# the two models. 
#Need to go into the file and set the iterations, and save a copy of the model .txt file
# with the same name but with "_modelVal" on the end.
source("7. splineModelsValidationP.R")