rm(list=ls(all=TRUE))

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

#-----5.otherRegionalGroupings
#This is the same code as above which produces the regional and global 
# groupings. You need to go into the code and pick from the list which 
# regional grouping to produce.
#This sometimes uses a lot of space on the machine you're using. 
# If this is a problem, clear your workspace and un-comment the code at the 
# top of the script and rerun.
#source("5.otherRegionalGroupings.R")


#---- 6. subgroupsMetaP
#This is the meta-analysis of the subgroup data.
# Adds additional study data.
# Uses the latest year from each country, and applies the total pooled estimate
# to the regional and global point estimate of the preterm number.
# Also plots the subgroups by region.
#If you have clearer your workspace above, then un-comment lines 79 and 80 in 
# this code to rerun some of the initial raw survey data cleaning.
source("6.subgroupsMetaP.R")

