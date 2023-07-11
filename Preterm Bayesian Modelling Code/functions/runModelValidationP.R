#NEED TO UPDATE MODEL INPUT FROM THE splineModels

runModelValidation2P<-function(input.data,  
                               nchains = 4, nburnin = 1000, niter = 1000 + 30000, nthin = 30,model.file.path = "model_splines1_2NT_NF10_12.txt",
                               leave.out.method="random", leave.out.percent = 20, 
                               nreps = 10, alpha.level = 0.05, alpha.n = 0.1, seed = 123) 
{
  
  library(tidyverse)
  #devtools::install_github("MJAlexander/distortr")
  #library(distortr)
  library(rjags)
  library(R2jags)
  library(fields)
  library(splines)
  library(boot)
  library(RColorBrewer)
  library(writexl)
  
  df <- input.data
  df<-df %>% mutate(sourceIndex=ifelse(category %in% c("A"), 1,
                                       ifelse(category %in% c("B"), 2,
                                              ifelse(category %in% c("C"), 3, 
                                                     ifelse(category=="D", 4, 
                                                            ifelse(category=="E" & dataType %in% c(1,2), 5, 
                                                                   ifelse(category=="E" & dataType==3,6, NA))))))) %>% 
    filter(Year>=2010 & Year<=2020) %>% 
    mutate(dataType=ifelse(is.na(dataType) | dataType==1, 0, dataType))
  regionCodes<-readRDS("output/regionCodes.RDS")
  
  NIsos<-length(unique(df$ISO))
  N<-nrow(df)
  df$row<-1:N
  endyear <- 2020
  startYear<-2010
  estyears <- seq(startYear,endyear)
  
  
  rm(input.data)
  
  
  #-----INPUT DATA
  #Full Data
  d<-df
  N<-dim(d)[1]
  #library(distortr)
  
  input.data_full <- processData(d = d, iso.column = "ISO",
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
  input.data_full$MGA<-input.data2$source.ci
  input.data_full$singMult<-input.data2$y.ci
  input.data_full$dataType<-input.data2$gett.ci
  

  rm(input.data2, df)
  source("functions/createArrayForStan.R")
  #Rfiles <- list.files(file.path(paste0(getwd(),"/functions/")), ".R") #gets names of r files
  #sapply(paste0(paste0(getwd(),"/functions/"), Rfiles), source) #gets functions from file
  
  lbwEstimates<-"inputs/finalLBWEstimates.csv"
  
  int_cov <- c("logLBW", "logLBWSe")
  
  covarset<-read.csv(lbwEstimates) %>% 
    dplyr::select(ISO, year, LBW_2020_estimate=est, LBW_lower=estL) %>% 
    mutate(logLBW=log(LBW_2020_estimate),
           #logLBWSe=sqrt(LBW_sd*(1/LBW_2020_estimate^2))
           #logLBWSe=sd(logLBW, na.rm=TRUE),
           logLBWSe=(log(LBW_2020_estimate)-log(LBW_lower))/1.96)
  
  index<-data.frame(ISO=unique(finalPreterm$ISO), 
                    countryIndex2=1:length(unique(finalPreterm$ISO)))
  covarset<-merge(x=covarset, y=index,by="ISO", all.y=TRUE)%>% dplyr::select(-countryIndex2) %>% distinct()
  

  covar_array <- create_covar_array(interest_cov = int_cov,estyears = estyears, 
                                    dataset = covarset)
  
  
  #source("PRETERM/2.covarArrayP.R")
  
  input.data_full$covar_array<-covar_array
  #input.data$ncov<-dim(covar_array)[1]
  input.data_full$ncov<-1
  #covar_array<-readRDS("output/covar_array2.rds")
  
  rm(covar_array)
  
  #----
  
  source("functions/getSplines_Old.R")
  splines.data <- getSplinesData(26, order = 1, degree=3, I=155)
  
  input.data.splines_full <- c(input.data_full, splines.data)
  
  cs.smoothing <- TRUE
  nserror.estimated <- TRUE
  input.data.splines_full$sigma.y <- NA
  
  
  for(i in 1:length(input.data_full)){
    assign(names(input.data_full)[i], input.data_full[[i]])
  }
  isos <- unique(d[["ISO"]])
  
  rm(input.data_full)
  
  source("functions/runMCMC2P.R")
  source("functions/runMCMCGlobal2P.R")
  
  cat("running Full Model!")

  
  allMods<-list.files("output/models/")
  notModVal<- paste0(substr(model.file.path, 16, 
                            as.numeric(gregexpr(".txt", model.file.path))-1),
                     niter, ".RDS")
  if (notModVal %in% allMods){
    mod.full<-readRDS(paste0("output/models/",notModVal))
  } else{
    mod.full<- runMCMC2P(method = "splines",
                        order = 1,
                        input.data = input.data.splines_full,
                        cs.smoothing = cs.smoothing,
                        nserror.estimated = nserror.estimated,
                        nchains = nchains,
                        nburnin = nburnin,
                        niter = niter,
                        nthin = nthin,
                        model.file.path = model.file.path, 
                        save.pdf=FALSE, adapt=FALSE)
  }
  
  
  
  
  allDataFull<-as.data.frame(list(time=NA, lower=NA, median=NA, upper=NA, ISO=NA))
  isos <- unique(d[["ISO"]])
  
  for (i in 1:NIsos){
    iso<-isos[i]
    #iso.number <- which(isos==iso)
    data <- getResults(mod.full, method = "splines", iso.number = i, time.trend = time.trend,
                       nyears = input.data.splines_full$nyears.c[i],
                       startyear = input.data.splines_full$startyear.c[i])
    data$ISO<-iso
    
    allDataFull<-rbind(allDataFull, data)
  }
  
  allDataFull<-allDataFull %>% mutate(lower=inv.logit(lower)*100, median=inv.logit(median)*100,
                                      upper=inv.logit(upper)*100)%>% filter(!is.na(ISO)) %>% mutate(isoYear=paste0(iso, time))
  allDataFull$isoYear<-paste0(allDataFull$ISO, allDataFull$time)
  
  #--80%
  allDataFull2<-as.data.frame(list(time=NA, lower80=NA, median=NA, upper80=NA, ISO=NA))
  isos <- unique(d[["ISO"]])
  
  for (i in 1:NIsos){
    iso<-isos[i]
    #iso.number <- which(isos==iso)
    data <- getResults(mod.full, method = "splines", iso.number = i, time.trend = time.trend,
                       nyears = input.data.splines_full$nyears.c[i],
                       startyear = input.data.splines_full$startyear.c[i], alpha=0.2)
    data$ISO<-iso
    
    allDataFull2<-rbind(allDataFull2, data %>% rename(lower80=lower, 
                                                      upper80=upper))
  }
  
  allDataFull2<-allDataFull2 %>% mutate(lower80=inv.logit(lower80)*100, median=inv.logit(median)*100,
                                        upper80=inv.logit(upper80)*100)%>% filter(!is.na(ISO)) %>% mutate(isoYear=paste0(iso, time))
  allDataFull2$isoYear<-paste0(allDataFull2$ISO, allDataFull2$time)
  #--
  #--50%
  allDataFull3<-as.data.frame(list(time=NA, lower50=NA, median=NA, upper50=NA, ISO=NA))
  isos <- unique(d[["ISO"]])
  
  for (i in 1:NIsos){
    iso<-isos[i]
    #iso.number <- which(isos==iso)
    data <- getResults(mod.full, method = "splines", iso.number = i, time.trend = time.trend,
                       nyears = input.data.splines_full$nyears.c[i],
                       startyear = input.data.splines_full$startyear.c[i], alpha=0.5)
    data$ISO<-iso
    
    allDataFull3<-rbind(allDataFull3, data %>% rename(lower50=lower, 
                                                      upper50=upper))
  }
  
  allDataFull3<-allDataFull3 %>% mutate(lower50=inv.logit(lower50)*100, median=inv.logit(median)*100,
                                        upper50=inv.logit(upper50)*100)%>% filter(!is.na(ISO)) %>% mutate(isoYear=paste0(iso, time))
  allDataFull3$isoYear<-paste0(allDataFull3$ISO, allDataFull3$time)
  #--
  
  allDataFull<-merge(x=merge(x=allDataFull, y=allDataFull2, by=c("ISO", "time"), all.x=TRUE), 
                     y=allDataFull3, by=c("ISO", "time"), all.x=TRUE) %>% 
    dplyr::select(ISO, time, lower, median=median.x, upper, lower50, upper50, lower80, upper80)
  
  
  fullDIC<-mod.full$BUGSoutput$DIC
  
  rm(mod.full)
  
  cat("finished Full Model!")
  
  cat("Getting the left out samples")
  
  for (m in 1:length(leave.out.method)){ #For each of the methods do the whole thing
    
    if (leave.out.method[m] == "recent") {
      nreps <- 1
    }   
    rmse<-c()
    desinCI<-c()
    desaboveCI<-c()
    desbelowCI<-c()
    inCI<-c()
    aboveCI<-c()
    belowCI<-c()
    predE<-c()
    predEAbs<-c()
    medInFCI<-c()
    medInFCI50<-c()
    medInFCI80<-c()
    dic<-c()
    for (i in 1:nreps) {
      
      if (leave.out.method[m] == "recent") {
        #nreps <- 1
        #list.sampled <- list()
        #list.lo <- list()
        # leaving out the most recent data point in all countries with at least 2 observations
        indiceslastobs <- list()
        list.lo <- list()
        for (c in 1: NIsos){
          if (sum(d$countryIndex==c)>2){
            indiceslastobs[[c]] <- which(d$yearIndex==max(d$yearIndex[d$countryIndex==c]) & d$countryIndex==c)
          }
        }
        list.lo<-d %>% filter((row %in% indiceslastobs))
        list.sampled<-d %>% filter(!(row %in% indiceslastobs))
      }
      
      if (leave.out.method[m] == "random") {
        #list.lo<-list()
        #list.sampled <- list()
        #set.seed(seed)
        #for (i in 1:nreps) {
        indicesPercOut <- list()
        for (c in 1: NIsos){
          if (sum(d$countryIndex==c)>5){ #For countries with more than 5 observations
            
            country<-d %>% filter(countryIndex==c)  
            
            indicesPercOut[[c]] <- country$row[sort(sample(1:nrow(country), round(nrow(country) * leave.out.percent/100)))] #Removing 20% of the countries data
            
          }
        }
        list.lo<-d %>% filter((row %in% unlist(indicesPercOut)))
        list.sampled<-d %>% filter(!(row %in% unlist(indicesPercOut)))
        
        #}
      }
      
      #Starting the sampling!
      
      #Last obs
      #i=1
      d2<-list.sampled
      N<-dim(d2)[1]
      input.data_lo <- processData(d = d2, iso.column = "ISO",
                                data.column = "logitY",
                                se.column = "logitSe",
                                obsyear.column = "Year",
                                region.column = "regionIndex",
                                source.column="sourceIndex",
                                end.year = endyear,
                                start.year = startYear)
      
      input.data2 <- processData(d = d2, iso.column = "ISO",
                                 data.column = "singMult",
                                 se.column = "logitSe",
                                 obsyear.column = "dataType",
                                 region.column = "regionIndex",
                                 source.column="methodGAGroup",
                                 #source.column = "dataType",
                                 end.year = endyear,
                                 start.year = 2000)
      input.data_lo$MGA<-input.data2$source.ci
      input.data_lo$singMult<-input.data2$y.ci
      input.data_lo$dataType<-input.data2$gett.ci
      
      input.data_lo$covar_array<-covar_array
      input.data_lo$ncov<-dim(covar_array)[1]
      input.data.splines_lo <- c(input.data_lo, splines.data)
      input.data.splines_lo$sigma.y <- NA #THIS IS WHERE WE WOULD PUT THE HIERARCHY
      
      for(j in 1:length(input.data_lo)){
        assign(names(input.data_lo)[j], input.data_lo[[j]])
      }
      isos <- unique(d[["ISO"]])
      
      rm(input.data2, input.data_lo)
      
      save=FALSE
      
      cat(paste0("running model for iteration  "), i)
      
      
      mod.lo <- runMCMC2P(method = "splines",
                         order = 1,
                         input.data = input.data.splines_lo,
                         cs.smoothing = cs.smoothing,
                         nserror.estimated = nserror.estimated,
                         nchains = nchains,
                         nburnin = nburnin,
                         niter = niter,
                         nthin = nthin,
                         model.file.path = model.file.path, save.pdf=save, adapt=save)
      
      
      
      
      cat(paste0("predicting for model for iteration  "), i)
      
      allDataLO<-as.data.frame(list(time=NA, lower=NA, median=NA, upper=NA, ISO=NA))
      isos <- unique(d[["ISO"]])
      for (j in 1:NIsos){
        iso<-isos[j]
        #iso.number <- which(isos==iso)
        data <- getResults(mod.lo, method = "splines", iso.number = j, time.trend = time.trend,
                           nyears = input.data.splines_lo$nyears.c[j],
                           startyear = input.data.splines_lo$startyear.c[j])
        data$ISO<-iso
        
        allDataLO<-rbind(allDataLO, data)
      }
      allDataLO<-allDataLO%>% filter(!is.na(ISO)) %>% mutate(isoYear=paste0(ISO, time))
      
      allDataLO<-allDataLO %>% mutate(lower=inv.logit(lower)*100, median=inv.logit(median)*100,
                                      upper=inv.logit(upper)*100) %>% filter(!is.na(ISO))
      
      
      cat(paste0("doing calculations for iteration "), i)
      
      
      allDataLO2<-allDataLO %>% 
        dplyr::select(ISO, time, isoYear, LOmedian=median, LOlower=lower, LOupper=upper) %>% 
        mutate(id=1)
      leftOuts<-as.data.frame(list.lo)  %>% mutate(leftOut=y*100)%>% dplyr::select(isoYear, leftOut)
      together<-merge(x=merge(x=allDataFull, y=allDataLO2, by=c("ISO", "time"), all.x=TRUE), y=leftOuts, by="isoYear", all.x=TRUE)
      
      #Getting the calculations
      calcs<-together %>% filter(time>=2000) %>% 
        mutate(logitSe=(qlogis(LOupper/100)/1.96)*qlogis(LOmedian/100),
               rmse=sqrt(sum((median - LOmedian)^2)/nrow(filter(together, time>=2000))),
               desirableCI=ifelse(leftOut>lower & leftOut<upper, 1, 0),
               desirableUpper=ifelse(leftOut>upper, 1, 0),
               desirableLower=ifelse(leftOut<lower, 1, 0),
               leftInPredCI=ifelse(leftOut>LOlower & leftOut<LOupper, 1, 0),
               leftInPredUpper=ifelse(leftOut>LOupper, 1, 0),
               leftInPredLower=ifelse(leftOut<LOlower, 1, 0),
               predictionErrors=((qlogis(leftOut/100)-qlogis(LOmedian/100))/logitSe),
               LOmedianInFullCI=ifelse(LOmedian>lower & LOmedian<upper, 1, 0),
               LOmedianInFullCI80=ifelse(LOmedian>lower80 & LOmedian<upper80, 1, 0),
               LOmedianInFullCI50=ifelse(LOmedian>lower50 & LOmedian<upper50, 1, 0))
      
      rmse.c<-mean(calcs$rmse, na.rm=TRUE)
      help<-calcs %>% filter(!is.na(leftOut))
      #How many of the left out values fall in the predicted intervals of the LOmodel
      inCI.c<-mean(calcs$leftInPredCI, na.rm=TRUE)
      desinCI.c<-mean(calcs$desirableCI, na.rm=TRUE)
      #How many of the left out values fall above upper LOmodel - should be 2.5%
      aboveCI.c<-mean(calcs$leftInPredUpper, na.rm=TRUE)
      desaboveCI.c<-mean(calcs$desirableUpper, na.rm=TRUE)
      #How many of the left out values fall below lower LOmodel - should be 2.5%
      belowCI.c<-mean(calcs$leftInPredLower, na.rm=TRUE)
      desbelowCI.c<-mean(calcs$desirableLower, na.rm=TRUE)
      #Prediction errors - prediction of the left out values from the LOmodel
      predE.c<-mean(calcs$predictionErrors, na.rm=TRUE)
      
      #Abs Prediction errors - prediction of the left out values from the LOmodel
      predEAbs.c<-mean(abs(calcs$predictionErrors), na.rm=TRUE)
      
      #How many values from the LOmodel are in the full model CIs
      medInFCI.c<-mean(calcs$LOmedianInFullCI, na.rm=TRUE)
      
      #How many values from the LOmodel are in the full model CIs - 80
      medInFCI80.c<-mean(calcs$LOmedianInFullCI80, na.rm=TRUE)
      
      #How many values from the LOmodel are in the full model CIs - 50
      medInFCI50.c<-mean(calcs$LOmedianInFullCI50, na.rm=TRUE)
      
      #DIC
      dic.c<-fullDIC - mod.lo$BUGSoutput$DIC
      
      rmse<-c(rmse, rmse.c)
      desinCI<-c(desinCI, desinCI.c)
      desaboveCI<-c(desaboveCI, desaboveCI.c)
      desbelowCI<-c(desbelowCI, desbelowCI.c)
      inCI<-c(inCI, inCI.c)
      aboveCI<-c(aboveCI, aboveCI.c)
      belowCI<-c(belowCI, belowCI.c)
      predE<-c(predE, predE.c)
      predEAbs<-c(predEAbs, predEAbs.c)
      medInFCI<-c(medInFCI, medInFCI.c)
      medInFCI50<-c(medInFCI50, medInFCI50.c)
      medInFCI80<-c(medInFCI80, medInFCI80.c)
      dic<-c(dic, dic.c)
      
      cat(paste0("finished calculations for iteration "), i)
      
      rm(mod.lo)
      outputs<-list(rmse = rmse, 
                    desinCI = desinCI, desaboveCI = desaboveCI,
                    desbelowCI=desbelowCI,
                    inCI = inCI, aboveCI = aboveCI, 
                    belowCI=belowCI, predE=predE, predEAbs=predEAbs, medInFCI=medInFCI,
                    medInFCI50=medInFCI50, medInFCI80=medInFCI80, dic=dic)
      #Resaves it every rep so if it fails
      saveRDS(outputs, "output/modelVal.RDS")
      
      
    } #end of nreps
    
    assign(paste0(leave.out.method[m], "Output"), list(rmse = rmse,
                                                       desinCI = desinCI, desaboveCI = desaboveCI,desbelowCI=desbelowCI, 
                                                       inCI = inCI, aboveCI = aboveCI,belowCI=belowCI, 
                                                       predE=predE, predEAbs=predEAbs, 
                                                       medInFCI=medInFCI,medInFCI80=medInFCI80,medInFCI50=medInFCI50, dic=dic))
  } #end of method
  
  
  
  if (length(leave.out.method)==2){
    return(list(recent=recentOutput, random=randomOutput))
  } else if (leave.out.method=="random"){
    return(list(random=randomOutput))
  } else{
    return(list(recent=recentOutput))
  }
  
  
  
  
}
