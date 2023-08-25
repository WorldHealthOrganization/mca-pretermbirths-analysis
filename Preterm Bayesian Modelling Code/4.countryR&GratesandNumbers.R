#PREDICT USING WHOLE ITERATIONS 

alpha.level=0.05
undo<-function(data){
  data<-as.data.frame(data)
  long<-gather(data, "chain", "name") %>% dplyr::select(-chain)
  return(long)
}
#Takes the SDG regions (rather than the modified regions used in the modelling)
regionCodesOther<-readRDS("input/regionCodesOther.rds") %>% 
  rename(regionName2=regionName, regionIndex2=regionIndex)
#WPP Livebirths estimates
wpp2<-readRDS("output/wpp2.rds")

#-----
#Coeffs - getting the coefficients from the model for each iteration

#Covariate coefficient
alpha1<-undo(mod$BUGSoutput$sims.array[,,"alpha"]) %>% rename(alpha=name)%>% mutate(row=row_number())

#Sampling from the Regional coefficients
beta1<-cbind(undo(mod$BUGSoutput$sims.array[,,"mu.beta[1]"]) %>% rename(mubeta1=name), undo(mod$BUGSoutput$sims.array[,,"tau.beta[1]"]) %>% rename(taubeta1=name)) %>% 
  rowwise() %>% mutate(beta1=rnorm(1,mubeta1, 1/sqrt(taubeta1)))
beta2<-cbind(undo(mod$BUGSoutput$sims.array[,,"mu.beta[2]"]) %>% rename(mubeta2=name), undo(mod$BUGSoutput$sims.array[,,"tau.beta[2]"]) %>% rename(taubeta2=name)) %>% 
  rowwise() %>% mutate(beta2=rnorm(1,mubeta2, 1/sqrt(taubeta2)))
beta3<-cbind(undo(mod$BUGSoutput$sims.array[,,"mu.beta[3]"]) %>% rename(mubeta3=name), undo(mod$BUGSoutput$sims.array[,,"tau.beta[3]"]) %>% rename(taubeta3=name)) %>% 
  rowwise() %>% mutate(beta3=rnorm(1,mubeta3, 1/sqrt(taubeta3)))
beta4<-cbind(undo(mod$BUGSoutput$sims.array[,,"mu.beta[4]"]) %>% rename(mubeta4=name), undo(mod$BUGSoutput$sims.array[,,"tau.beta[4]"]) %>% rename(taubeta4=name)) %>% 
  rowwise() %>% mutate(beta4=rnorm(1,mubeta4, 1/sqrt(taubeta4)))
beta5<-cbind(undo(mod$BUGSoutput$sims.array[,,"mu.beta[5]"]) %>% rename(mubeta5=name), undo(mod$BUGSoutput$sims.array[,,"tau.beta[5]"]) %>% rename(taubeta5=name)) %>% 
  rowwise() %>% mutate(beta5=rnorm(1,mubeta5, 1/sqrt(taubeta5)))
beta6<-cbind(undo(mod$BUGSoutput$sims.array[,,"mu.beta[6]"]) %>% rename(mubeta6=name), undo(mod$BUGSoutput$sims.array[,,"tau.beta[6]"]) %>% rename(taubeta6=name)) %>% 
  rowwise() %>% mutate(beta6=rnorm(1,mubeta6, 1/sqrt(taubeta6)))


coeffs<-cbind(alpha1, 
              beta1, beta2, beta3, beta4, beta5, beta6)

#----
#Getting the time element of the penalised splines section

Z<-list()
for (i in 1:11){
  for (j in 1:3){
    z<-as.data.frame(list(coefficient=paste0("Z.tk[", i,",",j, "]"), z=undo(mod$BUGSoutput$sims.array[,,paste0("Z.tk[", i,",",j, "]")]))) %>% 
      mutate(id=row_number())
    Z<-rbind(Z,z)
  }
}

Zbig<-reshape(Z, idvar="id", timevar="coefficient", direction = "wide")

#----
#Getting the country-level part of the penalised splines element, and 
# averaging over the (modelling) regions to get a regional value.

regions<-mod$model[[1]]$data()$region.c
yearIndex<-as.data.frame(list(year=2010:2020, yearIndex=1:11))

delta<-list()

for (k in 1:6){
  
  counts<-which(regions==k)
  
  for (i in 1:3){
    b<-list()
    for (j in 1:length(counts)){
      
      d<-as.data.frame(list(z=undo(mod$BUGSoutput$sims.array[,,paste0("delta.hc[", i,",",counts[j], "]")])))
      
      if (j==1){
        b<-d
      } else {
        b<-cbind(b,d)
      }
      
    }
    
    
    names(b)<-paste0(names(b), c(1:length(names(b))))
    bMean<-b %>% mutate(mean=rowMeans(.)) %>% dplyr::select(mean)
    names(bMean)<-paste0("r", k, "mean", i)
    
    assign(paste0("b", i), bMean)
    
  }
  
  all<-cbind(b1, b2, b3)
  
  if (k==1){
    delta<-all
  } else {
    delta<-cbind(delta,all)
  }
  
}

#------------------
#adding all together, rows for each iteration.

allCovars<-cbind(coeffs, cbind(Zbig, delta)) %>% dplyr::select(-id)
rm(alpha1, 
   b, b1, b2, b3, 
   beta1, beta2, beta3, beta4, beta5, beta6,
   bMean, coeffs, z, Z, Zbig)
#-----------
#adding for each of the countries

regionCodes<-readRDS("output/regionCodes.RDS")
regionCodesOther<-readRDS("output/regionCodesOther.rds") %>% 
  rename(regionName2=regionName, regionIndex2=regionIndex) 

#Getting the countries which have no predictions
toPred<-anti_join(regionCodes, finalPreterm, by=c("ISO"))

#Adding covariates
int_cov<-c("logLBW")

covarset<-read.csv(lbwEstimates) %>% 
  dplyr::select(ISO, year, LBW_2020_estimate=est, LBW_lower=estL) %>% 
  mutate(logLBW=log(LBW_2020_estimate),
         logLBWSe=(log(LBW_2020_estimate)-log(LBW_lower))/1.96)

toPred2<-merge(x=merge(x=covarset, y=toPred,by=c("ISO"), all.y=TRUE),
               y=regionCodesOther %>% 
                 dplyr::select(-OfficialName), by="ISO", all.x=TRUE)

isos2<-unique(toPred2$ISO)
nIsos<-length(isos2)

predict<-c()
region1<-as.data.frame(list(ISO=NA, year=NA, mu=NA))
region2<-as.data.frame(list(ISO=NA, year=NA, mu=NA))
region3<-as.data.frame(list(ISO=NA, year=NA, mu=NA))
region4<-as.data.frame(list(ISO=NA, year=NA, mu=NA))
region5<-as.data.frame(list(ISO=NA, year=NA, mu=NA))
region6<-as.data.frame(list(ISO=NA, year=NA, mu=NA))
for (i in 1:nIsos){
  
  iso<-isos2[i]
  cPred<-toPred2 %>% filter(ISO==iso)
  cPred2<-merge(x=cPred, y=allCovars)
  
  deltaCol1<-which(names(cPred2)== paste0("r", unique(cPred2$regionIndex), "mean", 1))
  deltaCol2<-which(names(cPred2)== paste0("r", unique(cPred2$regionIndex), "mean", 2))
  deltaCol3<-which(names(cPred2)== paste0("r", unique(cPred2$regionIndex), "mean", 3))
  
  cPred3<-cPred2
  cPred3$deltaCol1<-cPred2[,deltaCol1]
  cPred3$deltaCol2<-cPred2[,deltaCol2]
  cPred3$deltaCol3<-cPred2[,deltaCol3]
  
  cPredList<-list()
  for (j in 1:11){
    cPredTime<-cPred3 %>% filter(year==yearIndex$year[j])
    
    zCol1<-which(names(cPredTime)== paste0("name.Z.tk[", j, ",", 1, "]"))
    zCol2<-which(names(cPredTime)== paste0("name.Z.tk[", j, ",", 2, "]"))
    zCol3<-which(names(cPredTime)== paste0("name.Z.tk[", j, ",", 3, "]"))
    
    cPredTime2<-cPredTime
    cPredTime2$zCol1<-cPredTime[,zCol1]
    cPredTime2$zCol2<-cPredTime[,zCol2]
    cPredTime2$zCol3<-cPredTime[,zCol3]
    
    if (j==1){
      cPredList<-cPredTime2
    } else{
      cPredList<-rbind(cPredList, cPredTime2)
    }
    
  }
  #For each country and each iteration, adds the regional intercept, 
  # regional time element, and covariate value and coefficient.
  
  cPredList2<-cPredList %>% mutate(beta.d=ifelse(regionIndex==1, beta1,
                                                 ifelse(regionIndex==2, beta2, 
                                                        ifelse(regionIndex==3, beta3, 
                                                               ifelse(regionIndex==4, beta4, 
                                                                      ifelse(regionIndex==5, beta5, 
                                                                             ifelse(regionIndex==6, beta6, NA)))))),
                                   time=(deltaCol1*zCol1)+(deltaCol2*zCol2)+(deltaCol3*zCol3),
                                   
                                   mu=logLBW*alpha+
                                     beta.d+time) %>% dplyr::select(ISO, year, mu)
  #Taking the 2.5th, 50th and 97.5th quantiles of the iterations to get the estimates
  # and the 95% credible intervals.
  cPredList3<-cPredList2 %>% group_by(ISO, year) %>% summarise(estL=inv.logit(quantile(mu, 0.025))*100, 
                                                               est=inv.logit(quantile(mu, 0.5))*100, 
                                                               estU=inv.logit(quantile(mu, 0.975))*100)
  #Getting the SDG region (not the modelling region)
  region<-unique(cPred2$regionIndex2)
  
  #Outputting the country-level iterations to regional datasets
  if(region==1){
    region1<-rbind(region1, cPredList2)
  } else if(region==2){
    region2<-rbind(region2, cPredList2)
  } else if(region==3){
    region3<-rbind(region3, cPredList2)
  } else if(region==4){
    region4<-rbind(region4, cPredList2)
  } else if(region==5){
    region5<-rbind(region5, cPredList2)
  } else{
    region6<-rbind(region6, cPredList2)
  } 
  
  #Outputting the country-level estimates (summarised iterations)
  if (i==1){
    predict<-cPredList3
  } else{
    predict<-rbind(predict, cPredList3)
  }
  
  
}
#-----------------------------
#Outputting the country-level estimates from the model

isos<-unique(finalPreterm$ISO)
isoNum<-length(isos)
allData<-as.data.frame(list(time=NA, lower=NA, median=NA, upper=NA, ISO=NA))
for (i in 1:isoNum){
  iso<-isos[i]
  iso.number <- which(isos==iso)
  data <- getResults(mod, method = "splines", iso.number = iso.number, time.trend = time.trend,
                     nyears = nyears.c[iso.number],
                     startyear = startyear.c[iso.number])
  data$ISO<-iso
  
  allData<-rbind(allData, data)
}

allData<-allData %>% mutate(lower=inv.logit(lower)*100, median=inv.logit(median)*100,
                            upper=inv.logit(upper)*100) %>% filter(!is.na(ISO)) %>% 
  mutate(predicted=0) %>% 
  rename(year=time) %>% 
  rename(est=median, estL=lower, estU=upper)

predict2<-predict %>% dplyr::select(ISO, year,est, estL, estU) %>% 
  mutate(predicted=1)

rates<-rbind(allData, predict2) %>% 
  filter(if_any(everything(), ~ !is.na(.))) %>% 
  filter(year>=2000)

#Getting the numbers of the Preterm from the estimate rates and the WPP livebirths
estimates<-merge(x=rates, wpp2, 
                 by=c("ISO", "year"), 
                 all.x=TRUE) %>% 
  mutate(estN=round((est/100)*wpp_lb), 
         estNL=round((estL/100)*wpp_lb),
         estNU=round((estU/100)*wpp_lb))

write.csv(estimates,
          paste0("output/" ,substr(fileName, 1, nchar(fileName)),"_", 
                 ((niter-nburnin)/nthin)*nchains,"_Estimates", ".csv"))
rm(predict, cPredList, cPred, cPred2, 
   cPred3, cPredList2, cPredList3,
   
   all, allCovars, delta)
#------------------------------------------
#Getting regional and global estimates

#Getting the iterations of the countries with data from the model
# and outputting these to a regional dataset.

mu<-mod[["BUGSoutput"]][["sims.list"]][["mu.ct"]]
isoss<-as.data.frame(list(ISO=isos)) %>% mutate(countryIndex=row_number())
isos1<-merge(x=isoss, y=regionCodesOther %>% 
               dplyr::select(-OfficialName), by="ISO", all.x=TRUE)
for (r in 1:6){
  isos2<-isos1 %>% filter(regionIndex2==r)
  for (i in 1:nrow(isos2)){
    for (j in 1:dim(mu)[3]){
      datas<-as.data.frame(list(ISO=rep(isos2$ISO[i], dim(mu)[1]), 
                                year=rep(2009+j, dim(mu)[1]), 
                                mu=mu[,isos2$countryIndex[i],j]))
      if (i==1 & j==1){
        otherPs<-datas
      }else{
        otherPs<-rbind(otherPs, datas)
      }
    }
    print(i)
  }
  assign(paste0("region", r, "A"), otherPs)
}


#------------------------
#REGIONS
#Summarising the country-level iterations across regions, to get regional estimates
# iterations, then taking the quantiles to get the credible intervals of the estimates.
#Finally adding all the country-level iterations together and summarising to get
# the global level estimates.
for (i in 1:7){
  
  if(i==1){ data1<-region1
  data2<-region1A
  rm(region1, region1A)
  } else if (i==2){ data1<-region2
  data2<-region2A
  rm(region2, region2A)
  } else if (i==3){data1<-region3
  data2<-region3A
  rm(region3, region3A)
  } else if (i==4){ data1<-region4
  data2<-region4A
  rm(region4, region4A)
  }else if (i==5){ data1<-region5
  data2<-region5A
  rm(region5, region5A)
  }else if (i==6){ data1<-region6
  data2<-region6A
  rm(region6, region6A)
  }else if(i==7){
    data3<-regionsAll
    rm(regionsAll, data1, data2)
  }
  
  if (i<7){
    data3<-rbind(x=data1 %>% filter(!ISO %in% data2$ISO),
                 y=data2 %>% filter(year>=2000))
  }
  print(length(unique(data3$ISO)))
  data5<-merge(x=data3, y=wpp2 %>% dplyr::select(-c(regionName2, regionIndex2)), 
               by=c("ISO", "year"), all.x=TRUE)%>%
 
   filter(!is.na(ISO)) %>% 
    mutate(numbers=round(wpp_lb*inv.logit(mu),0)) %>% 
    group_by(ISO,year) %>% mutate(iteration=row_number()) %>% 
    group_by(year, iteration) %>% 
    summarise(regionNumbers=sum(numbers),
              wppR=sum(wpp_lb),
              ISO=length(unique(ISO)))  %>% 
    ungroup() %>% 
    group_by(year) %>% 
    summarise(estNL=quantile(regionNumbers, 0.025), 
              estN=quantile(regionNumbers, 0.5), 
              estNU=quantile(regionNumbers, 0.975),
              wppR=mean(wppR)) %>% 
    mutate(estL=estNL/wppR, 
           est=estN/wppR, 
           estU=estNU/wppR) %>% 
    mutate(regionIndex=i)
  
  if (i==1){
    regionsAll<-data3
    all<-data5
    print(length(unique(regionsAll$ISO)))
  } else if (i<7){
    regionsAll<-rbind(regionsAll, data3)
    all<-rbind(all, data5)
    
    print(length(unique(regionsAll$ISO)))
  }
  else{
    all<-rbind(all, data5)
  }
  
}

#Adding the country level numbers of the LBW babies together to make sure 
# that the regional and global estimates are the sum of the countries.

regionNumbers<-estimates %>% 
  group_by(regionName2, year) %>% 
  summarise(estN=sum(estN), 
            wpp_lb=sum(wpp_lb)) %>% 
  mutate(est=estN/wpp_lb)
globalNumbers<-estimates %>% 
  group_by(year) %>% 
  summarise(estN=sum(estN), 
            wpp_lb=sum(wpp_lb)) %>% 
  mutate(est=estN/wpp_lb)%>% 
  mutate(regionName2="Global")


both<-rbind(regionNumbers, globalNumbers)

#Adding the credible intervals calculated above to the summed estimates.
regions<-regionCodes %>% dplyr::select(regionName2=regionName, regionIndex) %>% 
  distinct()

regionalAndGlobal<-merge(x=all %>% dplyr::select(-c(est, estN)), y=regions, 
                         by="regionIndex", all.x=TRUE) %>% 
  mutate(regionName2=ifelse(is.na(regionName2), "Global", regionName2)) %>% 
  rename(wpp_lb=wppR)

regionalAndGlobal2<-merge(x=regionalAndGlobal, y=both %>% dplyr::select(-wpp_lb), 
                          by=c("regionName2", "year"), all.x=TRUE)

write.csv(regionalAndGlobal2%>% 
            dplyr::select(regionName2, year, estL, est, estU, wpp_lb, 
                          estNL, estN, estNU),
          paste0("output/" ,substr(fileName, 1, nchar(fileName)),"_", 
                 ((niter-nburnin)/nthin)*nchains,"_regionalAndGlobalEstimates", ".csv"))


#-----------------------
#Output tables for the paper

table2<-regionalAndGlobal2 %>% filter(year %in% c(2020)) %>% 
  arrange(factor(regionIndex, levels=c(5,6,3,1,2,4,7))) %>% 
  mutate(rates=paste0(round(est*100,1), " (", round(estL*100, 1), ", ", round(estU*100, 1), ")"),
         numbers=paste0(round(estN), " (", round(estNL), ", ", round(estNU), ")"),
         propWPP=round(wpp_lb/wpp_lb[n()]*100,1),
         propPreterm=round(estN/estN[n()]*100,1)) %>% 
  dplyr::select(regionIndex, regionName2, year, rates, numbers, wpp_lb, 
                propWPP, propPreterm) %>% 
  arrange(year, regionIndex) %>% 
  #pivot_wider(names_from="year", values_from=c("rates", "numbers")) %>% 
  mutate(WPP=paste0(wpp_lb, " (", propWPP, ")")) %>% 
  arrange(factor(regionIndex, levels=c(5,6,3,1,2,4,7))) %>% 
  dplyr::select(regionIndex,regionName2, WPP, 
                rates, numbers, propPreterm)

#ARR Method 1 - regional and global by year
# lbw<-regionalAndGlobal2 %>% group_by(regionName) %>% 
#   mutate(percDiff=round(((est-lag(est))/lag(est))*100,1))
# lbw2<-lbw %>% group_by(regionName) %>% summarise(mean=mean(percDiff, na.rm=TRUE))

#ARR Method 2 - between 2000 and 2020.
lbw<-regionalAndGlobal2%>% filter(year %in% c(2010,2020)) %>% 
  group_by(regionName2) %>% 
  mutate(ARR=round((1-(est*100/lag(est*100))^(1/10))*100,2)) %>% 
  filter(!is.na(ARR)) %>% 
  dplyr::select(regionName2, ARR)

# help<-merge(x=lbw, y=lbw2, by="regionName", 
#             all.x=TRUE)


#ARR Method 3 (WHA method) - between 2012 and 2020.
arr2<-regionalAndGlobal2 %>% filter(year >=2012) %>% 
  arrange(regionName, year) %>% group_by(regionName) %>% 
  do(model=lm(log(est)~year, data=.)) %>% 
  mutate(WHA_AARR2012=round(100*(1-exp(coef(model)["year"])),2))%>%
  dplyr::select(regionName, WHA_AARR2012)

#- between 2000 and 2020.
arr3<-regionalAndGlobal2 %>% filter(year >=2000) %>% 
  arrange(regionName, year) %>% group_by(regionName) %>% 
  do(model=lm(log(est)~year, data=.)) %>% 
  mutate(WHA_AARR2000=round(100*(1-exp(coef(model)["year"])),2))%>%
  dplyr::select(regionName, WHA_AARR2000)

arrAll<-merge(x=arr2, y=arr3, by="regionName")


table23<-merge(x=table22, y=arrAll, 
               by="regionName", 
               all.x = TRUE) %>% 
  arrange(factor(regionIndex, levels=c(5,6,3,1,2,4,7)))

# write.csv(table23,
#           paste0("output/" ,substr(fileName, 1, nchar(fileName)),"_", 
#                  ((niter-nburnin)/nthin)*nchains,"_table2", ".csv"))

