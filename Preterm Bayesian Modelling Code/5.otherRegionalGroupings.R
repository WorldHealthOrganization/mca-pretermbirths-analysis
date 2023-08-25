# source("0.loadPackages.R")
# nchains = 4
# nburnin = 100000
# nthin = 25
# niter = (nthin*4000)+nburnin
# file<-"PRETERM/models/pCCFullModelP_dataType1DQC1.txt"
# fileName<-substr(file, 9, as.numeric(gregexpr(".txt", file))-1)
lbwEstimates<-"output/finalLBWestimates.csv"

#------------------------------
finalPreterm<-readRDS("PRETERM/output/finalPreterm_MGA.RDS")
mod <- readRDS(paste0("PRETERM/output/", fileName,((niter-nburnin)/nthin)*nchains, ".RDS"))

alpha.level=0.05
undo<-function(data){
  data<-as.data.frame(data)
  long<-gather(data, "chain", "name") %>% dplyr::select(-chain)
  return(long)
}
source("0.fileNames.R")

#WHO_REGION	LDC_LATEST	UN_REGION	UN_SUBREGION	UN_SDG	LLDS_SIDS_LATEST	
# WB_LATEST	FAO_LIFD_LATEST	UNICEF_REGION	UNICEF_SUBREGION	UNICEF_PROGRAMME_REGION

#For each of the regional groupings in the list above, add the name into the "regionalGrouping" variable below
# and run the whole of the script.
regionalGrouping<-"FAO_LIFD_LATEST"

regionCodes<-readRDS("output/regionCodesOther.rds") %>%
  rename(regionName2=regionName, regionIndex2=regionIndex)

regionCodesOther<-readxl::read_xlsx("input/Crosswalk_02242023b.xlsx") %>% 
  rename(ISO=`ISO-alpha3 Code`) %>% 
  filter(ISO %in% regionCodes$ISO) %>% 
  dplyr::select(ISO, regionName2=FAO_LIFD_LATEST) %>% 
  group_by(regionName2) %>% 
  mutate(regionIndex2 = cur_group_id())



wpp2<-readRDS("output/wpp2.rds")

#-----
#Coeffs
alpha1<-undo(mod$BUGSoutput$sims.array[,,"alpha"]) %>% rename(alpha=name)%>% mutate(row=row_number())

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
#Z

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
#delta
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
# regionCodesOther<-readRDS("output/regionCodesOther.rds") %>% 
#   rename(regionName2=regionName, regionIndex2=regionIndex) 
toPred<-anti_join(regionCodes, finalPreterm, by=c("ISO"))

int_cov<-c("logLBW")

covarset<-read.csv(lbwEstimates) %>% 
  dplyr::select(ISO, year, LBW_2020_estimate=est, LBW_lower=estL) %>% 
  mutate(logLBW=log(LBW_2020_estimate),
         #logLBWSe=sqrt(LBW_sd*(1/LBW_2020_estimate^2))
         #logLBWSe=sd(logLBW, na.rm=TRUE),
         logLBWSe=(log(LBW_2020_estimate)-log(LBW_lower))/1.96)

toPred2<-merge(x=merge(x=covarset, y=toPred,by=c("ISO"), all.y=TRUE),
               y=regionCodesOther, by="ISO", all.x=TRUE)

isos2<-unique(toPred2$ISO)
nIsos<-length(isos2)

predict<-c()
for (i in 1:max(regionCodesOther$regionIndex2)){
  region<-as.data.frame(list(ISO=NA, year=NA, mu=NA))
  assign(paste0("region", i), region)
}
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
  
  cPredList2<-cPredList %>% mutate(beta.d=ifelse(regionIndex==1, beta1,
                                                 ifelse(regionIndex==2, beta2, 
                                                        ifelse(regionIndex==3, beta3, 
                                                               ifelse(regionIndex==4, beta4, 
                                                                      ifelse(regionIndex==5, beta5, 
                                                                             ifelse(regionIndex==6, beta6, NA)))))),
                                   time=(deltaCol1*zCol1)+(deltaCol2*zCol2)+(deltaCol3*zCol3),
                                   
                                   mu=logLBW*alpha+
                                     beta.d+time) %>% dplyr::select(ISO, year, mu)
  cPredList3<-cPredList2 %>% group_by(ISO, year) %>% summarise(estL=inv.logit(quantile(mu, 0.025))*100, 
                                                               est=inv.logit(quantile(mu, 0.5))*100, 
                                                               estU=inv.logit(quantile(mu, 0.975))*100)
  
  region<-unique(cPred2$regionIndex2)
  
  if(region==1){region1<-rbind(region1, cPredList2)
  } else if(region==2){
    region2<-rbind(region2, cPredList2)
  } else if(region==3){
    region3<-rbind(region3, cPredList2)
  } else if(region==4){
    region4<-rbind(region4, cPredList2)
  } else if(region==5){
    region5<-rbind(region5, cPredList2)
  } else if(region==5){
    region5<-rbind(region5, cPredList2)
  }else if(region==6){
    region6<-rbind(region6, cPredList2)
  }else if(region==7){
    region7<-rbind(region7, cPredList2)
  }else if(region==8){
    region8<-rbind(region8, cPredList2)
  }else if(region==9){
    region9<-rbind(region9, cPredList2)
  }else if(region==10){
    region10<-rbind(region10, cPredList2)
  }else if(region==11){
    region11<-rbind(region11, cPredList2)
  }else if(region==12){
    region12<-rbind(region12, cPredList2)
  }else if(region==13){
    region13<-rbind(region13, cPredList2)
  }else if(region==14){
    region14<-rbind(region14, cPredList2)
  }else if(region==15){
    region15<-rbind(region15, cPredList2)
  }else if(region==16){
    region16<-rbind(region16, cPredList2)
  }else if(region==17){
    region17<-rbind(region17, cPredList2)
  }else if(region==18){
    region18<-rbind(region18, cPredList2)
  }else if(region==19){
    region19<-rbind(region19, cPredList2)
  }else if(region==20){
    region20<-rbind(region20, cPredList2)
  }else if(region==21){
    region21<-rbind(region21, cPredList2)
  }else if(region==22){
    region22<-rbind(region22, cPredList2)
  }else{
    region23<-rbind(region23, cPredList2)
  } 
  
  if (i==1){
    predict<-cPredList3
  } else{
    predict<-rbind(predict, cPredList3)
  }
  
  
}
#-----------------------------
#Country-level estimates
isos<-unique(finalPreterm$ISO)
isoNum<-length(isos)
nyears.c<-mod$model[[1]]$data()$nyears.c
startyear.c<-2010
allData<-as.data.frame(list(time=NA, lower=NA, median=NA, upper=NA, ISO=NA))

for (i in 1:isoNum){
  iso<-isos[i]
  iso.number <- which(isos==iso)
  data <- getResults(mod, method = "splines", iso.number = iso.number, time.trend = time.trend,
                     nyears = 11,
                     startyear = 2010)
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

estimates<-merge(x=rates, wpp2, 
                 by=c("ISO", "year"), 
                 all.x=TRUE) %>% 
  mutate(estN=round((est/100)*wpp_lb), 
         estNL=round((estL/100)*wpp_lb),
         estNU=round((estU/100)*wpp_lb))

# write.csv(estimates,
#           paste0("PRETERM/output/" ,substr(fileName, 8, nchar(fileName)),"_", 
#                  ((niter-nburnin)/nthin)*nchains,"_Estimates", ".csv"))
rm(predict, cPredList, cPred, cPred2, 
   cPred3, cPredList2, cPredList3,
   cPredTime, cPredTime,
   all, allCovars, delta)
#------------------------------------------
#Getting regional and global estimates

#Get mus
mu<-mod[["BUGSoutput"]][["sims.list"]][["mu.ct"]]
#isos<-finalPreterm  %>% dplyr::select(ISO, regionIndex, countryIndex) %>% distinct()
isoss<-as.data.frame(list(ISO=isos)) %>% mutate(countryIndex=row_number())
isos1<-merge(x=isoss, y=regionCodesOther , by="ISO", all.x=TRUE)
for (r in 1:max(regionCodesOther$regionIndex2)){
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
# 
# length(unique(filter(region1, year>=2000)$ISO))+
#   length(unique(filter(region2, year>=2000)$ISO))+
#   length(unique(filter(region3, year>=2000)$ISO))+
#   length(unique(filter(region4, year>=2000)$ISO))+
#   length(unique(filter(region5, year>=2000)$ISO))+
#   length(unique(filter(region6, year>=2000)$ISO))-6+
#   length(unique(filter(region1A, year>=2000)$ISO))+
#   length(unique(filter(region2A, year>=2000)$ISO))+
#   length(unique(filter(region3A, year>=2000)$ISO))+
#   length(unique(filter(region4A, year>=2000)$ISO))+
#   length(unique(filter(region5A, year>=2000)$ISO))+
#   length(unique(filter(region6A, year>=2000)$ISO))
# 
# length(unique(filter(region6, year>=2000)$ISO))+
#   length(unique(filter(region6A, year>=2000)$ISO))-1


# help<-rbind(region1 %>% dplyr::select(ISO) %>% filter(!is.na(ISO)) %>%  distinct() %>% mutate(region=1),
#             region2 %>% dplyr::select(ISO) %>% filter(!is.na(ISO)) %>%  distinct() %>% mutate(region=2),
#             region3 %>% dplyr::select(ISO) %>% filter(!is.na(ISO)) %>%  distinct() %>% mutate(region=3),
#             region4 %>% dplyr::select(ISO) %>% filter(!is.na(ISO)) %>%  distinct() %>% mutate(region=4),
#             region5 %>% dplyr::select(ISO) %>% filter(!is.na(ISO)) %>%  distinct() %>% mutate(region=5),
#             region6 %>% dplyr::select(ISO) %>% filter(!is.na(ISO)) %>%  distinct() %>% mutate(region=6),
#             region1A %>% dplyr::select(ISO) %>% filter(!is.na(ISO)) %>%  distinct() %>% mutate(region=1),
#             region2A %>% dplyr::select(ISO) %>% filter(!is.na(ISO)) %>%  distinct() %>% mutate(region=2),
#             region3A %>% dplyr::select(ISO) %>% filter(!is.na(ISO)) %>%  distinct() %>% mutate(region=3),
#             region4A %>% dplyr::select(ISO) %>% filter(!is.na(ISO)) %>%  distinct() %>% mutate(region=4),
#             region5A %>% dplyr::select(ISO) %>% filter(!is.na(ISO)) %>%  distinct() %>% mutate(region=5),
#             region6A %>% dplyr::select(ISO) %>% filter(!is.na(ISO)) %>%  distinct() %>% mutate(region=6))



for (i in 1:max(regionCodesOther$regionIndex2)){
  
  if(i==1){ 
    if (i==(max(regionCodesOther$regionIndex2)+1)){
      data3<-regionsAll
      rm(regionsAll)
    } else{
      data1<-region1
      data2<-region1A
      rm(region1A, region1)
    }
  } else if (i==2){     if (i==(max(regionCodesOther$regionIndex2)+1)){
    data3<-regionsAll
    rm(regionsAll)
  } else{
    data1<-region2
    data2<-region2A
    rm(region2A, region2)
  }
  } else if (i==3){    if (i==(max(regionCodesOther$regionIndex2)+1)){
    data3<-regionsAll
    rm(regionsAll)
  } else{
    data1<-region3
    data2<-region3A
    rm(region3A, region3)
  }
  } else if (i==4){     if (i==(max(regionCodesOther$regionIndex2)+1)){
    data3<-regionsAll
    rm(regionsAll)
  } else{
    data1<-region4
    data2<-region4A
    rm(region4A, region4)
  }
  }else if (i==5){     if (i==(max(regionCodesOther$regionIndex2)+1)){
    data3<-regionsAll
    rm(regionsAll)
  } else{
    data1<-region5
    data2<-region5A
    rm(region5A, 
       region5)
  }
  }else if (i==6){ if (i==(max(regionCodesOther$regionIndex2)+1)){
    data3<-regionsAll
    rm(regionsAll)
  } else{
    data1<-region6
    data2<-region6A
    rm(region6A, 
       region6)
  }
  }else if(i==7){if (i==(max(regionCodesOther$regionIndex2)+1)){
    data3<-regionsAll
    rm(regionsAll)
  } else{
    data1<-region7
    data2<-region7A
    rm(region7A, 
       region7)
  }
  }else if(i==8){if (i==(max(regionCodesOther$regionIndex2)+1)){
    data3<-regionsAll
    rm(regionsAll)
  } else{
    data1<-region8
    data2<-region8A
    rm(region8A, 
       region8)
  }
  }else if(i==9){if (i==(max(regionCodesOther$regionIndex2)+1)){
    data3<-regionsAll
    rm(regionsAll)
  } else{
    data1<-region9
    data2<-region9A
    rm(region9A, 
       region9)
  }
  }else if(i==10){if (i==(max(regionCodesOther$regionIndex2)+1)){
    data3<-regionsAll
    rm(regionsAll)
  } else{
    data1<-region10
    data2<-region10A
    rm(region10A, 
       region10)}}else if(i==11){if (i==(max(regionCodesOther$regionIndex2)+1)){
         data3<-regionsAll
         rm(regionsAll)
       } else{
         data1<-region11
         data2<-region11A
         rm(region11A, 
            region11)}}else if(i==12){if (i==(max(regionCodesOther$regionIndex2)+1)){
              data3<-regionsAll
              rm(regionsAll)
            } else{
              data1<-region12
              data2<-region12A
              rm(region12A, 
                 region12)}}else if(i==13){if (i==(max(regionCodesOther$regionIndex2)+1)){
                   data3<-regionsAll
                   rm(regionsAll)
                 } else{
                   data1<-region13
                   data2<-region13A
                   rm(region13A, 
                      region13)}}else if(i==14){if (i==(max(regionCodesOther$regionIndex2)+1)){
                        data3<-regionsAll
                        rm(regionsAll)
                      } else{
                        data1<-region14
                        data2<-region14A
                        rm(region14A, 
                           region14)}}else if(i==15){if (i==(max(regionCodesOther$regionIndex2)+1)){
                             data3<-regionsAll
                             rm(regionsAll)
                           } else{
                             data1<-region15
                             data2<-region15A
                             rm(region15A, 
                                region15)}}else if(i==16){if (i==(max(regionCodesOther$regionIndex2)+1)){
                                  data3<-regionsAll
                                  rm(regionsAll)
                                } else{
                                  data1<-region16
                                  data2<-region16A
                                  rm(region16A, 
                                     region16)}}else if(i==17){if (i==(max(regionCodesOther$regionIndex2)+1)){
                                       data3<-regionsAll
                                       rm(regionsAll)
                                     } else{
                                       data1<-region17
                                       data2<-region17A
                                       rm(region17A, 
                                          region17)}}else if(i==18){if (i==(max(regionCodesOther$regionIndex2)+1)){
                                            data3<-regionsAll
                                            rm(regionsAll)
                                          } else{
                                            data1<-region18
                                            data2<-region18A
                                            rm(region18A, 
                                               region18)}}else if(i==19){if (i==(max(regionCodesOther$regionIndex2)+1)){
                                                 data3<-regionsAll
                                                 rm(regionsAll)
                                               } else{
                                                 data1<-region19
                                                 data2<-region19A
                                                 rm(region19A, 
                                                    region19)}}else if(i==20){if (i==(max(regionCodesOther$regionIndex2)+1)){
                                                      data3<-regionsAll
                                                      rm(regionsAll)
                                                    } else{
                                                      data1<-region20
                                                      data2<-region20A
                                                      rm(region20A, 
                                                         region20)}}else if(i==21){if (i==(max(regionCodesOther$regionIndex2)+1)){
                                                           data3<-regionsAll
                                                           rm(regionsAll)
                                                         } else{
                                                           data1<-region21
                                                           data2<-region21A
                                                           rm(region21A, 
                                                              region21)}}else if(i==22){if (i==(max(regionCodesOther$regionIndex2)+1)){
                                                                data3<-regionsAll
                                                                rm(regionsAll)
                                                              } else{
                                                                data1<-region22
                                                                data2<-region22A
                                                                rm(region22A, 
                                                                   region22)}}
  
  if (i<(max(regionCodesOther$regionIndex2)+1)){
    data3<-rbind(x=data1 %>% filter(!ISO %in% data2$ISO),
                 y=data2 %>% filter(year>=2000))
  }
  print(length(unique(data3$ISO)))
  data5<-merge(x=data3, y=wpp2 %>% dplyr::select(-c(regionName2, regionIndex2)), 
               by=c("ISO", "year"), all.x=TRUE)%>%
    #data5<-data4 
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

# country<-read.csv("PRETERM/output/addingSingMult_2 with predictions.csv") %>% 
#   dplyr::select(-X) %>% rename(year=time) %>% filter(year>=2000)
# #others2<-merge(x=others, y=wpp2 %>% rename(Year=year), by=c("ISO", "Year"), all.x=TRUE)
# countryNumbers<-merge(x=country, y=wpp2, 
#                       by=c("ISO", "year"), all.x=TRUE) %>% 
#   mutate(estN=round(splines1/100*wpp_lb)) %>% 
#   dplyr::select(ISO, regionName=regionName2, year,estN, wpp_lb)

regionNumbers<-estimates %>% 
  group_by(regionName2, year) %>% 
  summarise(estN=sum(estN), 
            wpp_lb=sum(wpp_lb)) %>% 
  mutate(est=estN/wpp_lb)
# globalNumbers<-estimates %>% 
#   group_by(year) %>% 
#   summarise(estN=sum(estN), 
#             wpp_lb=sum(wpp_lb)) %>% 
#   mutate(est=estN/wpp_lb)%>% 
#   mutate(regionName2="Global")


#both<-rbind(regionNumbers, globalNumbers)
both<-regionNumbers

regions<-regionCodesOther %>% dplyr::select(regionName2, regionIndex=regionIndex2) %>% 
  distinct()

regionalAndGlobal<-merge(x=all %>% dplyr::select(-c(est, estN)), y=regions, 
                         by="regionIndex", all.x=TRUE) %>% 
  mutate(regionName2=ifelse(is.na(regionName2), "Global", regionName2)) %>% 
  rename(wpp_lb=wppR)

regionalAndGlobal2<-merge(x=regionalAndGlobal, y=both %>% dplyr::select(-wpp_lb), 
                          by=c("regionName2", "year"), all.x=TRUE)%>% 
  mutate(check=ifelse(estL<est & est<estU, 0, 1))

#write.csv(regionalAndGlobal2, "PRETERM/output/Preterm regionalAndGlobal Estimates.csv")
write.csv(regionalAndGlobal2%>% 
            dplyr::select(regionName2, year, estL, est, estU, wpp_lb, 
                          estNL, estN, estNU, check),
          paste0("PRETERM/output/" ,substr(fileName, 8, nchar(fileName)),"_", 
                 ((niter-nburnin)/nthin)*nchains,"_regionalAndGlobalEstimates",
                 regionalGrouping,".csv"))

# regionalAndGlobal2<-merge(x=read.csv("PRETERM/output/pCCFullModelP_dataType1DQC1_16000_regionalAndGlobalEstimates.csv") %>% 
#                             dplyr::select(-X), y=regionCodesOther %>% 
#                             rename(regionName2=regionName) %>% 
#                             dplyr::select(regionName2, regionIndex) %>% distinct(), by="regionName2", all.x=TRUE) %>% 
#   mutate(regionIndex=ifelse(is.na(regionIndex), 7, regionIndex))

#-----------------------
# table2<-regionalAndGlobal2 %>% filter(year %in% c(2020)) %>% 
#   arrange(factor(regionIndex, levels=c(5,6,3,1,2,4,7))) %>% 
#   mutate(rates=paste0(round(est*100,1), " (", round(estL*100, 1), ", ", round(estU*100, 1), ")"),
#          numbers=paste0(round(estN), " (", round(estNL), ", ", round(estNU), ")"),
#          propWPP=round(wpp_lb/wpp_lb[n()]*100,1),
#          propPreterm=round(estN/estN[n()]*100,1)) %>% 
#   dplyr::select(regionIndex, regionName2, year, rates, numbers, wpp_lb, 
#                 propWPP, propPreterm) %>% 
#   arrange(year, regionIndex) %>% 
#   #pivot_wider(names_from="year", values_from=c("rates", "numbers")) %>% 
#   mutate(WPP=paste0(wpp_lb, " (", propWPP, ")")) %>% 
#   arrange(factor(regionIndex, levels=c(5,6,3,1,2,4,7))) %>% 
#   dplyr::select(regionIndex,regionName2, WPP, 
#                 rates, numbers, propPreterm)
# 
# #ARR Method 1 - regional and global by year
# # lbw<-regionalAndGlobal2 %>% group_by(regionName) %>% 
# #   mutate(percDiff=round(((est-lag(est))/lag(est))*100,1))
# # lbw2<-lbw %>% group_by(regionName) %>% summarise(mean=mean(percDiff, na.rm=TRUE))
# 
# #ARR Method 2 - between 2010 and 2020.
# # lbw<-regionalAndGlobal2 %>% filter(year %in% c(2010,2020)) %>%
# #   arrange(regionName2, year) %>% 
# #   group_by(regionName2) %>%
# #   mutate(ARR=round((1-(est*100/lag(est*100))^(1/10))*100,2)) %>%
# #   filter(!is.na(ARR)) %>%
# #   dplyr::select(regionName2, ARR)
# 
# #ARR Method 3 - using WHA formula
# lbw<-regionalAndGlobal2 %>% filter(year >=2010) %>% 
#   arrange(regionName2, year) %>% group_by(regionName2) %>% 
#   do(model=lm(log(est)~year, data=.)) %>% 
#   mutate(WHA_AARR=round(100*(1-exp(coef(model)["year"])),2))%>%
#   dplyr::select(regionName2, WHA_AARR)
# 
# 
# # help<-merge(x=lbw, y=lbw2, by="regionName", 
# #             all.x=TRUE)
# 
# table22<-merge(x=table2, y=lbw, 
#                by="regionName2", 
#                all.x = TRUE) %>% 
#   arrange(factor(regionIndex, levels=c(5,6,3,1,2,4,7))) %>% 
#   dplyr::select(-regionIndex)
# 
# #write.csv(table22, "PRETERM/output/Preterm table3.csv")
# write.csv(table22,
#           paste0("PRETERM/output/" ,substr(fileName, 8, nchar(fileName)),"_", 
#                  ((niter-nburnin)/nthin)*nchains,"_table2updatedARR", ".csv"))
# 
