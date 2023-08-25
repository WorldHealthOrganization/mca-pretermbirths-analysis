#PREDICT USING WHOLE ITERATIONS

source("0.loadPackages.R")
source("0.fileNames.R")

#Takes the SDG regions (rather than the modified regions used in the modelling)
regionCodesOther<-readRDS("input/regionCodesOther.rds") %>% 
  rename(regionName2=regionName, regionIndex2=regionIndex)

#WPP Livebirths estimates
wpp_2021 <- suppressWarnings(readxl::read_xlsx(paste0("input/", wpp2)))
wpp<-wpp_2021 %>% filter(LocTypeName=="Country/Area" & Year>1994 & Year<=2020) %>% 
  mutate(OfficialName=ifelse(LocationName=="North Macedonia", "Republic of North Macedonia",
                             ifelse(LocationName=="Micronesia (Fed. States of)", "Micronesia (Federated States of)",
                                    ifelse(LocationName=="Dem. People's Republic of Korea", 	"Democratic People's Republic of Korea",
                                           ifelse(LocationName=="Côte d'Ivoire", "Cote d'Ivoire", LocationName)))),
         wpp_lb=Total*1000)
wpp2<-merge(x=wpp, y=regionCodesOther, by="OfficialName", all.y=TRUE) %>% 
  dplyr::select(ISO, year=Year, wpp_lb,regionName2, regionIndex2)
rm(wpp_2021, wpp)

#------------------------------
finalData<-readRDS("output/LBWfinalInputDatabase.RDS")

alpha.level=0.05
undo<-function(data){
  data<-as.data.frame(data)
  long<-gather(data, "chain", "name") %>% dplyr::select(-chain)
  return(long)
}

#-----
#Coeffs - getting the coefficients from the model for each iteration

#Covariate coefficients
alpha1<-undo(mod$BUGSoutput$sims.array[,,"alpha[1]"]) %>% rename(alpha1=name)%>% mutate(row=row_number())
alpha2<-undo(mod$BUGSoutput$sims.array[,,"alpha[2]"]) %>% rename(alpha2=name)
alpha3<-undo(mod$BUGSoutput$sims.array[,,"alpha[3]"]) %>% rename(alpha3=name)
alpha4<-undo(mod$BUGSoutput$sims.array[,,"alpha[4]"]) %>% rename(alpha4=name)
alpha5<-undo(mod$BUGSoutput$sims.array[,,"alpha[5]"]) %>% rename(alpha5=name)

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


coeffs<-cbind(alpha1, alpha2, 
              alpha3, alpha4, alpha5, 
              beta1, beta2, beta3, beta4, beta5, beta6)

#----
#Getting the time element of the penalised splines section

Z<-list()
for (i in 1:26){
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
yearIndex<-as.data.frame(list(year=1995:2020, yearIndex=1:26))

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

#--------------
#PRINTING ALL THE COVARIATE COEFFICIENTS
alpha1P<-as.data.frame(list(covariate="GNI", coefficient="alpha[1]", median=median(alpha1$alpha1),
                            mean=NA, CI=NA, Direction="", expectedDirection="", difference=""))
alpha2P<-as.data.frame(list(covariate="Literacy Fem", coefficient="alpha[2]", median=median(alpha2$alpha2),
                            mean=NA, CI=NA, Direction="", expectedDirection="", difference=""))
alpha3P<-as.data.frame(list(covariate="Contraceptive method", coefficient="alpha[3]", median=median(alpha3$alpha3),
                            mean=NA, CI=NA, Direction="", expectedDirection="", difference=""))
alpha4P<-as.data.frame(list(covariate="Female underweight", coefficient="alpha[4]", median=median(alpha4$alpha4),
                            mean=NA, CI=NA, Direction="", expectedDirection="", difference=""))
alpha5P<-as.data.frame(list(covariate="%Urban", coefficient="alpha[5]", median=median(alpha5$alpha5),
                            mean=NA, CI=NA, Direction="", expectedDirection="", difference=""))

alpha1Inv<-as.data.frame(list(covariate="GNI",
                              coefficient="exp alpha[1]", 
                              median=median(exp(alpha1$alpha1)), 
                              mean=mean(exp(alpha1$alpha1)),
                              CI=paste0("(",exp(mean(alpha1$alpha1)-1.96*sd(alpha1$alpha1)), 
                                        " , ", 
                                        exp(mean(alpha1$alpha1)+1.96*sd(alpha1$alpha1)),
                                        ")"),
                              Direction=ifelse(mean(exp(alpha1$alpha1))>1, "increase", "decrease"),
                              expectedDirection="increase", 
                              difference=ifelse((ifelse(mean(exp(alpha1$alpha1))>1, "increase", "decrease"))=="increase", 
                                                "as expected", "opposite")
                              
))

alpha2Inv<-as.data.frame(list(covariate="Literacy Fem",
                              coefficient="exp alpha[2]", 
                              median=median(exp(alpha2$alpha2)), 
                              mean=mean(exp(alpha2$alpha2)),
                              CI=paste0("(",exp(mean(alpha2$alpha2)-1.96*sd(alpha2$alpha2)), 
                                        " , ", 
                                        exp(mean(alpha2$alpha2)+1.96*sd(alpha2$alpha2)),
                                        ")"),
                              Direction=ifelse(mean(exp(alpha2$alpha2))>1, "increase", "decrease"),
                              expectedDirection="decrease", 
                              difference=ifelse((ifelse(mean(exp(alpha2$alpha2))>1, "increase", "decrease"))=="decrease", 
                                                "as expected", "opposite")
                              
))

alpha3Inv<-as.data.frame(list(covariate="Contraceptive method",
                              coefficient="exp alpha[3]", 
                              median=median(exp(alpha3$alpha3)), 
                              mean=mean(exp(alpha3$alpha3)),
                              CI=paste0("(",exp(mean(alpha3$alpha3)-1.96*sd(alpha3$alpha3)), 
                                        " , ", 
                                        exp(mean(alpha3$alpha3)+1.96*sd(alpha3$alpha3)),
                                        ")"),
                              Direction=ifelse(mean(exp(alpha3$alpha3))>1, "increase", "decrease"),
                              expectedDirection="increase", 
                              difference=ifelse((ifelse(mean(exp(alpha3$alpha3))>1, "increase", "decrease"))=="increase", 
                                                "as expected", "opposite")
                              
))

alpha4Inv<-as.data.frame(list(covariate="Female underweight",
                              coefficient="exp alpha[4]", 
                              median=median(exp(alpha4$alpha4)), 
                              mean=mean(exp(alpha4$alpha4)),
                              CI=paste0("(",exp(mean(alpha4$alpha4)-1.96*sd(alpha4$alpha4)), 
                                        " , ", 
                                        exp(mean(alpha4$alpha4)+1.96*sd(alpha4$alpha4)),
                                        ")"),
                              Direction=ifelse(mean(exp(alpha4$alpha4))>1, "increase", "decrease"),
                              expectedDirection="decrease", 
                              difference=ifelse((ifelse(mean(exp(alpha4$alpha4))>1, "increase", "decrease"))=="decrease", 
                                                "as expected", "opposite")
                              
))

alpha5Inv<-as.data.frame(list(covariate="%Urban",
                              coefficient="exp alpha[5]", 
                              median=median(exp(alpha5$alpha5)), 
                              mean=mean(exp(alpha5$alpha5)),
                              CI=paste0("(",exp(mean(alpha5$alpha5)-1.96*sd(alpha5$alpha5)), 
                                        " , ", 
                                        exp(mean(alpha5$alpha5)+1.96*sd(alpha5$alpha5)),
                                        ")"),
                              Direction=ifelse(mean(exp(alpha5$alpha5))>1, "increase", "decrease"),
                              expectedDirection="decrease", 
                              difference=ifelse((ifelse(mean(exp(alpha5$alpha5))>1, "increase", "decrease"))=="decrease", 
                                                "as expected", "opposite")
                              
))
#These are the regional intercepts 
beta1P<-as.data.frame(list(covariate="Southern Asia",coefficient="inv.logit mu.beta[1]", 
                           median=median(inv.logit(mod$BUGSoutput$sims.array[,,"mu.beta[1]"])), mean=NA, CI=NA, Direction="", expectedDirection="", difference=""))
beta2P<-as.data.frame(list(covariate="Sub-Saharan Africa",coefficient="inv.logit mu.beta[2]", 
                           median=median(inv.logit(mod$BUGSoutput$sims.array[,,"mu.beta[2]"])),
                           mean=NA, CI=NA, Direction="", expectedDirection="", difference=""))
beta3P<-as.data.frame(list(covariate="Northern America, Australia and New Zealand, Central Asia and Europe",
                           coefficient="mu.beta[3]", median=median(inv.logit(mod$BUGSoutput$sims.array[,,"mu.beta[3]"])),
                           mean=NA, CI=NA, Direction="", expectedDirection="", difference=""))
beta4P<-as.data.frame(list(covariate="Western Asia and Northern Africa",coefficient="inv.logit mu.beta[4]", 
                           median=median(inv.logit(mod$BUGSoutput$sims.array[,,"mu.beta[4]"])),
                           mean=NA, CI=NA, Direction="", expectedDirection="", difference=""))
beta5P<-as.data.frame(list(covariate="Latin America and the Caribbean",coefficient="inv.logit mu.beta[5]", 
                           median=median(inv.logit(mod$BUGSoutput$sims.array[,,"mu.beta[5]"])),
                           mean=NA, CI=NA, Direction="", expectedDirection="", difference=""))
beta6P<-as.data.frame(list(covariate="Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)",
                           coefficient="inv.logit mu.beta[6]", median=median(inv.logit(mod$BUGSoutput$sims.array[,,"mu.beta[6]"])),
                           mean=NA, CI=NA, Direction="", expectedDirection="", difference=""))


#Bias terms
bias1<-as.data.frame(list(coefficient="bias1", median=median(mod$BUGSoutput$sims.array[,,"bias1"]), 
                          mean=NA, CI=NA, Direction="", expectedDirection="", difference="", covariate="DQC A"))
bias2<-as.data.frame(list(coefficient="bias2", median=median(mod$BUGSoutput$sims.array[,,"bias2"]), 
                          mean=NA, CI=NA, Direction="", expectedDirection="", difference="", covariate="DQC B"))
bias3<-as.data.frame(list(coefficient="bias3", median=median(mod$BUGSoutput$sims.array[,,"bias3"]), 
                          mean=NA, CI=NA, Direction="", expectedDirection="", difference="", covariate="DQC C"))

#Additional variance terms
sigma1<-as.data.frame(list(coefficient="sigma1", median=median(mod$BUGSoutput$sims.array[,,"sigma1"]), 
                           mean=NA, CI=NA, Direction="", expectedDirection="", difference="", covariate="DQC A"))
sigma2A<-as.data.frame(list(coefficient="sigma2A", median=median(mod$BUGSoutput$sims.array[,,"sigma2A"]), 
                            mean=NA, CI=NA, Direction="", expectedDirection="", difference="", covariate="DQC B"))
sigma2S<-as.data.frame(list(coefficient="sigma2S", median=median(mod$BUGSoutput$sims.array[,,"sigma2S"]), 
                            mean=NA, CI=NA, Direction="", expectedDirection="", difference="", covariate="Surveys"))
sigma3<-as.data.frame(list(coefficient="sigma3", median=median(mod$BUGSoutput$sims.array[,,"sigma3"]), 
                           mean=NA, CI=NA, Direction="", expectedDirection="", difference="", covariate="DQC C"))

coeffsAll<-rbind(alpha1P, alpha2P, 
              alpha3P, alpha4P, alpha5P, 
              alpha1Inv, alpha2Inv, 
              alpha3Inv, alpha4Inv, alpha5Inv,
              beta1P, beta2P, beta3P, 
              beta4P, beta5P, beta6P,
              bias1, bias2, bias3,
              sigma1, sigma2A, sigma2S, 
              sigma3)

write.csv(coeffsAll,
          paste0("output/" ,fileName,"_", 
                 ((niter-nburnin)/nthin)*nchains,"_Coefficients", ".csv"))

#------------------
#adding all together, rows for each iteration.

allCovars<-cbind(coeffs, cbind(Zbig, delta)) %>% dplyr::select(-id)
rm(alpha1, alpha2, alpha3, alpha4, alpha5, 
   alpha1Inv, alpha2Inv, alpha3Inv, alpha4Inv, alpha5Inv, 
   b, b1, b2, b3, 
   beta1, beta2, beta3, beta4, beta5, beta6,
   bMean, coeffs, z, Z, Zbig,
   beta1P, beta2P, beta3P, 
   beta4P, beta5P, beta6P,
   bias1, bias2, bias3,
   sigma1, sigma2A, sigma2S, 
   sigma3)
#-----------
#adding for each of the countries

regionCodes<-readRDS("output/regionCodes.RDS")
regionCodesOther<-readRDS("output/regionCodesOther.rds") %>% 
  rename(regionName2=regionName, regionIndex2=regionIndex) 

#Getting the countries which have no predictions
toPred<-anti_join(regionCodes, finalData, by=c("ISO"))

#Covariate values
covarset.raw<-readRDS("input/LBWcovariate1995.RDS") %>% rename(iso=iso3) %>% 
  dplyr::select(iso, whoname, year, abr=abr_sm, 
                anc4=anc4_sm, sab=sab_sm, birth_healthfacility=birth_healthfacility_sm,
                csection=csection_sm, contraception_met=contraception_met_sm, 
                edu_mean_f=edu_mean_f_sm, literacy_fem=literacy_fem_sm, 
                bmi_f_over=bmi_f_over_sm,
                bmi_f_mean=bmi_f_mean_sm,
                bmi_f_under=bmi_f_under_sm, anaemia=anaemia_sm, 
                smoking=smoking_sm, gfr=gfr_sm, tfr=tfr_sm, hiv_wra=hiv_wra_sm, 
                underweight=underweight_sm, stunting=stunting_sm, 
                mmr=mmr_sm, nmr=nmr_sm, pfpr=pfpr_sm, ITN=ITN, 
                urban=urban_sm, gni=gni_sm)
int_cov <- c("gni","literacy_fem","contraception_met","bmi_f_under", 
             "urban", "nmr"
)

covarset <- covarset.raw %>% dplyr::select(c("iso", "year", int_cov)) %>%  
  dplyr::rename(ISO=iso)
transform<-covarset %>% mutate(gniT=log(gni),
                               literacy_femT=ifelse(literacy_fem>100, log(100-99.99999), log(100-literacy_fem)),
                               contraception_metT=log(1-contraception_met),
                               bmi_f_underT=log(bmi_f_under),
                               urbanT=urban
)


#centring the covariates
covarsetCentred<-transform %>% mutate(gniC=gniT-mean(transform$gniT, na.rm = TRUE),
                                      literacy_femC=literacy_femT-mean(transform$literacy_femT, na.rm=TRUE),
                                      contraception_metC=contraception_metT-mean(transform$contraception_metT, na.rm=TRUE),
                                      bmi_f_underC=bmi_f_underT-mean(transform$bmi_f_underT, na.rm=TRUE),
                                      urbanC=urbanT-mean(transform$urbanT, na.rm=TRUE)
)

int_cov <- paste0(int_cov, "C")
toPred2<-merge(x=merge(x=covarsetCentred, y=toPred,by=c("ISO"), all.y=TRUE),
               y=regionCodesOther %>% 
                 dplyr::select(-OfficialName), by="ISO", all.x=TRUE)

isos2<-unique(toPred2$ISO)
nIsos2<-length(isos2)

predict<-c()
region1<-as.data.frame(list(ISO=NA, year=NA, mu=NA))
region2<-as.data.frame(list(ISO=NA, year=NA, mu=NA))
region3<-as.data.frame(list(ISO=NA, year=NA, mu=NA))
region4<-as.data.frame(list(ISO=NA, year=NA, mu=NA))
region5<-as.data.frame(list(ISO=NA, year=NA, mu=NA))
region6<-as.data.frame(list(ISO=NA, year=NA, mu=NA))
for (i in 1:nIsos2){
  
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
  for (j in 1:26){
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
                                   
                                   mu=gniC*alpha1+literacy_femC*alpha2+
                                     contraception_metC*alpha3+bmi_f_underC*alpha4+
                                     urbanC*alpha5+
                                     beta.d+time) %>% dplyr::select(ISO, year, mu) %>% 
    filter(year>=2000)
  
  #Imposing the upper bounds
  cPredList2<-cPredList2 %>% mutate(mu=ifelse(ISO %in% c("AFG", "PAK") & mu<logit(0.021), 
                                              logit(0.021), 
                                              ifelse(ISO %in% c("AFG", "PAK") & mu>logit(0.4), 
                                                     logit(0.4), mu)))
  
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

rates<-rbind(allData, predict2)%>% 
  filter(if_any(everything(), ~ !is.na(.))) %>% 
  filter(year>=2000)

#Getting the numbers of the LBW from the estimate rates and the WPP livebirths
estimates<-merge(x=rates, wpp2, 
               by=c("ISO", "year"), 
               all.x=TRUE) %>% 
  mutate(estN=round((est/100)*wpp_lb), 
         estNL=round((estL/100)*wpp_lb),
         estNU=round((estU/100)*wpp_lb))
         
write.csv(estimates,
          paste0("output/" ,fileName,"_", 
                 ((niter-nburnin)/nthin)*nchains,"_Estimates", ".csv"))
rm(predict, cPredList, cPred, cPred2, 
   cPred3, cPredList, cPredList2, cPredList3,
   cPredTime, 
   cPredTime2, d, allCovars)
#------------------------------------
#Getting regional and global estimates

#Getting the iterations of the countries with data from the model
# and outputting these to a regional dataset.

mu<-mod[["BUGSoutput"]][["sims.list"]][["mu.ct"]]
isos<-finalData %>% dplyr::select(ISO, regionIndex, countryIndex) %>% distinct()
isos1<-merge(x=isos, y=regionCodesOther %>% 
               dplyr::select(-OfficialName), by="ISO", all.x=TRUE)
for (r in 1:6){
  isos2<-isos1 %>% filter(regionIndex2==r)
  for (i in 1:nrow(isos2)){
    for (j in 1:dim(mu)[3]){
      datas<-as.data.frame(list(ISO=rep(isos2$ISO[i], dim(mu)[1]), 
                                year=rep(1994+j, dim(mu)[1]), 
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

rm(mod, otherPs)

#------------------------
#REGIONS

#Summarising the country-level iterations across regions, to get regional estimates
# iterations, then taking the quantiles to get the credible intervals of the estimates.
#Finally adding all the country-level iterations together and summarising to get
# the global level estimates.
for (i in 1:7){
  
  if(i==1){ data1<-region1
  data2<-region1A
  rm(region1A, region1)
  } else if (i==2){ data1<-region2
  data2<-region2A
  rm(region2A, region2)
  } else if (i==3){data1<-region3
  data2<-region3A
  rm(region3A, region3)
  } else if (i==4){ data1<-region4
  data2<-region4A
  rm(region4A, region4)
  }else if (i==5){ data1<-region5
  data2<-region5A
  rm(region5A, region5)
  }else if (i==6){ data1<-region6
  data2<-region6A
  rm(region6A, region6)
  }else if(i==7){
    data3<-regionsAll
    rm(regionsAll)
  }
  
  if (i<7){
    data3<-rbind(x=data1 %>% filter(!ISO %in% data2$ISO),
                 y=data2 %>% filter(year>=2000))
    data3<-merge(x=data3, y=wpp2 %>% filter(year>=2000) %>% dplyr::select(-c(regionName2, regionIndex2)), 
                 by=c("ISO", "year"), all.x=TRUE)
  }
  print(length(unique(data3$ISO)))

  
  # if (i==7){
  #   data3 <- data3 %>% 
  #     rename("wpp_lb" = "wpp_lb.x")
  #   data3 <- data3 %>% 
  #     dplyr::select(-wpp_lb.y)
  # }
  
  data5<-data3 %>% filter(!is.na(ISO)) %>% 
    mutate(numbers=round(wpp_lb*inv.logit(mu),0)) %>%
    group_by(ISO,year) %>% mutate(iteration=row_number()) %>% 
    group_by(year, iteration) %>% 
    summarise(regionNumbers=sum(numbers),
              wppR=sum(wpp_lb),
              ISO=length(unique(ISO)))  %>% 
    ungroup() %>% 
    group_by(year) %>% 
    summarise(estNL=quantile(regionNumbers, 0.025, na.rm=TRUE), 
              estN=quantile(regionNumbers, 0.5, na.rm=TRUE), 
              estNU=quantile(regionNumbers, 0.975, na.rm=TRUE),
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
countryNumbers<-estimates %>% 
  dplyr::select(ISO, regionName=regionName2, year,estN, wpp_lb)

regionNumbers<-countryNumbers %>% 
  group_by(regionName, year) %>% 
  summarise(estN=sum(estN), 
            wpp_lb=sum(wpp_lb)) %>% 
  mutate(est=estN/wpp_lb) %>% 
  dplyr::select(-wpp_lb)
globalNumbers<-countryNumbers %>% 
  group_by(year) %>% 
  summarise(estN=sum(estN), 
            wpp_lb=sum(wpp_lb)) %>% 
  mutate(est=estN/wpp_lb)%>% 
  mutate(regionName="Global")


both<-rbind(regionNumbers, globalNumbers)

#Adding the credible intervals calculated above to the summed estimates.
regions<-regionCodes %>% dplyr::select(regionName, regionIndex) %>% 
  distinct()

regionalAndGlobal<-merge(x=all %>% 
                           dplyr::select(-c(est, estN)) %>% 
                           rename(wpp_lb=wppR) , 
                         y=regions, 
                         by="regionIndex", all.x=TRUE) %>% 
  mutate(regionName=ifelse(is.na(regionName), "Global", regionName))

regionalAndGlobal2<-merge(x=regionalAndGlobal, y=both, 
                          by=c("regionName", "year"), all.x=TRUE) %>% 
  mutate(wpp_lb=ifelse(is.na(wpp_lb.x), wpp_lb.y, wpp_lb.x)) %>% 
  dplyr::select(-c(wpp_lb.x, wpp_lb.y))


write.csv(regionalAndGlobal2%>% 
            dplyr::select(regionName, year, estL, est, estU, wpp_lb, 
                          estNL, estN, estNU),
          paste0("output/" ,fileName,"_", 
                 ((niter-nburnin)/nthin)*nchains,"_regionalAndGlobalEstimates", ".csv"))

#-------------------------------------------------------------
#Output tables for the paper

table2<-regionalAndGlobal2 %>% filter(year %in% c(2000, 2020)) %>% 
  mutate(rates=paste0(round(est*100,1), " (", round(estL*100, 1), ", ", round(estU*100, 1), ")"),
         numbers=paste0(round(estN), " (", round(estNL), ", ", round(estNU), ")")) %>% 
  dplyr::select(regionIndex, regionName, year, rates, numbers) %>% 
  arrange(year, regionIndex) %>% 
  pivot_wider(names_from="year", values_from=c("rates", "numbers")) %>% 
  arrange(factor(regionIndex, levels=c(5,6,3,1,2,4,7))) %>% 
  dplyr::select(regionName, regionIndex, rates_2000, numbers_2000, rates_2020, numbers_2020)

#ARR Method 1 - regional and global by year
# lbw<-regionalAndGlobal2 %>% group_by(regionName) %>% 
#   mutate(percDiff=round(((est-lag(est))/lag(est))*100,1))
# lbw2<-lbw %>% group_by(regionName) %>% summarise(mean=mean(percDiff, na.rm=TRUE))

#ARR Method 2 - between 2000 and 2020.
arr<-regionalAndGlobal2%>% filter(year %in% c(2000,2020)) %>% 
  group_by(regionName) %>% 
  mutate(ARR=(1-(est/lag(est))^(1/20))*100) %>% 
  filter(!is.na(ARR)) %>% 
  dplyr::select(regionName, ARR)

table22<-merge(x=table2, y=arr, 
               by="regionName", 
               all.x = TRUE) %>% 
  arrange(factor(regionIndex, levels=c(5,6,3,1,2,4,7)))

write.csv(table22,
          paste0("output/" ,fileName,"_", 
                 ((niter-nburnin)/nthin)*nchains,"_table2", ".csv"))

