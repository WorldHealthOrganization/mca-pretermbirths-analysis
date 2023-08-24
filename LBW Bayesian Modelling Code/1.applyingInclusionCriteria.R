#wpp_2021 <- read_dta(paste0("input/", wpp))
wpp_2021 <- readxl::read_xlsx(paste0("input/", wpp2))
birth_healthfacility_who2021 <- read_dta(paste0("input/", birth_healthfacility))


#---Prepare birth at health facility  
fb<-subset(birth_healthfacility_who2021, (year>1994 & year<=2020) & !iso3 %in% c("PRI", "TWN"))
fb<-subset(fb, select=c(iso3, year, whoname,birth_healthfacility_sm))
fb<-dplyr::rename(fb,facility_birth=birth_healthfacility_sm)

#---WPP
# wpp<-subset(wpp_2021, (year>1999 & year<=2020) & !iso3 %in% c("PRI", "TWN")) 
# wpp$wpp_lb<-as.numeric(wpp$wpp_lb)

wpp<-wpp_2021 %>% filter(LocTypeName=="Country/Area" & Year>1994 & Year<=2020) %>% 
  mutate(OfficialName=ifelse(LocationName=="North Macedonia", "Republic of North Macedonia",
                             ifelse(LocationName=="Micronesia (Fed. States of)", "Micronesia (Federated States of)",
                                    ifelse(LocationName=="Dem. People's Republic of Korea", 	"Democratic People's Republic of Korea",
                                           ifelse(LocationName=="Côte d'Ivoire", "Cote d'Ivoire", LocationName)))),
         wpp_lb=Total*1000)
wpp2<-merge(x=wpp, y=regionCodes, by="OfficialName", all.y=TRUE) %>% dplyr::select(iso3=ISO, year=Year, wpp_lb)

#----add onto allDataFinal
wpp_fb<-left_join(wpp2,fb,by=c("iso3","year")) %>% dplyr::rename(ISO=iso3, 
                                                         Year=year)
allDataPlus<-left_join(allDataFinal, wpp_fb, by=c("ISO", "Year"))

#test<-allDataPlus %>% filter(is.na(wpp_lb))

#-------------------------#
#---Inclusion criteria----#
#-------------------------#

 allDataPlus<-allDataPlus %>% mutate( 
                                                      #lb_lbw=ifelse(!is.na(NumberofbabieswithBW), NumberofbabieswithBW,
#                                                    ifelse(!is.na(NumberofallLBs6), NumberofallLBs6,
#                                                         ifelse(!is.na(Numberoftotalbirths5),Numberoftotalbirths5,NA))),
lb_lbw=ifelse(!is.na(NumberofbabieswithBW), NumberofbabieswithBW,
              ifelse(!is.na(NumberofallLBs6), NumberofallLBs6,NA)))

# allDataPlus<-allDataPlus %>% mutate(lb_lbw=ifelse(!is.na(NumberofallLBs6) & is.na(NumberofbabieswithBW),
#                                                     NumberofallLBs6,
#                                                     ifelse(is.na(NumberofbabieswithBW) & !is.na(Numberoftotalbirths5), 
#                                                     Numberoftotalbirths5, NumberofbabieswithBW)))


#allDataPlus<-allDataPlus %>% mutate(lb_lbw=ifelse(!is.na(NumberofbabieswithBW), NumberofbabieswithBW,
#                                                  ifelse(!is.na(NumberofallLBs6), NumberofallLBs6,
#                                                         ifelse(!is.na(Numberoftotalbirths5),Numberoftotalbirths5,NA))))



allDataInc<-allDataPlus %>% 
  mutate(wpp_lbw=(lb_lbw/wpp_lb)*100,                   ## Calculate % of estimated wpp livebirth
         cri1_lbw=ifelse((LBW_adjusted_point_est>2.1 | LBW_adjusted_point_est==2.1) & LBW_adjusted_point_est<40,1,
                         ifelse(LBW_adjusted_point_est<2.1,0,NA)), ## Create a variable to flag implausibility criteria
         cri2_lbw=ifelse(wpp_lbw>80 | wpp_lbw==80,1,
                         ifelse(wpp_lbw<80,0,NA)),      ## Create a variable to flag 80% wpp threshold
         cri12_lbw=ifelse(cri1_lbw==1 & is.na(cri2_lbw) & Source=="Survey",1,    #keep surveys in
                          ifelse(cri1_lbw==0 & is.na(cri2_lbw)& Source=="Survey",0,
                          ifelse(cri1_lbw==1 & is.na(cri2_lbw) & Source=="Admin" & facility_birth>=80,1, #admin missing births only included if >=80% FB
                          ifelse(is.na(cri2_lbw) & Source=="Admin" & facility_birth<80,0, #admin missing births excluded included if >=80% FB
                          ifelse(cri1_lbw==1 & cri2_lbw==1,1,
                          ifelse(cri1_lbw==1 & cri2_lbw==0,2, NA)))))))   ## Create a variable to flag 80% wpp threshold


#saveRDS(allDataInc, "output/allDataInc.RDS")
removedInc<-allDataInc %>% filter(is.na(cri12_lbw) | cri12_lbw!=1)
#allDataFinal1<-allDataInc %>% filter(cri12_lbw==1 & !(ISO=="FRA" & Year==2020))
allDataFinal1<-allDataInc %>% filter(cri12_lbw==1)
allDataFinal1 %>% group_by(Source) %>% summarise(n=n())


#help<-allDataFinal1 %>% filter(ISO=="TKM" | ISO=="NAM")


help<-allDataInc %>% filter(is.na(lb_lbw) & Source=="Admin"&facility_birth>=80&sourceSubType==3) %>%  
  dplyr::select(ISO, OfficialName, Year,NumberofallLBs6, 
                #Numberoftotalbirths5,
         NumberofbabieswithBW, LBWlt2500g, LBW_adjusted_point_est, Source, sourceSubType, wpp_lb, wpp_lbw, facility_birth,
         cri1_lbw, cri2_lbw, cri12_lbw) %>% 
   mutate(sourceSubType=ifelse(sourceSubType==3, "HMIS(DHIS2) (primarily for low-and middle-income countries)", sourceSubType))
#write.csv(help, "output/adminWithoutBirthEstFB80HMIS.csv")



#FINALISE
#----

allDataFinal1 <-allDataFinal1 %>% arrange(ISO, Year)

#Indexing time, countries and region
Year<-unique(allDataFinal1$Year)
yindex<-as.data.frame(Year)
yindex$yearIndex<-c(1:nrow(yindex))

ISO<-sort(unique(allDataFinal1$ISO))
index<-as.data.frame(ISO)
index$countryIndex<-c(1:nrow(index))

combine<-merge(x=allDataFinal1, y=index, by="ISO", all.x = TRUE)
finalData<-merge(x=combine, y=yindex, by="Year", all.x = TRUE) %>% arrange(countryIndex, yearIndex) 
#%>% 
#  mutate(defInc25=ifelse(LBWdefinition==2 & !is.na(LBWdefinition), 1, 0))
  
 allDataFinal %>% group_by(Source) %>% summarise(n=n())
 
#------------------------------------------------------------------
#SMOOTHING SMALL COUNTRIES - 3-year moving average
 
finalData<-finalData %>% mutate(n=ifelse(!is.na(NumberofbabieswithBW), NumberofbabieswithBW, 
                                  ifelse(!is.na(NumberofallLBs6),NumberofallLBs6,
                                  #ifelse(!is.na(Numberoftotalbirths5),Numberoftotalbirths5, 
                                         wpp_lb))) %>% #USING THE WPP FOR N IF THERE IS NO N
  group_by(ISO, Source) %>% #averages by country and source
   mutate(wpp_lbwMean=mean(wpp_lbw, na.rm=TRUE),
          meanN=mean(n, na.rm=TRUE),
          y=LBW_adjusted_point_est/100,
          yL=lead(y), 
          yL2=lead(lead(y))) %>% 
   rowwise() %>% mutate(meanY=ifelse(!is.na(mean(c(y, yL, yL))), mean(c(y, yL, yL)), y), #setting mean of the next 3 (if no next 3 then use y)
                        y=ifelse(meanN<10000| is.nan(meanN), meanY, y)) 
 
 #%>% 
  # arrange(ISO, Source) %>% dplyr::select(ISO, Source,meanN, y, yL, yL2, meanY, y2) 
          
 
#Add variance - check this. Below is for the log.
#- assume a Poisson data-generating process - y=LBW/all, LBW|theta~Poisson(all x theta)
#then var=D/B^2, using delta var(logit(y))=y/B(y-y^2)^2
#var(log(y))=1/(b*y)
# For the logit and binomial distribution: y~bin(n, p) ~~ y~N(np, np(1-p)). By CLT, sqrt(n)[y/n-p]->N(0, p(1-p)).
#Using delta method: N(0, p(1-p)*(g'(p))^2). Asymptotic variance (as the variance of logit(y/n) does not exist as y can be zero) - add the sqrt(n) back in.
#g(p)=logit(p), g'(p)=1/(p(1-p))^2 => var(logit(p))=1/np(1-p)                                                
#https://en.wikipedia.org/wiki/Delta_method
#https://math.stackexchange.com/questions/1598503/example-of-using-delta-method 
  
finalData<-finalData %>% mutate(logitY=qlogis(y),
                                #logitYSurvey=ifelse(Source=="Survey",qlogis(y) + (se^2/2)*((1-2*y)/(y-y^2))^2, NA),
                                #logitSe=1/(n*y*(1-y)),
                                logitSe=ifelse(Source=="Survey", sqrt(se^2*(1/(y-y^2))^2), 
                                                     ifelse(Source=="Admin", sqrt(1/(n*y*(1-y))), NA)),
                                logY100=log(y*100),
                                logY=log(y),
                                # logSe=ifelse(!is.na(NumberofbabieswithBW), 1/(NumberofbabieswithBW*y), 
                                #              ifelse(!is.na(NumberofallLBs6),1/(NumberofallLBs6*y),
                                #              ifelse(!is.na(Numberoftotalbirths5),
                                #               1/(Numberoftotalbirths5*y), 1/(wpp_lb*y))))
                                logSe=ifelse(!is.na(NumberofbabieswithBW), 1/(NumberofbabieswithBW*y), 
                                             ifelse(!is.na(NumberofallLBs6),1/(NumberofallLBs6*y),
                                                     1/(wpp_lb*y)))) %>% 
                        mutate(qualityHierarchy=ifelse(Source=="Admin" & (wpp_lbwMean<=95 | is.nan(wpp_lbwMean)), 3,
                                                ifelse(Source=="Admin"& wpp_lbwMean>95 , 1,
                                                ifelse(Source=="Survey", 2,99)))) 

finalData %>% group_by(Source) %>% summarise(meanY=mean(y), 
                                             meanLogitY=mean(logitY), 
                                             meanLogitSe=mean(logitSe),
                                             meanLogY=mean(logY),
                                             meanLoY100=mean(logY100),
                                             meanLogSe=mean(logSe),
                                             meanSE=mean(LBW_adjusted_se)
                                             ) 
finalData %>% group_by(qualityHierarchy) %>% summarise(n=n())

help<-finalData %>% dplyr::select(isoYear, y, yL, yL2, meanN, meanY)

#Save
#saveRDS(finalData, "output/finalData.rds")

#----
#Regional codes dataset
regionCodesAll<-finalData %>% ungroup() %>% dplyr::select(ISO, OfficialName, countryIndex, regionName, regionIndex) %>% 
  distinct()


