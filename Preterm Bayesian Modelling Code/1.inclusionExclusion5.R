#INCLUSION AND EXCLUSION CRITERIA 

wpp_2021 <- suppressWarnings(readxl::read_xlsx(paste0("inputs/", wpp2)))
birth_healthfacility_who2021 <- read_dta(paste0("inputs/", birth_healthfacility))

#---Prepare birth at health facility  
fb<-subset(birth_healthfacility_who2021, (year>1999 & year<=2020) & !iso3 %in% c("PRI", "TWN"))
fb<-subset(fb, select=c(iso3, year, whoname,birth_healthfacility_sm))
fb<-dplyr::rename(fb,facility_birth=birth_healthfacility_sm)

#---WPP
wpp<-wpp_2021 %>% filter(LocTypeName=="Country/Area" & Year>1999 & Year<=2020) %>% 
  mutate(OfficialName=ifelse(LocationName=="North Macedonia", "Republic of North Macedonia",
                             ifelse(LocationName=="Micronesia (Fed. States of)", "Micronesia (Federated States of)",
                                    ifelse(LocationName=="Dem. People's Republic of Korea", 	"Democratic People's Republic of Korea",
                                           ifelse(LocationName=="Côte d'Ivoire", "Cote d'Ivoire", LocationName)))),
         wpp_lb=Total*1000)
wpp2<-merge(x=wpp, y=regionCodes, by="OfficialName", all.y=TRUE) %>% dplyr::select(iso3=ISO, year=Year, wpp_lb)


#----add onto allDataFinal
wpp_fb<-left_join(wpp2,fb,by=c("iso3","year")) %>% dplyr::rename(ISO=iso3, 
                                                                 Year=year)
pretermPlus<-left_join(pretermAll %>% mutate(Year=as.numeric(Year)), wpp_fb, by=c("ISO", "Year"))%>% 
  filter(ISO!="GUF" & ISO!="PRI" & ISO!="GRL")

#FIRST THREE INCLUSION CRITERIA

pretermInc<-pretermPlus %>% mutate(Birth_PTB=ifelse(!is.na(NumberofallLBs23), NumberofallLBs23,
                                                    ifelse(!is.na(NumberofbabieswithGA), NumberofbabieswithGA,
                                                           ifelse(is.na(NumberofallLBs23) & is.na(NumberofbabieswithGA) & !is.na(Numberoftotalbirths22), Numberoftotalbirths22, NA))),
                                   wpp_ptb=(Birth_PTB/wpp_lb)*100,
                                   cri1_ptb=ifelse(Preterm_adjusted_point_est>3.0 | Preterm_adjusted_point_est==3.0,1,
                                                   ifelse(Preterm_adjusted_point_est<3.0,0,NA)), 
                                   
                                   cri2_ptb=ifelse(Source=="Admin"&!is.na(Preterm_adjusted_point_est) & wpp_ptb>80,1, 
                                                   ifelse(Source=="Admin"&!is.na(Preterm_adjusted_point_est) & wpp_ptb==80,1,
                                                          ifelse(Source=="Admin"&!is.na(Preterm_adjusted_point_est) & wpp_ptb<80,0,NA))),  
                                   
                                   cri12_ptb=ifelse(Source=="Admin"&!is.na(Preterm_adjusted_point_est) & cri1_ptb==1 & is.na(cri2_ptb) ,0,
                                                    ifelse(Source=="Study"&!is.na(Preterm_adjusted_point_est) & cri1_ptb==1 & is.na(cri2_ptb) ,1,
                                                           ifelse(!is.na(Preterm_adjusted_point_est) & cri1_ptb==1 & cri2_ptb==1,1,
                                                                  ifelse(!is.na(Preterm_adjusted_point_est) & cri1_ptb==1 & cri2_ptb==0,2, 
                                                                         ifelse(!is.na(Preterm_adjusted_point_est) & cri1_ptb==0 & cri2_ptb==0,0,
                                                                                ifelse(!is.na(Preterm_adjusted_point_est) & cri1_ptb==0 & cri2_ptb==1,3, NA)))))))


#More inclusion criteria update
pretermInc$cri12_ptb[pretermInc$cri1_ptb==1 & is.na(pretermInc$cri2_ptb) & is.na(pretermInc$fb)]<-2
pretermInc$cri12_ptb[pretermInc$cri1_ptb==1 & is.na(pretermInc$cri2_ptb) & pretermInc$fb<80]<-2
pretermInc$cri12_ptb[pretermInc$cri1_ptb==1 & is.na(pretermInc$cri2_ptb) & pretermInc$fb>=80]<-1

#Exclude any admin data with 1 country-year with <3% 
#Countries with 1 country-year
cri3_ptb<-pretermInc %>% filter(Source=="Admin") %>% group_by(ISO) %>% 
  summarise(nCri1=sum(cri1_ptb==0)) %>% 
  mutate(cri3_ptb=ifelse(nCri1>=1, 0, 1)) %>% mutate(Source="Admin")

pretermInc3<-merge(x=pretermInc, y=cri3_ptb, by=c("ISO", "Source"), all.x=TRUE)

removed3Perc<-pretermInc3 %>% filter(Source=="Admin" & cri3_ptb==0)

#-----
#Exclude study data from countries that have admin data meeting inclusion criteria or from HIC
#for ≥50% of data years for estimation period (2010 – 2019) or HIC

regionCodesHIC=read.csv(paste0("inputs/",regionsName))%>% 
  dplyr::select(ISO3Code,WBRegion4, OfficialName) %>% rename("ISO"="ISO3Code") %>% 
  filter(WBRegion4=="High income")

admin80<-pretermInc3 %>% filter(Source=="Admin" & Year>=2010 
                                &Year<=2019 & cri12_ptb==1 & cri3_ptb==1 &
                                  is.na(Datatobeexcludedfrominputdatabase)) %>% 
  group_by(ISO) %>% 
  summarise(nPerc=n()/10) %>% filter(nPerc>=0.5) %>% 
  mutate(removeStudy=1) %>% dplyr::select(-nPerc)

allPreterm80<-merge(x=pretermInc3, y=admin80, by="ISO", all.x=TRUE) %>% 
  mutate(removeStudyHIC=ifelse(ISO %in% regionCodesHIC$ISO, 1, NA))

removedStudy2<-allPreterm80 %>% 
  filter(Source=="Study" & (removeStudy==1 | removeStudyHIC==1 | Pretermdefinition==3| cri1_ptb!=1 | !is.na(DQexclusion) )) %>% 
           filter(!(ISO=="CHN" & is.na(Representative)))
removedInc<-allPreterm80 %>% filter(is.na(cri12_ptb) | 
                                      cri12_ptb!=1 |
                                      !is.na(Datatobeexcludedfrominputdatabase)|
                                      (Source=="Admin" & (cri3_ptb!=1 | is.na(cri3_ptb))) |
                                      (Source=="Study"& (Pretermdefinition==3 | cri1_ptb!=0| !is.na(DQexclusion) )) |
                                      (ISO=="CHN" & is.na(Representative)))

saveRDS(allPreterm80,"output/pretermPreIncExc.RDS")


finalPreterm<-allPreterm80 %>% 
  filter((Source=="Study" & is.na(removeStudy) & 
            is.na(removeStudyHIC) & is.na(DQexclusion) &
            (Pretermdefinition!=3 | is.na(Pretermdefinition)) & cri1_ptb==1)| 
           (Source=="Admin" & cri3_ptb==1 & cri12_ptb==1 & 
              Year>=2010 & is.na(Datatobeexcludedfrominputdatabase)))%>% 
  filter(!(ISO=="CHN" & is.na(Representative)))

#-----------------
#Do transformations
countryIndex<-as.data.frame(list(ISO=unique(finalPreterm$ISO), 
                                 countryIndex=1:length(unique(finalPreterm$ISO))))
finalPreterm<-merge(x=finalPreterm, y=countryIndex, by="ISO", all.x=TRUE) %>%
  mutate(n=ifelse(!is.na(NumberofbabieswithGA), NumberofbabieswithGA, 
                  ifelse(!is.na(NumberofallLBs23),NumberofallLBs23,
                         ifelse(!is.na(Numberoftotalbirths22),
                                Numberoftotalbirths22, wpp_lb))),
         logitY=qlogis(y),
         logitSe=sqrt(1/(n*y*(1-y))), 
         denominatorUsed=ifelse(allGAUsed==1, "LBwithGA", 
                                ifelse(allLBUsed==1, "LBonly", 
                                       ifelse(allTBUsed==1, "TB", 
                                              ifelse(pretermRCalculated==0, "Reported", NA)))))

#Adding the number of sources
sources<-finalPreterm %>% dplyr::select(ISO, Source) %>% distinct() %>% 
  mutate(sourceIndex=ifelse(Source=="Admin", 1, 2)) %>% group_by(ISO) %>% 
  summarise(nSources=sum(sourceIndex)) %>% mutate(nSourcesLab=ifelse(nSources==1, "Only admin", 
                                                                     ifelse(nSources==2, "Only study", "Admin and Study")))


finalPreterm<-merge(x=finalPreterm, y=sources, by="ISO", all.x = TRUE)
#----

saveRDS(finalPreterm, "output/finalPreterm.RDS")
