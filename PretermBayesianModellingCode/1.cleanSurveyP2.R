

surveyP=readxl::read_excel(paste0(surveyPNameNew),
                           sheet="pretermStudies") 
  

#---------------------
#Data cleaning

surveyP2<-surveyP %>% 
  mutate(Numberoftotalbirths22=as.numeric(Numberoftotalbirths22),
         NumberofallLBs23=as.numeric(NumberofallLBs23),
         NumberofbabieswithGA=as.numeric(NumberofbabieswithGA),
         Pretermlt37weeks=as.numeric(Pretermlt37weeks), 
         Preterm32tolt37weeks=as.numeric(Preterm32tolt37weeks),
         Preterm28tolt32weeks=as.numeric(Preterm28tolt32weeks), 
         Pretermlt28weeks=as.numeric(Pretermlt28weeks),
         MissingGA=as.numeric(MissingGA),
         
         y=Preterm_adjusted_point_est/100, 
         se=y*(1-y),
         Preterm_adjusted_se=se,
         hannahExcluded=ifelse(is.na(MidyearHB), 1, 0),
         Year=ifelse(!is.na(MidyearHB), MidyearHB, YearEB),
         isoYear=paste0(IsoCode, Year)) %>% 
  rename(ISO=IsoCode) %>% 
  dplyr::select(-c(YearEB, MidyearHB,Excluded,
                   paperAvailable)) %>% 
  filter(Year>=2010 & Year<=2020) %>% 
  filter(is.na(DQexclusion))

#-----------------------
studyAll<-surveyP2

  
