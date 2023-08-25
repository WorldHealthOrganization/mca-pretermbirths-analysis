##Inputting the Survey data

surveyBefore=read_excel(paste0(surveyName), 
                  sheet = "Estimates_final")

surveyBefore<-tidyVariableNames(surveyBefore)

#Tidying variables
surveyBefore<-suppressWarnings(surveyBefore %>% rename(
                          Year=`MidtopointforLBWestimate`,
                          iso=`ISOCode`,
                          Countryname=Country, sourceSubTypeOther=Survey) %>% 
  dplyr::select(iso,Countryname,Year, LBW_adjusted_point_est,
         LBW_adjusted_se, sourceSubTypeOther, Crude_LBW_point_est, Crude_LBW_se) %>%
  mutate(Year=as.numeric(Year)) %>% 
  mutate(LBW_adjusted_point_est=LBW_adjusted_point_est*100,
         Crude_LBW_point_est=as.numeric(Crude_LBW_point_est)*100,
         LBW_adjusted_se=as.numeric(LBW_adjusted_se),
         Crude_LBW_se=as.numeric(Crude_LBW_se)) %>% 
  mutate(Source="Survey", sourceSubType=ifelse(sourceSubTypeOther=="DHS", 5,
                                               ifelse(sourceSubTypeOther=="MICS", 6,
                                                      7))) %>% 
  mutate(isoYear=paste0(iso, Year)))
surveyBefore$sourceSubType<-as.numeric(surveyBefore$sourceSubType)

surveyBefore<-surveyBefore %>% filter(iso!="XKX")

#Removing other years
surveyBefore<-surveyBefore %>% filter(Year>=1995 & Year<=2020)

#Remove NAs
removedSurvey<-surveyBefore %>% filter(is.na(LBW_adjusted_point_est))
surveyAll<-surveyBefore %>% filter(!is.na(LBW_adjusted_point_est))

#---adding in se for those without using median of same country SEs
without<-surveyAll %>% filter(is.na(LBW_adjusted_se)) %>% dplyr::select(-LBW_adjusted_se)
with<-surveyAll %>% filter(!is.na(LBW_adjusted_se) & 
                             iso %in% unique(without$iso)) %>% 
  group_by(iso) %>% summarise(LBW_adjusted_se=median(LBW_adjusted_se))

without2<-merge(x=without, y=with, by="iso", all.x=TRUE)

with2<-surveyAll %>% filter(!is.na(LBW_adjusted_se))
surveyAll<-rbind(with2,without2)


#-------------

surveyAll<-surveyAll %>% mutate(y=LBW_adjusted_point_est/100) %>% 
  mutate(se=LBW_adjusted_se)

