finalPreterm1<-readRDS("output/finalPreterm.RDS")

#-----------------------------
#Missing source sub type- replacing with the LBW one from the same database
lbw<-readRDS("inputs/LBWfinalData.rds") %>% filter(Source=="Admin") %>% 
  dplyr::select(isoYear, sourceSubType) 

adminCodes=readxl::read_excel(paste0(adminName), 
                              sheet = "VN Source database", skip=1)
adminCodes<-tidyVariableNames(adminCodes) %>% mutate(isoYear=paste0(IsoCode, MidYear))



missing<-finalPreterm1 %>% filter(Source=="Admin" & (is.na(sourceSubType) | sourceSubType=="EuroStat")) %>% 
  dplyr::select(OfficialName, Year, sourceSubType, isoYear)

missing2<-merge(x=missing, y=adminCodes, by="isoYear", all.x=TRUE) %>% 
  dplyr::select(OfficialName, Year, sourceSubTypePreterm=sourceSubType, SourcetypeforLBLBW, isoYear)

missing3<-merge(x=missing2, y=lbw, by="isoYear", all.x=TRUE) %>% 
  dplyr::select(OfficialName, Year, sourceSubTypePreterm, sourceSubTypeLBWsource=SourcetypeforLBLBW, sourceSubTypeLBWupdated=sourceSubType, isoYear)

# write.csv(missing3, "output/missingSourceSubType.csv")

missing4<-missing3 %>% dplyr::select(isoYear, sourceSubTypeLBWsource,
                                     sourceSubTypeLBWupdated) %>% mutate(Source="Admin")

finalPreterm1<-merge(x=finalPreterm1, y=missing4, by=c("isoYear", "Source"), all.x=TRUE) 
finalPreterm1<-finalPreterm1 %>% 
  mutate(sourceSubType=ifelse((is.na(sourceSubType) | sourceSubType=="EuroStat") & !is.na(sourceSubTypeLBWupdated), 
                              sourceSubTypeLBWupdated, 
                              ifelse((is.na(sourceSubType) | sourceSubType=="EuroStat") & is.na(sourceSubTypeLBWupdated), 
                                     sourceSubTypeLBWsource, sourceSubType))) %>% distinct()
#------------------------------
#Split the two sources
admins<-finalPreterm1 %>% filter(Source=="Admin") %>% 
  mutate(Preterm28tolt32weeks=ifelse(as.numeric(Preterm28tolt32weeks)==0, NA, 
                                     as.numeric(Preterm28tolt32weeks)),
         Pretermlt28weeks=ifelse(as.numeric(Pretermlt28weeks)==0, NA, 
                                 as.numeric(Pretermlt28weeks))) %>% 
  mutate(Pretermlt32weeks=rowSums(dplyr::select(.,Preterm28tolt32weeks, 
                                                Pretermlt28weeks),na.rm=TRUE)) %>% 
  mutate(Pretermlt32weeks=ifelse(Pretermlt32weeks==0, NA, Pretermlt32weeks))
studies<-finalPreterm1 %>% filter(Source=="Study")

#------------------------------
#DQC FOR THE ADMINS

#-----------------------
#1. Coverage criteria
#>=90% coverage and >=90% facility births across 80% of the time series 
#(8 country-years between 2010-2019)

highQual<-admins %>% filter(Year>=2010 & Year<=2019) %>% 
  mutate(facility=ifelse(facility_birth>=90 , 1, 0),
         wpp=ifelse(wpp_ptb>=90 , 1, 0))

highQual2<-highQual %>% group_by(OfficialName) %>% 
  summarise(countF=sum(facility), countW=sum(wpp)) %>% 
  mutate(highQual=ifelse(countF>=8 & countW>=8, 1, 0))%>% 
  filter(highQual==1)


#----------
#2. All country-years have denominator live births with GA

allGA<-admins %>% rowwise() %>%  
  mutate(goodDen=sum(allGAUsed,allLBUsed)) %>% 
  group_by(OfficialName) %>% 
  summarise(nGA=length(unique(goodDen)), 
            nGA1=ifelse(unique(goodDen)==1, 1, 0))%>% 
  filter(nGA==1 & nGA1==1)

# allGA<-admins %>% group_by(OfficialName) %>% summarise(nGA=length(unique(allGAUsed)), 
#                                                        nGA1=ifelse(unique(allGAUsed)==1, 1, 0)) %>% 
#   filter(nGA==1 & nGA1==1)

#---------------------

#----------------------
#4. ADDITIONAL CRITERIA 
#<1000/<2500 is ≥4% across 80% of the time series and 
#when <1000/<2500 is NA then <1500/<2500 is ≥12.5% across 80% of the time series.

subEnvs<-admins %>%  filter(Year>=2010 & Year<=2019) %>% 
  mutate(lt28=as.numeric(Pretermlt28weeks)/Pretermlt37weeks, 
         lt32=Pretermlt32weeks/Pretermlt37weeks,
           subEnvs28=ifelse(!is.na(lt28) & lt28>=0.035, 1,0),
           subEnvs32=ifelse(!is.na(lt32) & lt32>=0.155, 1, 0),
           subEnvs=ifelse(!is.na(lt28), subEnvs28, subEnvs32)) %>% 
  dplyr::select(OfficialName, Year, Pretermlt28weeks, Pretermlt37weeks, lt28, lt32, subEnvs)

subEnvs2<-subEnvs %>% group_by(OfficialName) %>% 
  summarise(countS=sum(subEnvs)) %>% 
  mutate(subEnvs=ifelse(countS>=8, 1, 0)) %>%
  filter(subEnvs==1)

#----------
#3. Putting all criteria into the admin data
allGAs<-allGA$OfficialName

adminsFinal<-admins %>% 
  mutate(highQual=ifelse(OfficialName %in% highQual2$OfficialName, 1, 0),
         allGA=ifelse(OfficialName %in% allGAs, 1, 0), 
         subEnvProp=as.numeric(Pretermlt28weeks)/Pretermlt37weeks,
         subEnvProp2=Pretermlt32weeks/Pretermlt37weeks,
         subEnvs=ifelse(OfficialName %in% subEnvs2$OfficialName, 1, 0),
         topCat=ifelse(((highQual==1 & 
                           allGA==1 & 
                           sourceSubType!=3 & sourceSubType!=4 & 
                           #ADDITIONAL CRITERIA 
                           subEnvs==1 &
                           pretermRCalculated==1) | ISO=="HRV"), "A",
                       ifelse(sourceSubType==3 | sourceSubType==4 | pretermRCalculated==0, "C", "B")),
         subCat=ifelse(nSources==3, "1", "2"), 
         category=topCat) %>% 
  dplyr::select(ISO, OfficialName, Year,nSources, sourceSubType, category, Source, 
                facility_birth, wpp_ptb, allGAUsed, allLBUsed, allTBUsed, pretermRCalculated, isoYear,
                Representative, highQual, allGA,subEnvProp, subEnvProp2, subEnvs, Preterm_adjusted_point_est,
                studyPopulation)

#----
#Outputs
help1<-adminsFinal %>% group_by(OfficialName, category) %>% summarise(n=n()) %>% 
  mutate(DQC=paste0(OfficialName, " (", n, " country-years)")) %>% arrange(category)


#Repeated countries
output<-help1 %>% dplyr::select(OfficialName, category) %>% 
  distinct() %>%  arrange(category) %>% group_by(OfficialName) %>% 
  summarise(n=n()) %>% filter(n>1) %>% mutate(repeated=1) %>% dplyr::select(-n)

help2<-merge(x=help1, y=output, by="OfficialName", all.x=TRUE) %>% 
  arrange(category, OfficialName)

#write.csv(help2, "PRETERM/output/DQCforAdmin_countries.csv")

adminsFinal2<-adminsFinal %>% 
  dplyr::select(isoYear, Source, category, Representative, highQual, allGA, 
                subEnvs, Preterm_adjusted_point_est, studyPopulation)

#-----------------------------------
#DQC FOR STUDIES
#Those final ones for China are attempting to be national so better than 
#the rest of the sub-national studies
studies<-studies %>% mutate(row=row_number())
studiesFinal<-studies %>% mutate(category=ifelse(ISO=="CHN" & Year>=2012 & 
                                                   Year<=2018 & !is.na(Representative) & Representative=="Total population",
                                                 "D", "E")) %>% 
  arrange(category) %>% dplyr::select(ISO, OfficialName, Year, nSources, sourceSubType, 
                                      Source, category, facility_birth, wpp_ptb, allGAUsed,
                                      allLBUsed, allTBUsed, pretermRCalculated, isoYear, 
                                      Representative,studyPopulation, Preterm_adjusted_point_est) %>% distinct()  %>%
  mutate(highQual=NA, allGA=NA, subEnvs=NA, subEnvProp=NA, subEnvProp2=NA)

#------------------------------------
#ALL BACK TOGETHER

allCats<-rbind(adminsFinal, studiesFinal)

#--output
help2<-allCats %>% group_by(OfficialName, category) %>% summarise(n=n()) %>% 
  mutate(DQC=paste0(OfficialName, " (", n, " country-years)")) %>% arrange(category)


allCats2<-allCats %>% 
  dplyr::select(isoYear, Source, category, Representative, studyPopulation,
                highQual, allGA, subEnvs, subEnvProp,
                subEnvProp2, Preterm_adjusted_point_est)

finalPreterm<-merge(x=finalPreterm1, y=allCats2, 
                    by=c("isoYear","Source", "studyPopulation", "Preterm_adjusted_point_est"), 
                    all.x=TRUE)


#saveRDS(finalPreterm, "output/finalPreterm.RDS")


#-----
#DQC Calculations output

calcs<-finalPreterm %>% 
  dplyr::select(ISO, Country=OfficialName, Region=regionName ,Year, Source,
                sourceSubType, nSources, wpp_ptb, facility_birth,
                highQual, category, allGAUsed, allLBUsed, 
                allTBUsed, pretermRCalculated, subEnvProp, subEnvProp2, subEnvs) %>% arrange(Country, Year)

# write.csv(calcs, "output/DQC4_calculations.csv")
# 
# write.csv(help2, "output/DQC4_countries.csv")
