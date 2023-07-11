finalData1<-readRDS("output/finalData.RDS")

finalData1<-finalData1 %>% mutate(sourceSubType=ifelse(ISO=="IND" & Source=="Admin", 3, sourceSubType))

sources<-finalData1 %>% dplyr::select(ISO, Source) %>% distinct() %>% mutate(sourceIndex=ifelse(Source=="Admin", 1, 2)) %>% group_by(ISO) %>% 
  summarise(nSources=sum(sourceIndex)) %>% mutate(nSourcesLab=ifelse(nSources==1, "Only admin", 
                                                                     ifelse(nSources==2, "Only Survey", "Admin and Survey")))

finalData2<-merge(x=finalData1, y=sources, by="ISO", all.x=TRUE) %>% filter(Source=="Admin")
#-----------------------
#Coverage criteria
#>=90% coverage and >=90% facility births across 80% of the time series 
#>#(16 country-years between 2000-2019)

highQual<-finalData2 %>% mutate(facility=ifelse(facility_birth>=90 , 1, 0),
                                wpp=ifelse(wpp_lbw>=90 , 1, 0))
highQual2<-highQual %>% group_by(OfficialName) %>% 
  summarise(countF=sum(facility), countW=sum(wpp)) %>% 
  mutate(highQual=ifelse(countF>=16 & countW>=16, 1, 0))%>% 
  filter(highQual==1)

#----------------------
#ADDITIONAL CRITERIA 
#<1000/<2500 is ≥4% across 80% of the time series and 
#when <1000/<2500 is NA then <1500/<2500 is ≥12.5% across 80% of the time series.

subEnvs<-finalData2 %>% mutate(lt1000=LBWlt1000g/LBWlt2500g, 
                               lt1500=LBWlt1500g/LBWlt2500g, 
                               subEnvs=ifelse((is.na(lt1000) & !is.na(lt1500) & 
                                                 lt1500>=0.125), 1, 
                                              ifelse(!is.na(lt1000) & lt1000 >= 0.04, 1, 0))) %>% 
  dplyr::select(OfficialName, Year, LBWlt1000g, LBWlt1500g, LBWlt2500g, lt1000, lt1500, subEnvs)

subEnvs2<-subEnvs %>% group_by(OfficialName) %>% 
  summarise(countS=sum(subEnvs)) %>% 
  mutate(subEnvs=ifelse(countS>=16, 1, 0)) %>%
  filter(subEnvs==1)

#----------
#All country-years have denominator live births with birthweight
allBW<-finalData2 %>% group_by(OfficialName) %>% summarise(nBW=length(unique(allBWUsed)), 
                                                           nBW1=ifelse(unique(allBWUsed)==1, 1, 0)) %>% 
  filter(nBW==1 & nBW1==1)


#----------
#Applying the criteria to generate the DQC

finalData3<-finalData2 %>% filter(Source=="Admin")%>% 
  mutate(topCat=ifelse((OfficialName %in% highQual2$OfficialName & 
                         OfficialName %in% allBW$OfficialName & 
                         sourceSubType!=3 & sourceSubType!=4 & 
                         #ADDITIONAL CRITERIA 
                         OfficialName %in% subEnvs2$OfficialName &
                         lbwrCalculated==1 ) | ISO=="FRA", "A",
                       ifelse(sourceSubType==3 | sourceSubType==4 | lbwrCalculated==0, "C", "B")),
         subCat=ifelse(nSources==3, "i", "ii"), 
         newRank=paste0(topCat, subCat)) %>% 
  dplyr::select(ISO, OfficialName, Year,nSources, sourceSubType, category=newRank, Source, 
                facility_birth, wpp_lbw, allBWUsed, allLBUsed, 
                #allTBUsed, 
                lbwrCalculated)

#--outputting the DQC
help1<-finalData3 %>% group_by(OfficialName, category) %>% summarise(n=n()) %>% 
  mutate(DQH=paste0(OfficialName, " (", n, " country-years)")) %>% arrange(category)

output<-finalData3 %>% dplyr::select(OfficialName, category) %>% distinct() %>%  arrange(category)

write.csv(help1, "output/DQC5Update_countries.csv")

#Repeated countries
output2<-output %>% group_by(OfficialName) %>% summarise(n=n()) %>% filter(n>1)
output3<-finalData3 %>% group_by(OfficialName, category, nSources) %>% summarise(n=n()) %>% filter(category==1 & nSources==3)
#help<-finalData3 %>% filter(is.na(MissingBW_perc) & qualityHierarchy==3 &nSources==1)

#------------------
#Outputting country flags

countryFlag<-finalData3%>% group_by(ISO, category) %>% summarise(n=n()) %>% arrange(ISO, desc(n)) %>% 
  group_by(ISO) %>% mutate(row=row_number()) %>% ungroup() %>% filter(row==1) %>% 
  dplyr::select(ISO, category) %>% rename(rankCountry=category)

finalData3<-merge(x=finalData3, y=countryFlag, by="ISO", all.x=TRUE) %>%
  dplyr::select(ISO, OfficialName, Year,nSources, sourceSubType, category, rankCountry, Source, 
                facility_birth, wpp_lbw, allBWUsed, allLBUsed, 
                #allTBUsed, 
                lbwrCalculated, nSources) %>% rename(newFlag=category) %>% 
  mutate(isoYear=paste0(ISO, Year))

countryFlag<-countryFlag %>% rename(iso=ISO)
saveRDS(countryFlag, "output/dqc5_CountryFlag.rds")

#--------------------
#Adding back into the final dataset 
admins<-finalData3 %>% dplyr::select(isoYear, Source, newFlag)
survey<-finalData3 %>% dplyr::select(ISO, rankCountry) %>% mutate(Source="Survey") %>% rename(newFlag=rankCountry) %>% distinct()
finalData1<-merge(x=finalData1, y=admins, by=c("isoYear","Source"), all.x=TRUE)
finalData1<-merge(x=finalData1, y=survey,by=c("ISO","Source"), all.x=TRUE) %>% 
  mutate(newFlag=ifelse(!is.na(newFlag.x), newFlag.x, 
                        ifelse(is.na(newFlag.x) & !is.na(newFlag.y), newFlag.y, 1)))

finalData<-finalData1

#save
saveRDS(finalData, "output/finalData.RDS")


#-----------
#Outputting all the criteria variables that were involved
regionCodes<-readRDS("output/regionCodes.RDS") %>% dplyr::select(-c(regionIndex, OfficialName))
finalData4<-merge(x=finalData3, y=regionCodes, by="ISO", all.x=TRUE)

finalData5<-merge(x=finalData4, y=subEnvs, by=c("OfficialName", "Year"), all.x=TRUE) %>% 
  mutate(highQual=ifelse(OfficialName %in% highQual2$OfficialName, 1, 0),
         subEnvs=ifelse(OfficialName %in% subEnvs2$OfficialName,1, 0),
         newcriteriaDQC=ifelse(newFlag=="Ai", "A1",
                               ifelse(newFlag=="Aii", "A2",
                                      ifelse(newFlag=="Bi", "B1", 
                                             ifelse(newFlag=="Bii", "B2", 
                                                    ifelse(newFlag=="Ci", "C1", 
                                                           ifelse(newFlag=="Cii", "C2", NA)))))))%>%
  dplyr::select(ISO, Country=OfficialName, Region=regionName ,Year, 
                sourceSubType, nSources, wpp_lbw, facility_birth,
                highQual, subEnvs, newcriteriaDQC, allBWUsed, allLBUsed, 
                #allTBUsed, 
                lbwrCalculated, lt1000, lt1500) %>% arrange(Country, Year)
write.csv(finalData5, "output/DQC5criteria_calculations.csv")
