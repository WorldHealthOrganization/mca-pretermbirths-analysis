#Adding other groupings

#pretermFinal<-readRDS("output/finalPreterm.rds")

#0 = Singleton's only
#1 - singles and multiples
#9 - women only, not baby

pretermFinal<-pretermFinal %>% 
  mutate(singMult=ifelse(is.na(Allpretermbirths) | 
                           Allpretermbirths %in% c(1), 1,#muliples
                         0))


# 0=Not stated
# 1=Best Obstetric estimate
# 2=Ultrasound-based upto 18 weeks
# 3=Ultrasound-based assessment, date unknown 
# 4=Last menstrual period
# 5=Symphysis Fundal Height
# 6=Postnatal clinical assessment or other, specify

wbregionCodes<-read.csv("inputs/sbr_regions v2.csv") %>% dplyr::select(ISO=ISO3Code,
                                                                      WBRegion4)
addWBregs<-merge(x=pretermFinal, y=wbregionCodes, by="ISO", all.x = TRUE)

help<-addWBregs %>% group_by(Methodofgestationalageassessment, WBRegion4, Source) %>% 
  summarise(n=n())

methodGroup<-addWBregs %>% 
  mutate(methodGAGroup=ifelse(((is.na(Methodofgestationalageassessment) | Methodofgestationalageassessment %in% c(0)) & WBRegion4=="High income")|
                                                         (Methodofgestationalageassessment %in% c(1, 2, 3)), 1,
                        ifelse(Methodofgestationalageassessment ==4 | ((is.na(Methodofgestationalageassessment) |
                                                 Methodofgestationalageassessment%in% c(0)) & WBRegion4!="High income" & Source=="Admin"), 2, 3))) %>% 
  mutate(topCat=substr(category, 1,1))


saveRDS(methodGroup, "output/PretermfinalInputDatabase.rds")



#Outputting method of GA countries
methodGACountries<-methodGroup %>%  group_by(methodGAGroup) %>% distinct(OfficialName) %>% arrange(methodGAGroup, OfficialName)
# write.csv(methodGACountries,"output/methodGACountries.csv")

