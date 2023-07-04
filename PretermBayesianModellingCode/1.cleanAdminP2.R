#Input Admin data

adminBefore1=readxl::read_excel(paste0(adminName), 
                                sheet = "VN database", skip=1)

adminCodes=readxl::read_excel(paste0(adminName), 
                              sheet = "VN Source database", skip=1)


#Change the variable names
adminBefore1<-tidyVariableNames(adminBefore1)
adminCodes<-tidyVariableNames(adminCodes)


#----------
#Remove LBW and stillbriths columms from the joint admin database
adminBefore1<-adminBefore1 %>% 
  mutate (Numberoftotalbirths22=ifelse(is.na(as.numeric(Numberoftotalbirths22)),
                                       as.numeric(NumberofallLBs23) + as.numeric(Numberofstillbirths),Numberoftotalbirths22))
adminBefore1<-adminBefore1[,-c(5,6,7,8,20,21,which(grepl("LBW", names(adminBefore1)) | 
                                             grepl("Stillbirths", names(adminBefore1))| 
                                             grepl("stillbirths", names(adminBefore1))|
                                               grepl("SGA", names(adminBefore1))|
                                               grepl("Neonatal", names(adminBefore1))|
                                             grepl("neonatal", names(adminBefore1))))]
adminBefore2<-adminBefore1 %>% mutate_at(vars(names(adminBefore1)[3:ncol(adminBefore1)]), as.numeric)

#Inputting the new preterm data from the country consultation
source("1.newPreterm.R")

adminBefore<-bind_rows(x=newPreterm2,
                       y=adminBefore2 %>% rename(ISO=IsoCode) %>% filter(!(ISO %in% newPreterm2$ISO)))

#----
#Adding missing values
admin_0<-adminBefore%>%
  mutate (MissingGA =ifelse(is.na(MissingGA)  & !is.na(NumberofbabieswithGA),
                            NumberofallLBs23 -NumberofbabieswithGA, MissingGA),
          
          NumberofbabieswithGA=ifelse(is.na(NumberofbabieswithGA)  & !is.na(MissingGA)& !is.na(NumberofallLBs23),
                                      NumberofallLBs23 - MissingGA,NumberofbabieswithGA))%>% 
  mutate(isoYear=paste0(ISO, Year))


#---
#Removing Kosovo and only keeping the years needed

adminCombined<-admin_0%>% filter(ISO!="XKX")
admin<-adminCombined %>% filter(Year>=2010 & Year<=2020)

#Calculate Preterm rates for those with birth estimates
# if they dont have number of babies with GA, then use all live births, 
# then total births, then the reported rate.
admin<-admin %>% mutate(allGAUsed=ifelse(!is.na(NumberofbabieswithGA) & !is.na(Pretermlt37weeks),1, 0),
                        allLBUsed=ifelse(is.na(NumberofbabieswithGA) & !is.na(NumberofallLBs23) & !is.na(Pretermlt37weeks),1, 0),
                        allTBUsed=ifelse(is.na(NumberofbabieswithGA) & is.na(NumberofallLBs23) & 
                                           !is.na(admin$Numberoftotalbirths22) & !is.na(Pretermlt37weeks),1, 0)) %>% 
  
  mutate(pretermRCalculated=ifelse(allGAUsed==1 | allLBUsed==1 | allTBUsed==1, 1, 0)) %>% 
  
  mutate(calculatedPretermRate=ifelse(!is.na(NumberofbabieswithGA) & !is.na(Pretermlt37weeks), 
                                      (Pretermlt37weeks/NumberofbabieswithGA)*100, 
                                      ifelse(!is.na(NumberofallLBs23) & !is.na(Pretermlt37weeks), 
                                             (Pretermlt37weeks/NumberofallLBs23)*100,
                                             ifelse(!is.na(Numberoftotalbirths22) & !is.na(Pretermlt37weeks),
                                                    (Pretermlt37weeks/Numberoftotalbirths22)*100,NA))))

#Calculate Livebirths from those with rate and preterm numbers
admin<-admin %>% mutate(NumberofbabieswithGA=ifelse(is.na(NumberofallLBs23) & is.na(Numberoftotalbirths22) & is.na(NumberofbabieswithGA)&
                                                      !is.na(Pretermlt37weeks) & !is.na(Reportedpretermrate), 
                                                    ceiling(Pretermlt37weeks/(Reportedpretermrate/100)), 
                                                    NumberofbabieswithGA))

admin<-admin %>% mutate(Preterm_adjusted_point_est=ifelse(!is.na(calculatedPretermRate),calculatedPretermRate,
                                                          ifelse(!is.na(Reportedpretermrate), Reportedpretermrate,
                                                                 NA)))

#----
#Exclude missing LBW with no way to calculate it
removedAdmin<-admin %>% filter(is.na(Preterm_adjusted_point_est))
admin2<-admin %>% filter(!is.na(Preterm_adjusted_point_est))
adminNotCalc<-admin2 %>% filter(pretermRCalculated==0)


#----
#Add in admin meta-data

adminCodes<-adminCodes %>% mutate(isoYear=paste0(IsoCode, MidYear))
adminCodesPreterm<-adminCodes %>% dplyr::select(isoYear, names(adminCodes)[35:62])

codes<-names(adminCodesPreterm %>% dplyr::select(-c(SourcetypeOtherforLBpreterm,
                                             PreviousUNpretermdatabasechecked,
                                             Isseparatedataforsingletonandmultiplebirth,
                                             AbstractorforLBpreterm,
                                             "Arenumberofmissinggestationalageavailable?")))
newCodes<-admin2 %>% dplyr::select(isoYear, codes)
help<-newCodes %>% filter(is.na(SourcetypeforLBpreterm))


admin3<-merge(x=admin2, y=adminCodesPreterm %>% 
                                  dplyr::select(-c(SourcetypeOtherforLBpreterm,
                                                   PreviousUNpretermdatabasechecked,
                                                   Isseparatedataforsingletonandmultiplebirth,
                                                   AbstractorforLBpreterm,
                                    "Arenumberofmissinggestationalageavailable?")),
              by="isoYear", all.x=TRUE) %>% 
  mutate(SourcetypeforLBpreterm=ifelse(is.na(SourcetypeforLBpreterm.x), SourcetypeforLBpreterm.y, SourcetypeforLBpreterm.x),
         SourcenameLBpreterm=ifelse(is.na(SourcenameLBpreterm.x), SourcenameLBpreterm.y, SourcenameLBpreterm.x),
         AuthororinstitutionnameforLBpreterm=ifelse(is.na(AuthororinstitutionnameforLBpreterm.x), AuthororinstitutionnameforLBpreterm.y, AuthororinstitutionnameforLBpreterm.x),
         FullcitationLBpreterm=ifelse(is.na(FullcitationLBpreterm.x), FullcitationLBpreterm.y, FullcitationLBpreterm.x),
         WebsitelinkforLBpreterm=ifelse(is.na(WebsitelinkforLBpreterm.x), WebsitelinkforLBpreterm.y, WebsitelinkforLBpreterm.x),
         LinkactiveforLBpreterm=ifelse(is.na(LinkactiveforLBpreterm.x), LinkactiveforLBpreterm.y, LinkactiveforLBpreterm.x),
         WebsiteisinEnglishforLBpreterm=ifelse(is.na(WebsiteisinEnglishforLBpreterm.x), WebsiteisinEnglishforLBpreterm.y, WebsiteisinEnglishforLBpreterm.x),
         WebsitelanguageotherLBpreterm=ifelse(is.na(WebsitelanguageotherLBpreterm.x), WebsitelanguageotherLBpreterm.y, WebsitelanguageotherLBpreterm.x),
         FocalpersonLBpreterm=ifelse(is.na(FocalpersonLBpreterm.x), FocalpersonLBpreterm.y, FocalpersonLBpreterm.x),
         DataabstractionsourceLBpreterm=ifelse(is.na(DataabstractionsourceLBpreterm.x), DataabstractionsourceLBpreterm.y, DataabstractionsourceLBpreterm.x),
         DataabstractionsourceotherforLBpreterm=ifelse(is.na(DataabstractionsourceotherforLBpreterm.x), DataabstractionsourceotherforLBpreterm.y, DataabstractionsourceotherforLBpreterm.x),
         AbstractiondateforLBpreterm=ifelse(is.na(AbstractiondateforLBpreterm.x), AbstractiondateforLBpreterm.y, AbstractiondateforLBpreterm.x),
         AbstractorOtherforLBpreterm=ifelse(is.na(AbstractorOtherforLBpreterm.x), AbstractorOtherforLBpreterm.y, AbstractorOtherforLBpreterm.x),
         Pretermdefinition=ifelse(is.na(Pretermdefinition.x), Pretermdefinition.y, Pretermdefinition.x),
         Pretermdenominator=ifelse(is.na(Pretermdenominator.x),Pretermdenominator.y, Pretermdenominator.x),
         Pretermdefinitionother=ifelse(is.na(Pretermdefinitionother.x), Pretermdefinitionother.y, Pretermdefinitionother.x),
         Pretermbirthsreportedbyweeks=ifelse(is.na(Pretermbirthsreportedbyweeks.x), Pretermbirthsreportedbyweeks.y, Pretermbirthsreportedbyweeks.x),
         Allpretermbirths=ifelse(is.na(Allpretermbirths.x), Allpretermbirths.y, Allpretermbirths.x),
         Methodofgestationalageassessment=ifelse(is.na(Methodofgestationalageassessment.x), Methodofgestationalageassessment.y, Methodofgestationalageassessment.x),
         MethodofgestationalageassessmentOther=ifelse(is.na(MethodofgestationalageassessmentOther.x), MethodofgestationalageassessmentOther.y, MethodofgestationalageassessmentOther.x),
         Gestationalagelowerthresholdavailable=ifelse(is.na(Gestationalagelowerthresholdavailable.x), Gestationalagelowerthresholdavailable.y, Gestationalagelowerthresholdavailable.x),
         Gestationalagelowerthreshold=ifelse(is.na(Gestationalagelowerthreshold.x), Gestationalagelowerthreshold.y, Gestationalagelowerthreshold.x),
         Gestationalagelowerthresholdother=ifelse(is.na(Gestationalagelowerthresholdother.x), Gestationalagelowerthresholdother.y, Gestationalagelowerthresholdother.x))
adminAll <- dplyr::select(admin3, -contains(c(".x", ".y"))) %>% 
  mutate(Source="Admin", 
         sourceSubType=SourcetypeforLBpreterm)

#---------------
#Adding the source sub-type from other years in the same country if missing
help<-adminAll %>% filter(is.na(sourceSubType))
help2<-adminAll %>% filter(ISO %in% help$ISO & Source=="Admin" & 
                             !is.na(sourceSubType)) %>% 
  dplyr::select(ISO, sourceSubType)%>% 
  group_by(ISO, sourceSubType) %>% 
  summarise(n=n()) %>% group_by(ISO) %>% 
  mutate(row=row_number()) %>% filter(row==1) %>% 
  dplyr::select(ISO, sourceSubType) %>% 
  rename(sourceSubType2=sourceSubType)
adminAll<-merge(x=adminAll, y=help2, by="ISO", all.x=TRUE) %>% 
  mutate(sourceSubType=ifelse(is.na(sourceSubType), sourceSubType2, sourceSubType))

#--
#Final calculations
adminAll<-adminAll %>% mutate(y=Preterm_adjusted_point_est/100) %>% 
  mutate(se=y*(1-y))

adminAll$Preterm_adjusted_se<-adminAll$se

#---Checks: How many Preterm rates calculcated, and then how many were calculated from all LB rather 
#than LB with BW.

pretermCalculated<-adminAll %>% filter(pretermRCalculated==1)

allLBUsed<-adminAll %>% group_by(allLBUsed) %>% summarise(n=n())
allLBUsedRows<-adminAll %>% filter(allLBUsed==1)

