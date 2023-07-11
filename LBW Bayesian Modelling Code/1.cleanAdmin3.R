#Inputting the Admin data

source("1.newAdmin5.R")

adminCombined<-newAdmin5
#----
#Adding missing values
admin_0<-adminCombined%>%
  mutate (MissingBW=ifelse(is.na(MissingBW)  & !is.na(NumberofbabieswithBW),
                           NumberofallLBs6 -NumberofbabieswithBW, 
                           ifelse(is.na(MissingBW) & !is.na(percofmissingbirthweight)& !is.na(NumberofallLBs6), 
                                  percofmissingbirthweight*NumberofallLBs6,
                                  MissingBW)),
          NumberofbabieswithBW=ifelse(is.na(NumberofbabieswithBW)  & !is.na(MissingBW)& !is.na(NumberofallLBs6),
                                      NumberofallLBs6 - MissingBW,NumberofbabieswithBW),
          LBWlt1000g=ifelse(is.na(LBWlt1000g), LBW500to999g + LBWlt500g, LBWlt1000g)
          )

admin_0<-admin_0%>%
  mutate(LBWlt2500g_1000_perc=(LBWlt2500g-LBW1000to1499g)/(NumberofallLBs6)*100,
         LBWlt1000g_perc=LBWlt1000g/NumberofallLBs6*100,
         MissingBW_perc=MissingBW/NumberofallLBs6*100) %>% 
  mutate(isoYear=paste0(IsoCode, Year))


#---

admin<-admin_0 %>% filter(Year>=2000 & Year<=2020)

#Calculate LBW rates for those with birth estimates
#if they dont have number of babies with birth weight, then use all live births, 
#then total births, then the reported rate
admin$allBWUsed<-ifelse(!is.na(admin$NumberofbabieswithBW)&
                          !is.na(admin$LBWlt2500g), 1, 0)
admin$allLBUsed<-ifelse(is.na(admin$NumberofbabieswithBW)&
                          !is.na(admin$NumberofallLBs6)&!is.na(admin$LBWlt2500g), 1, 0)
# admin$allTBUsed<-ifelse(is.na(admin$NumberofbabieswithBW)&is.na(admin$NumberofallLBs6)&
#                           !is.na(admin$Numberoftotalbirths5)&!is.na(admin$LBWlt2500g), 1, 0)
admin<-admin %>% mutate(lbwrCalculated=ifelse(allBWUsed==1|allLBUsed==1, 1, 0))

admin$calculatedLBWrate<-ifelse(!is.na(admin$NumberofbabieswithBW)&!is.na(admin$LBWlt2500g),
                                (admin$LBWlt2500g/admin$NumberofbabieswithBW)*100,
                                ifelse(!is.na(admin$NumberofallLBs6)&!is.na(admin$LBWlt2500g), 
                                       (admin$LBWlt2500g/admin$NumberofallLBs6)*100,
                                              NA))

#Calculate Livebirths from those with rate and LBW numbers
admin<-admin %>% mutate(NumberofbabieswithBW=ifelse(is.na(NumberofallLBs6) & is.na(NumberofbabieswithBW)&
                                                      !is.na(LBWlt2500g) & !is.na(ReportedLBWrate), 
                                                    ceiling(LBWlt2500g/(ReportedLBWrate/100)), 
                                                    NumberofbabieswithBW))

admin<-admin %>% mutate(LBW_adjusted_point_est=ifelse(!is.na(calculatedLBWrate),calculatedLBWrate,
                                                      ifelse(!is.na(ReportedLBWrate), ReportedLBWrate,
                                                             NA)))



#----
#Exclude missing LBW with no way to calculate it
removedAdmin<-admin %>% filter(is.na(LBW_adjusted_point_est))
admin2<-admin %>% filter(!is.na(LBW_adjusted_point_est))
adminNotCalc<-admin2 %>% filter(lbwrCalculated==0)


#Checks
output<-admin %>% filter(is.na(LBW_adjusted_point_est))
unique(output$IsoCode)
nrow(admin)-nrow(admin2)


admin3<-admin2 %>% mutate(isoYear=paste0(IsoCode, Year)) %>% mutate(fromAdmin3=1:nrow(admin2)) 

#----
#Add in admin codes

adminAll<-admin3 %>% 
  mutate(Year=as.numeric(Year), 
         Source="Admin") %>% 
  rename(sourceSubType=SourcetypeforLBLBW)

adminHelp<-adminAll %>% mutate(all=1:nrow(adminAll)) %>% 
  filter(fromAdmin3==lag(fromAdmin3))


#Temporary source-subtype - adds the sourceSubType from the rest of the country-years
#to those that dont have one
help<-adminAll %>% filter(is.na(sourceSubType))

#--
adminAll<-adminAll %>% mutate(y=LBW_adjusted_point_est/100) %>% 
  mutate(se=NA)


adminAll$LBW_adjusted_se<-adminAll$se

#---
#---Checks: How many LBW rates calculcated, and then how many were calculated from all LB rather 
#than LB with BW.

lbwCalculated<-adminAll %>% filter(lbwrCalculated==1)

allLBUsed<-adminAll %>% group_by(allLBUsed) %>% summarise(n=n())
allLBUsedRows<-adminAll %>% filter(allLBUsed==1)

