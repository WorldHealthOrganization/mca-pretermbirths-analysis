#Combine both Preterm input data sources

# #Adding the missing columns
# missingStudy<-setdiff(names(adminAll), names(studyAll))
# 
# addOn<-(adminAll %>% dplyr::select(missingStudy))[1,] %>% 
#   mutate_all(function(x) (x=NA))
# 
# studyAll2<-cbind(studyAll, addOn)
# 
# #---------
# #ADDING THEM BOTH TOGETHER 
# missingAdmin<-setdiff(names(studyAll2), names(adminAll) )
# 
# addOn2<-(studyAll2 %>% dplyr::select(missingAdmin))[1,] %>% 
#   mutate_all(function(x) (x=NA))
# 
# adminAll2<-cbind(adminAll, addOn2)


pretermTogether<-bind_rows(adminAll2, studyAll2) 

#-------
#ADD REGIONS
regionCodes<-readRDS("inputs/regionCodes.rds")
pretermAll<-merge(x=pretermTogether, y=regionCodes, 
                  by="ISO", all.x=TRUE) %>% 
  filter(Year>=2010 & Year<=2020)


#------
#SAVING

# saveRDS(pretermAll, "output/pretermPreIncExc.rds")
