newAdmin<-readxl::read_excel(paste0(newAdminName), 
                             sheet = "data", skip=1)
newAdmin2<-tidyVariableNames(newAdmin) %>% mutate(row=row_number(),
                                                  isoYear=paste0(IsoCode, Year),
                                                  SourcetypeforLBLBW=ifelse(SourcetypeforLBLBW=="NA", NA, as.numeric(SourcetypeforLBLBW))) %>% 
  mutate(sourceNew=ifelse(is.na(SourcetypeforLBLBW), 0, 1))


#Calculating the LBW subgroups from using the data that is already there
newAdmin3<-newAdmin2 %>% mutate(calculateBW=ifelse(NumberofbabieswithBW=="CL", 1, 
                                                   ifelse(NumberofbabieswithBW=="NA_LBW%", 2, 0)),
                                calculateLBW=ifelse(LBWlt2500g=="CL", 1, 0),
                                calculate1000=ifelse(LBWlt1000g=="CL", 1, 0), 
                                calculate1500=ifelse(LBWlt1500g=="CL", 1, 0),
                                calculate15002499=ifelse(LBW1500to2499g=="CL", 1, 0), 
                                calculate10001999=ifelse(LBW1000to1999g=="CL", 1, 0)) %>% 
  mutate_at(c(names(newAdmin2)[4:22]), as.numeric)

newAdmin4<-newAdmin3 %>% mutate(isoYear=paste0(IsoCode, Year)) %>%
  mutate(LBWlt2500g=ifelse(calculateLBW==0, LBWlt2500g,
                           
                           #1
                           ifelse(!is.na(LBW2000to2499g)&
                                    !is.na(LBW1500to1999g)&!is.na(LBW1000to1499g)& 
                                    !is.na(LBW500to999g),
                                  rowSums(dplyr::select(.,LBW2000to2499g,LBW1500to1999g,LBW1000to1499g,LBW500to999g,LBWlt500g), na.rm=TRUE), 
                                  #2  
                                  ifelse(!is.na(LBW2000to2499g)&
                                           !is.na(LBW1500to1999g)&!is.na(LBW1000to1499g)& 
                                           !is.na(LBWlt1000g),
                                         rowSums(dplyr::select(.,LBW2000to2499g,LBW1500to1999g,LBW1000to1499g,LBWlt1000g), na.rm=TRUE), 
                                         
                                         #4
                                         ifelse(!is.na(LBW2000to2499g)&
                                                  !is.na(LBW1500to1999g) &!is.na(LBWlt1500g), 
                                                rowSums(dplyr::select(.,LBW2000to2499g,LBW1500to1999g,LBWlt1500g), na.rm=TRUE),
                                                #3   
                                                ifelse(!is.na(LBWlt1500g) & !is.na(LBW1500to2499g), 
                                                       rowSums(dplyr::select(.,LBW1500to2499g,LBWlt1500g), na.rm=TRUE),
                                                       
                                                       #5
                                                       ifelse(!is.na(LBW1000to1499g)& 
                                                                !is.na(LBW500to999g) &
                                                                !is.na(LBW1500to2499g),
                                                              rowSums(dplyr::select(.,LBW1000to1499g,LBW500to999g,LBWlt500g, LBW1500to2499g), na.rm=TRUE), 
                                                              
                                                              #6
                                                              ifelse(!is.na(LBW2000to2499g)&
                                                                       !is.na(LBWlt1000g)&!is.na(LBW1000to1999g),
                                                                     rowSums(dplyr::select(.,LBW2000to2499g,LBWlt1000g,LBW1000to1999g), na.rm=TRUE), 
                                                                     
                                                                     #7
                                                                     ifelse(!is.na(LBW1000to1499g)&
                                                                              !is.na(LBW1500to2499g), 
                                                                            rowSums(dplyr::select(.,LBW1000to1499g,LBW1500to2499g,LBWlt1000g), na.rm=TRUE),
                                                                            
                                                                            
                                                                            ifelse(LBWlt2500g==0 & !is.na(ReportedLBWrate) & !is.na(NumberofallLBs), round(NumberofallLBs*(1-(ReportedLBWrate/100))),NA))))))))))%>% 
  mutate(LBWlt1000g=ifelse(calculate1000==1, rowSums(dplyr::select(.,LBWlt500g, LBW500to999g), na.rm=TRUE), LBWlt1000g)) %>%
  
  mutate(LBWlt1500g=ifelse(calculate1500==1, rowSums(dplyr::select(.,LBWlt1000g, LBW1000to1499g), na.rm=TRUE), LBWlt1500g)) %>%
  mutate(LBW1500to2499g=ifelse(calculate15002499==1,rowSums(dplyr::select(.,LBW1500to1999g, LBW2000to2499g), na.rm=TRUE), LBW1500to2499g)) %>% 
  mutate(LBW1000to1999g=ifelse(calculate10001999==1, rowSums(dplyr::select(.,LBW1500to1999g, LBW1000to1499g), na.rm=TRUE), LBW1000to1999g)) %>% 
  
  mutate(NumberofbabieswithBW=ifelse(calculateBW==1 & !is.na(`LBBW>/=2500g`) & !is.na(LBWlt2500g), 
                                     rowSums(dplyr::select(.,`LBBW>/=2500g`,LBWlt2500g), na.rm=TRUE), 
                                     ifelse(calculateBW==2 & !is.na(ReportedLBWrate) & !is.na(LBWlt2500g), 
                                            ceiling(LBWlt2500g/(ReportedLBWrate/100)), NA))) %>% 
  mutate(MissingBW=ifelse(!is.na(NumberofbabieswithBW) & !is.na(NumberofallLBs), NumberofallLBs-NumberofbabieswithBW, NA)) %>% 
  mutate(isoYear=paste0(IsoCode, Year)) %>% 
  mutate(LBWlt2500g=ifelse(LBWlt2500g==0, NA, LBWlt2500g))

newAdmin5<-newAdmin4 %>% dplyr::select(-c(Countryname, NewLBWdata,Numberoftotalbirths, `22`,
                                          LBWto26, LBWto27, LBWto28,LBWto29,LBWto30, LBWto31, LBWto32, LBWto33)) %>% 
  rename(NumberofallLBs6=NumberofallLBs,
         SourceLBW=LBWto34) %>% 
  dplyr::select(-isoYear)


