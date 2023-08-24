finalData<-readRDS("output/PretermfinalInputDatabase.RDS")
regionCodes<-readRDS("output/regionCodes.RDS")
#---------------------------------------------------
#Getting only the data with sub-envelopes 

finalData2<-finalData %>% mutate(sourceIndex=ifelse(category %in% c("A"), 1,
                                                    ifelse(category %in% c("B"), 2,
                                                           ifelse(category %in% c("C"), 3, 
                                                                  ifelse(category=="D", 4, 
                                                                         ifelse(category=="E", 5, NA)))))) %>% 
  mutate(Preterm32tolt37weeks=as.numeric(Preterm32tolt37weeks) ,                     
         Preterm28tolt32weeks=as.numeric(Preterm28tolt32weeks),                      
         Pretermlt28weeks=as.numeric(Pretermlt28weeks),
         Pretermlt25weeks=as.numeric(Pretermlt25weeks),
         Pretermlt28weeks=ifelse(is.na(Pretermlt28weeks) &
                                   !is.na(Preterm24tolt28weeks) &
                                   !is.na(Preterm22tolt24weeks), 
                                 Preterm24tolt28weeks+Preterm22tolt24weeks, round(Pretermlt28weeks)),
         Preterm28tolt32weeks=ifelse(is.na(Preterm28tolt32weeks) & 
                                       !is.na(Pretermlt32weeks) &
                                       !is.na(Preterm24tolt28weeks), 
                                     Pretermlt32weeks-Preterm24tolt28weeks, 
                                     round(Preterm28tolt32weeks)),
         Preterm32tolt37weeks=ifelse(is.na(Preterm32tolt37weeks) & 
                                       !is.na(Pretermlt37weeks) &
                                       !is.na(Preterm28tolt32weeks), 
                                     Pretermlt37weeks-Preterm28tolt32weeks, 
                                     round(Preterm32tolt37weeks)),
         Pretermlt37weeks=round(Pretermlt37weeks)) %>%
  mutate(Pretermlt28weeks=ifelse(is.na(Pretermlt28weeks) & 
                                   (Preterm28tolt32weeks + Preterm32tolt37weeks)==Pretermlt37weeks, 0, Pretermlt28weeks)) %>% 
  filter(!is.na(Pretermlt28weeks) &
           !is.na(Preterm28tolt32weeks) &
           !is.na(Preterm32tolt37weeks) & !is.na(Pretermlt37weeks)
  )  %>% 
  dplyr::select(ISO, OfficialName, regionName, regionIndex, Year, Source, sourceIndex,
                NumberofallLBs23, NumberofbabieswithGA, 
                Pretermlt37weeks, Preterm32tolt37weeks, 
                Preterm28tolt32weeks,
                Pretermlt28weeks) %>% 
  mutate(Preterm=Preterm32tolt37weeks+Preterm28tolt32weeks+Pretermlt28weeks,
         proplt28weeks=Pretermlt28weeks/Pretermlt37weeks, 
         prop28tolt32weeks=Preterm28tolt32weeks/Pretermlt37weeks,
         prop32tolt37weeks=Preterm32tolt37weeks/Pretermlt37weeks)

quantile(finalData2$proplt28weeks, c(0.01,0.05,0.1,0.25, 0.5, 0.75, 0.9, 0.99))

studies<-readxl::read_xlsx("inputs/VN subgroup data_EH_LS_4_hb.xlsx", 
                           sheet="Preterm subgroups_Table_A82") %>% 
  #filter(nchar(row)<4 & exclude==0 & Year>=2010) %>% 
  filter(nchar(row)<4 & Year>=2010) %>%
  mutate(ISO=substr(`Study name (if subnational)`,1,3)) %>% 
  filter(!(ISO %in% c("SWE", "MYS", "FIN", "NA"))) %>% 
  dplyr::select(ISO, Year, Source=`National or subnational`,
                NumberofallLBs23=`Number of livebirths`,
                NumberofbabieswithGA=`Livebirths with types by GA assessed`,
                Preterm32tolt37weeks=`Number 32 - 36 weeks`,
                Preterm28tolt32weeks=`Number 28 - 31 weeks`,
                Pretermlt28weeks=`Number <28 weeks`) %>% 
  mutate(Preterm=Preterm32tolt37weeks+
           Preterm28tolt32weeks+ 
           Pretermlt28weeks,
         proplt28weeks=Pretermlt28weeks/Preterm, 
         prop28tolt32weeks=Preterm28tolt32weeks/Preterm,
         prop32tolt37weeks=Preterm32tolt37weeks/Preterm, 
         Source=ifelse(Source=="National", "Admin", 
                       ifelse(Source=="Subnational", "Study", NA)),
         #Exclude implausible studies (cut off from admin data 1st percentile)
         exclude=ifelse(proplt28weeks<0.01, 1, 0)) %>% 
  filter(!(Source=="Study" & exclude==1)) %>% 
  filter(!(ISO %in% finalData2$ISO)) %>% 
  dplyr::select(-exclude)


studies2<-merge(x=studies, y=regionCodes, 
                by="ISO", all.x=TRUE) 
source("0.fileNames.R")
source("1.cleanSurveyP2.R")

additionalStudies<-studyAll %>% filter(DQexclusion==2& Year>=2010) %>% 
  mutate(Source="Study", 
         sourceIndex=0,
         Preterm=Preterm32tolt37weeks+Preterm28tolt32weeks+Pretermlt28weeks,
         proplt28weeks=Pretermlt28weeks/Pretermlt37weeks, 
         prop28tolt32weeks=Preterm28tolt32weeks/Pretermlt37weeks,
         prop32tolt37weeks=Preterm32tolt37weeks/Pretermlt37weeks) %>% 
  dplyr::select(names(studies)) %>% 
  filter(!is.na(Pretermlt28weeks) &
           !is.na(Preterm28tolt32weeks) &
           !is.na(Preterm32tolt37weeks) & !is.na(Preterm)) %>% 
  filter(proplt28weeks>=0.01) %>% 
  filter(!(ISO %in% finalData2$ISO))


finalData3<-bind_rows(finalData2, studies2, merge(x=additionalStudies, y=regionCodes, by="ISO", all.x=TRUE))

saveRDS(finalData3, "output/pretermSubgroupData.rds") 


finalData3<-finalData3%>% 
  mutate(row=row_number())


studies<-finalData3 %>% filter(Source=="Study") %>% 
  group_by(OfficialName) %>% summarise(n=n()) %>% 
  filter(n>1)


#Getting the latest year
finalData4<-finalData3 %>% group_by(ISO) %>% 
  arrange(ISO, desc(Year)) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(row=row_number())
#-----------------------------
# Meta-anaylsis for each of the sub-envelopes, using random effects.
metaPlot<-metaprop(event=Pretermlt28weeks, n=Preterm, 
                   studlab = OfficialName,
                   subgroup=regionName, data=finalData4, 
                   random=TRUE, 
                   overall = TRUE#, method="Inverse"
                   )
metaPlot2<-metaprop(event=Preterm28tolt32weeks, n=Preterm, 
                    studlab = OfficialName,
                    subgroup=regionName, data=finalData4, 
                    random=TRUE, 
                    overall = TRUE)
metaPlot3<-metaprop(event=Preterm32tolt37weeks, n=Preterm, 
                    studlab = OfficialName,
                    subgroup=regionName, data=finalData4, 
                    random=TRUE, 
                    overall = TRUE)

#Plotting the forest plots of these meta-analyses - uncomment to output forest plot

# pdf(file = paste0("output/" ,substr(fileName, 1, nchar(fileName)),"_", 
#                   ((niter-nburnin)/nthin)*nchains,"_pretermSubgroupForest", ".pdf"), width = 10, height = 23)
# forest.meta(metaPlot, layout="JAMA", common=FALSE, 
#             pooled.totals = TRUE, pooled.events = FALSE)
# grid.text("Preterm<28 weeks / Preterm<37 weeks", 0.5, 0.97, gp=gpar(cex=1))
# forest.meta(metaPlot2, layout="JAMA", common=FALSE, 
#             pooled.totals = TRUE, pooled.events = FALSE)
# grid.text("Preterm 28 - <32 weeks / Preterm<37 weeks", 0.5, 0.97, gp=gpar(cex=1))
# forest.meta(metaPlot3, layout="JAMA", common=FALSE, 
#             pooled.totals = TRUE, pooled.events = FALSE)
# grid.text("Preterm 32 - <37 weeks weeks / Preterm<37 weeks", 0.5, 0.97, gp=gpar(cex=1))
# dev.off()

#------------------------------
#Getting the estimate and upper and lower bounds for each proportion

#By region
metaEstsR<-as.data.frame(list(Pretermlt28weeksL_meta=inv.logit(metaPlot$TE.random.w-1.96*metaPlot$seTE.random.w),
                              Pretermlt28weeks_meta=inv.logit(metaPlot$TE.random.w),
                              Pretermlt28weeksU_meta=inv.logit(metaPlot$TE.random.w+1.96*metaPlot$seTE.random.w),
                              Preterm28tolt32weeksL_meta=inv.logit(metaPlot2$TE.random.w-1.96*metaPlot2$seTE.random.w),
                              Preterm28tolt32weeks_meta=inv.logit(metaPlot2$TE.random.w),
                              Preterm28tolt32weeksU_meta=inv.logit(metaPlot2$TE.random.w+1.96*metaPlot2$seTE.random.w),
                              Preterm32tolt37weeksL_meta=inv.logit(metaPlot3$TE.random.w-1.96*metaPlot3$seTE.random.w),
                              Preterm32tolt37weeks_meta=inv.logit(metaPlot3$TE.random.w),
                              Preterm32tolt37weeksU_meta=inv.logit(metaPlot3$TE.random.w+1.96*metaPlot3$seTE.random.w)))

#For the total estimate
metaEstsT<-as.data.frame(list(Pretermlt28weeksL_meta=inv.logit(metaPlot$TE.random-1.96*metaPlot$seTE.random),
                              Pretermlt28weeks_meta=inv.logit(metaPlot$TE.random),
                              Pretermlt28weeksU_meta=inv.logit(metaPlot$TE.random+1.96*metaPlot$seTE.random),
                              Preterm28tolt32weeksL_meta=inv.logit(metaPlot2$TE.random-1.96*metaPlot2$seTE.random),
                              Preterm28tolt32weeks_meta=inv.logit(metaPlot2$TE.random),
                              Preterm28tolt32weeksU_meta=inv.logit(metaPlot2$TE.random+1.96*metaPlot2$seTE.random),
                              Preterm32tolt37weeksL_meta=inv.logit(metaPlot3$TE.random-1.96*metaPlot3$seTE.random),
                              Preterm32tolt37weeks_meta=inv.logit(metaPlot3$TE.random),
                              Preterm32tolt37weeksU_meta=inv.logit(metaPlot3$TE.random+1.96*metaPlot3$seTE.random)))

lastestMetaEsts<-rbind(metaEstsR %>% mutate(regionName=row.names(metaEstsR)),
                       metaEstsT %>% mutate(regionName="Global"))

#Read in the latest estimates
rGLBW<-read.csv(paste0("output/" ,substr(fileName, 1, nchar(fileName)),"_", 
                       ((niter-nburnin)/nthin)*nchains,"_regionalAndGlobalEstimates", ".csv")) %>% 
  filter(year==2020) %>% 
  dplyr::select(regionName=regionName2, estN, estNL, estNU,
                est) %>% 
  rename(wpp_lb=estN)


#-----
#Applying global total to all regions

latestMetaEsts<-merge(x=rGLBW, y=lastestMetaEsts %>% filter(regionName=="Global") %>% dplyr::select(-regionName), 
                      all.x=TRUE) %>% 
  mutate(diff=(1-(Pretermlt28weeks_meta+Preterm28tolt32weeks_meta+Preterm32tolt37weeks_meta))/3) %>%
  mutate(Pretermlt28weeks_meta=Pretermlt28weeks_meta+diff, 
         Preterm28tolt32weeks_meta=Preterm28tolt32weeks_meta+diff, 
         Preterm32tolt37weeks_meta=Preterm32tolt37weeks_meta+diff) %>% 
  mutate(Pretermlt28weeksNL_meta=Pretermlt28weeksL_meta*wpp_lb,
         Pretermlt28weeksN_meta=Pretermlt28weeks_meta*wpp_lb,
         Pretermlt28weeksNU_meta=Pretermlt28weeksU_meta*wpp_lb,
         Preterm28tolt32weeksNL_meta=Preterm28tolt32weeksL_meta*wpp_lb,
         Preterm28tolt32weeksN_meta=Preterm28tolt32weeks_meta*wpp_lb,
         Preterm28tolt32weeksNU_meta=Preterm28tolt32weeksU_meta*wpp_lb,
         Preterm32tolt37weeksNL_meta=Preterm32tolt37weeksL_meta*wpp_lb,
         Preterm32tolt37weeksN_meta=Preterm32tolt37weeks_meta*wpp_lb,
         Preterm32tolt37weeksNU_meta=Preterm32tolt37weeksU_meta*wpp_lb) %>% 
  arrange(factor(regionName, levels=c("Latin America and the Caribbean",
                                      "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)",
                                      "Northern America, Australia and New Zealand, Central Asia and Europe",
                                      "Southern Asia",
                                      "Sub-Saharan Africa",
                                      "Western Asia and Northern Africa" ,
                                      "Total"))) %>% 
  mutate(sum=Pretermlt28weeks_meta+Preterm28tolt32weeks_meta+
           Preterm32tolt37weeks_meta,
         sumN=Pretermlt28weeksN_meta+Preterm28tolt32weeksN_meta+
           Preterm32tolt37weeksN_meta) %>% 
  mutate(Pretermlt28weeks=paste0(round(Pretermlt28weeks_meta*100, 1), " (", round(Pretermlt28weeksL_meta*100,1), ", ", round(Pretermlt28weeksU_meta*100,1), ")"), 
         Pretermlt28weeksN=paste0(round(Pretermlt28weeksN_meta, 0), " (", round(Pretermlt28weeksNL_meta,0), ", ", round(Pretermlt28weeksNU_meta,0), ")"),
         Preterm28tolt32weeks=paste0(round(Preterm28tolt32weeks_meta*100, 1), " (", round(Preterm28tolt32weeksL_meta*100,1), ", ", round(Preterm28tolt32weeksU_meta*100,1), ")"), 
         Preterm28tolt32weeksN=paste0(round(Preterm28tolt32weeksN_meta, 0), " (", round(Preterm28tolt32weeksNL_meta,0), ", ", round(Preterm28tolt32weeksNU_meta,0), ")"),
         Preterm32tolt37weeks=paste0(round(Preterm32tolt37weeks_meta*100, 1), " (", round(Preterm32tolt37weeksL_meta*100,1), ", ", round(Preterm32tolt37weeksU_meta*100,1), ")"), 
         Preterm32tolt37weeksN=paste0(round(Preterm32tolt37weeksN_meta, 0), " (", round(Preterm32tolt37weeksNL_meta,0), ", ", round(Preterm32tolt37weeksNU_meta,0), ")")) %>% 
  dplyr::select(regionName, Pretermlt28weeks,Pretermlt28weeksN,
                Preterm28tolt32weeks,Preterm28tolt32weeksN, 
                Preterm32tolt37weeks, Preterm32tolt37weeksN)

write.csv(latestMetaEsts, paste0("output/" ,substr(fileName, 1, nchar(fileName)),"_", 
                                 ((niter-nburnin)/nthin)*nchains,"_pretermSubgroupRegionalResultsTable", ".csv"))

#------------------
#Plotting the subgroups

latestMetaEsts2<-merge(x=rGLBW, y=lastestMetaEsts %>% filter(regionName=="Global") %>% dplyr::select(-regionName), 
                      all.x=TRUE) %>% 
  mutate(diff=(1-(Pretermlt28weeks_meta+Preterm28tolt32weeks_meta+Preterm32tolt37weeks_meta))/3) %>%
  mutate(Pretermlt28weeks_meta=Pretermlt28weeks_meta+diff, 
         Preterm28tolt32weeks_meta=Preterm28tolt32weeks_meta+diff, 
         Preterm32tolt37weeks_meta=Preterm32tolt37weeks_meta+diff) %>% 
  mutate(Pretermlt28weeksNL_meta=Pretermlt28weeksL_meta*wpp_lb,
         Pretermlt28weeksN_meta=Pretermlt28weeks_meta*wpp_lb,
         Pretermlt28weeksNU_meta=Pretermlt28weeksU_meta*wpp_lb,
         Preterm28tolt32weeksNL_meta=Preterm28tolt32weeksL_meta*wpp_lb,
         Preterm28tolt32weeksN_meta=Preterm28tolt32weeks_meta*wpp_lb,
         Preterm28tolt32weeksNU_meta=Preterm28tolt32weeksU_meta*wpp_lb,
         Preterm32tolt37weeksNL_meta=Preterm32tolt37weeksL_meta*wpp_lb,
         Preterm32tolt37weeksN_meta=Preterm32tolt37weeks_meta*wpp_lb,
         Preterm32tolt37weeksNU_meta=Preterm32tolt37weeksU_meta*wpp_lb) %>% 
  arrange(factor(regionName, levels=c("Latin America and the Caribbean",
                                      "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)",
                                      "Northern America, Australia and New Zealand, Central Asia and Europe",
                                      "Southern Asia",
                                      "Sub-Saharan Africa",
                                      "Western Asia and Northern Africa" ,
                                      "Total"))) %>% mutate(sum=Pretermlt28weeks_meta+Preterm28tolt32weeks_meta+
                                                           Preterm32tolt37weeks_meta,
                                                         sumN=Pretermlt28weeksN_meta+Preterm28tolt32weeksN_meta+
                                                           Preterm32tolt37weeksN_meta)


transform<-latestMetaEsts2 %>% 
  dplyr::select(regionName, "Preterm < 28 weeks"=Pretermlt28weeksN_meta,
                "Preterm 28 - <32 weeks"=Preterm28tolt32weeksN_meta,
                "Preterm 32 - <37 weeks"=Preterm32tolt37weeksN_meta,
                estNU, estNL, estN=wpp_lb, est) %>% 
  pivot_longer(-c(regionName, estNU, estNL, estN, est), 
               names_to="Subgroups", 
               values_to = "values")

transform$regionName<-factor(transform$regionName, c("Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)",

  "Latin America and the Caribbean",
                                                     "Northern America, Australia and New Zealand, Central Asia and Europe" ,  
  "Southern Asia"    ,                                                             
  
                                                     
                                                     "Sub-Saharan Africa"      ,  
  "Western Asia and Northern Africa", 
  
                                                     
                                                                                                          
                                                             "Global"))

ggplot(transform, aes(x=regionName, 
                                           y=values, fill=Subgroups))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#5c3a93',"#7E38B7",'#9379BD'), 
                    name="")+
  geom_errorbar(aes(ymin=estNL, 
                    ymax=estNU), alpha=0.5, width=0.2
  )+
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.1, 0.9))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(x="", y="Number of preterm babies (millions)")+
  scale_y_continuous(lim=c(0, 16000000), 
                     labels = unit_format(unit="",scale = 1e-6))
wpp<-merge(x=readRDS("output/wpp2.rds") %>% filter(year==2020) %>% 
             group_by(regionName2) %>% 
             summarise(wpp_lb=sum(wpp_lb)), 
           y=rGLBW %>% dplyr::select(regionName2=regionName, est) %>% 
             mutate(est=round(est*100, 1)), 
           by="regionName2", all.y=TRUE) %>% 
  mutate(wpp_lb=ifelse(regionName2=="Global", 
                       sum(wpp_lb, na.rm=TRUE), 
                       wpp_lb)) %>% 
  arrange(match(regionName2, c("Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)",
                               "Latin America and the Caribbean" ,                                              
                               "Northern America, Australia and New Zealand, Central Asia and Europe" ,          
                               "Southern Asia"    ,                                                             
                               "Sub-Saharan Africa"      ,                                                       
                               "Western Asia and Northern Africa",                                              
                               "Global")))

#Bars in height order
transform$regionName<-factor(transform$regionName, c("Latin America and the Caribbean",
                                                     "Northern America, Australia and New Zealand, Central Asia and Europe" ,          
                                                     "Western Asia and Northern Africa", 
                                                     "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)",
                                                     
                                                     "Sub-Saharan Africa"      ,                                                       
                                                     
                                                     
                                                     "Southern Asia"    ,                                                             
                                                     "Global"))
transform$Subgroups<-factor(transform$Subgroups, 
                            c("Preterm 32 - <37 weeks",
                              "Preterm 28 - <32 weeks",
                              "Preterm < 28 weeks" ))
write.csv(transform, paste0("output/" ,substr(fileName, 1, nchar(fileName)),"_", 
                            ((niter-nburnin)/nthin)*nchains,"_Figure3data", ".csv"))


p1<-ggplot(transform #%>% 
         #filter(regionName!="Global")
       , aes(x=regionName, 
                      y=values, fill=Subgroups))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#5c3a93',"#7E38B7",'#9379BD'), 
                    name="", 
                    breaks=c("Preterm < 28 weeks" ,
                             "Preterm 28 - <32 weeks",
                             "Preterm 32 - <37 weeks"
                             
                             ))+
  geom_errorbar(aes(ymin=estNL, 
                    ymax=estNU), alpha=0.5, width=0.2
                )+
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.1, 0.9))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(x="", y="Number of preterm babies (millions)")+
  scale_y_continuous(lim=c(0, 16000000), 
                     labels = unit_format(unit="",scale = 1e-6))

wpp<-merge(x=readRDS("output/wpp2.rds") %>% filter(year==2020) %>% 
  group_by(regionName2) %>% 
  summarise(wpp_lb=sum(wpp_lb)), 
  y=rGLBW %>% dplyr::select(regionName2=regionName, est) %>% 
    mutate(est=round(est*100, 1)), 
  by="regionName2", all.y=TRUE) %>% 
  mutate(wpp_lb=ifelse(regionName2=="Global", 
                       sum(wpp_lb, na.rm=TRUE), 
                       wpp_lb)) %>% 
  arrange(match(regionName2, c("Latin America and the Caribbean",
                               "Northern America, Australia and New Zealand, Central Asia and Europe" ,          
                               "Western Asia and Northern Africa", 
                               "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)",
                               
                               "Sub-Saharan Africa"      ,                                                       
                               
                               
                               "Southern Asia"    ,                                                             
                               "Global")))

# write.csv(wpp, paste0("output/" ,substr(fileName, 1, nchar(fileName)),"_", 
#                       ((niter-nburnin)/nthin)*nchains,"_Figure4table-reorder", ".csv"))
# 


# doc <- rvg::dml(doc, ggobj = p1, type = 'body')  
# # Write the document to a file
# #print(doc, target = 'PRETERM/output/Figure4editable.pptx')
# 
# officer::read_pptx() %>%
#   # add slide ----
# officer::add_slide() %>%
#   # specify object and location of object ----
# officer::ph_with(doc, ph_location()) %>%
#   # export slide -----
# base::print(
#   target = paste0("output/" ,substr(fileName, 1, nchar(fileName)),"_", 
#                   ((niter-nburnin)/nthin)*nchains,"_Figure4editable.pptx")
# )
