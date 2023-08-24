finalData<-readRDS("output/LBWfinalInputDatabase.RDS")
regionCodes<-readRDS("output/regionCodes.RDS")
regionCodesOther<-readRDS("output/regionCodesOther.rds") %>% 
  rename(regionName2=regionName, 
         regionIndex2=regionIndex)

#---------------------------------------------------
#Getting only the data with sub-envelopes 
finalData2<-finalData %>% mutate(sourceIndex=ifelse(Source=="Admin", 1, 0),
                                 newFlagN=ifelse(newFlag=="Ai"| newFlag=="Aii", 1,
                                                 ifelse(newFlag==1 |newFlag=="Bi", 2,
                                                        ifelse(newFlag=="Bii", 3, 
                                                               ifelse(newFlag=="Ci", 4, 
                                                                      ifelse(newFlag=="Cii", 5, NA)))))) %>% 
  dplyr::select(ISO, OfficialName, regionName, regionIndex, Year, sourceIndex,
                NumberofallLBs6, NumberofbabieswithBW,
                LBWgte2500g="LBBW>/=2500g", LBWlt500g, LBW500to999g,
                LBW1500to2499g, LBWlt1500g,
                LBWlt1000g, LBW1000to1499g, LBW1500to1999g, 
                LBW2000to2499g, LBWlt2500g) %>% 
  mutate(LBWlt1000g=ifelse(is.na(LBWlt500g) & is.na(LBW500to999g), NA, LBWlt1000g), 
         LBWlt1000g=ifelse(is.na(LBWlt1000g) &!is.na(LBW1000to1499g) & 
                             !is.na(LBW1500to1999g) &!is.na(LBW2000to2499g),
                           LBWlt2500g-LBW2000to2499g-LBW1500to1999g-LBW1000to1499g,
                           LBWlt1000g),
         proplt1000g=LBWlt1000g/LBWlt2500g , 
         prop1000to1499g=LBW1000to1499g/LBWlt2500g, 
         prop1500to1999g=LBW1500to1999g/LBWlt2500g,
         prop2000to2499g=LBW2000to2499g/LBWlt2500g) %>% 
  filter(!is.na(LBWlt1000g) & !is.na(LBW1000to1499g) &
           !is.na(LBW1500to1999g) & !is.na(LBW2000to2499g) &
           !is.na(LBWlt2500g)) %>% 
  mutate(row=row_number())

#Using only the latest year from each country
finalData3<-finalData2 %>% group_by(ISO) %>% 
  arrange(ISO, desc(Year)) %>% 
  filter(row_number()==1)

#------------------------------
#Coverage two ways

#coverage of countries in the region
regions<-finalData3 %>% group_by(regionName) %>% 
  summarise(nC=length(unique(ISO)))
regionsAll<-regionCodesOther %>% group_by(regionName2) %>% 
  summarise(total=length(unique(ISO)))

coverage<-merge(x=regions, y=regionsAll %>% rename(regionName=regionName2), 
                by="regionName", all.y=TRUE) %>% 
  mutate(percent=nC/total*100)


#UNICEF way- coverage of livebirths

data<-finalData2 %>% 
  filter(Year>=2015) %>% 
  dplyr::select(ISO) %>% distinct()

wpp2020<-wpp2 %>% 
  filter(year==2020)

wppR<-wpp2020 %>% group_by(regionName2) %>% 
  summarise(wppR=sum(wpp_lb))

dataUsed<-merge(x=wpp2020 %>% filter(ISO %in% data$ISO) %>% 
                  group_by(regionName2) %>% summarise(wpp=sum(wpp_lb)),
                y=wppR, by="regionName2") %>% 
  mutate(perc=round(wpp/wppR*100,1))

global<-wpp2020 %>% filter(ISO %in% data$ISO)

sum(global$wpp_lb)/sum(wpp2020$wpp_lb)

##WB regions

wbregionCodes<-read.csv("input/sbr_regions v2.csv") %>% dplyr::select(ISO=ISO3Code,
                                                                      WBRegion4)
wppC<-merge(x=wpp2020,
            y=wbregionCodes, by="ISO", all.x=TRUE)%>%  
  group_by(WBRegion4) %>% 
  summarise(wppR=sum(wpp_lb))

wpp3<-merge(x=wpp2020, 
            y=wbregionCodes, by="ISO", 
            all.x=TRUE)

dataUsed<-merge(x=wpp3 %>% filter(ISO %in% data$ISO) %>% 
                  group_by(WBRegion4) %>% summarise(wpp=sum(wpp_lb)),
                y=wppC, by="WBRegion4") %>% 
  mutate(perc=round(wpp/wppR*100,1))



#-----------------------------
# Meta-anaylsis for each of the sub-envelopes, using random effects.
metaPlot<-metaprop(event=LBWlt1000g, n=LBWlt2500g, 
                   studlab = OfficialName,
                   subgroup=regionName, data=finalData3, 
                   random=TRUE, 
                   overall = TRUE, method="Inverse")

metaPlot2<-metaprop(event=LBW1000to1499g, n=LBWlt2500g, 
                    studlab = OfficialName,
                    subgroup=regionName, data=finalData3, 
                    random=TRUE, 
                    overall = TRUE)
metaPlot3<-metaprop(event=LBW1500to1999g, n=LBWlt2500g, 
                    studlab = OfficialName,
                    subgroup=regionName, data=finalData3, 
                    random=TRUE, 
                    overall = TRUE)
metaPlot4<-metaprop(event=LBW2000to2499g, n=LBWlt2500g, 
                    studlab = OfficialName,
                    subgroup=regionName, data=finalData3, 
                    random=TRUE, 
                    overall = TRUE)

#Plotting the forest plots of these meta-analyses
#pdf(file = "output/LBW subgroup.pdf", width = 10, height = 21)
forest.meta(metaPlot, layout="JAMA", common=FALSE, 
            pooled.totals = FALSE, pooled.events = FALSE)
grid.text("LBW<1000g / LBW<2500g", 0.5, 0.97, gp=gpar(cex=1))
forest.meta(metaPlot2, layout="JAMA", common=FALSE, 
            pooled.totals = FALSE, pooled.events = FALSE)
grid.text("LBW 1000-1499g / LBW<2500g", 0.5, 0.97, gp=gpar(cex=1))
forest.meta(metaPlot3, layout="JAMA", common=FALSE, 
            pooled.totals = FALSE, pooled.events = FALSE)
grid.text("LBW 1500-1999g / LBW<2500g", 0.5, 0.97, gp=gpar(cex=1))
forest.meta(metaPlot4, layout="JAMA", common=FALSE, 
            pooled.totals = FALSE, pooled.events = FALSE)
grid.text("LBW 2000-2499g / LBW<2500g", 0.5, 0.97, gp=gpar(cex=1))
#dev.off()

#------------------------------
#Getting the estimate and upper and lower bounds for each proportion

#By region
metaEstsR<-as.data.frame(list(LBWlt1000gL_meta=inv.logit(metaPlot$TE.random.w-1.96*metaPlot$seTE.random.w),
                              LBWlt1000g_meta=inv.logit(metaPlot$TE.random.w),
                              LBWlt1000gU_meta=inv.logit(metaPlot$TE.random.w+1.96*metaPlot$seTE.random.w),
                              LBW1000to1499gL_meta=inv.logit(metaPlot2$TE.random.w-1.96*metaPlot2$seTE.random.w),
                              LBW1000to1499g_meta=inv.logit(metaPlot2$TE.random.w),
                              LBW1000to1499gU_meta=inv.logit(metaPlot2$TE.random.w+1.96*metaPlot2$seTE.random.w),
                              LBW1500to1999gL_meta=inv.logit(metaPlot3$TE.random.w-1.96*metaPlot3$seTE.random.w),
                              LBW1500to1999g_meta=inv.logit(metaPlot3$TE.random.w),
                              LBW1500to1999gU_meta=inv.logit(metaPlot3$TE.random.w+1.96*metaPlot3$seTE.random.w),
                              LBW2000to2499gL_meta=inv.logit(metaPlot4$TE.random.w-1.96*metaPlot4$seTE.random.w),
                              LBW2000to2499g_meta=inv.logit(metaPlot4$TE.random.w),
                              LBW2000to2499gU_meta=inv.logit(metaPlot4$TE.random.w+1.96*metaPlot4$seTE.random.w)))

#For the total estimate
metaEstsT<-as.data.frame(list(LBWlt1000gL_meta=inv.logit(metaPlot$TE.random-1.96*metaPlot$seTE.random),
                              LBWlt1000g_meta=inv.logit(metaPlot$TE.random),
                              LBWlt1000gU_meta=inv.logit(metaPlot$TE.random+1.96*metaPlot$seTE.random),
                              LBW1000to1499gL_meta=inv.logit(metaPlot2$TE.random-1.96*metaPlot2$seTE.random),
                              LBW1000to1499g_meta=inv.logit(metaPlot2$TE.random),
                              LBW1000to1499gU_meta=inv.logit(metaPlot2$TE.random+1.96*metaPlot2$seTE.random),
                              LBW1500to1999gL_meta=inv.logit(metaPlot3$TE.random-1.96*metaPlot3$seTE.random),
                              LBW1500to1999g_meta=inv.logit(metaPlot3$TE.random),
                              LBW1500to1999gU_meta=inv.logit(metaPlot3$TE.random+1.96*metaPlot3$seTE.random),
                              LBW2000to2499gL_meta=inv.logit(metaPlot4$TE.random-1.96*metaPlot4$seTE.random),
                              LBW2000to2499g_meta=inv.logit(metaPlot4$TE.random),
                              LBW2000to2499gU_meta=inv.logit(metaPlot4$TE.random+1.96*metaPlot4$seTE.random)))

lastestMetaEsts<-rbind(metaEstsR %>% mutate(regionName=row.names(metaEstsR)),
                       metaEstsT %>% mutate(regionName="Global"))

#Read in the latest estimates
  rGLBW<-read.csv(paste0("output/pCCFullModel","_", 
                                             ((niter-nburnin)/nthin)*nchains,"_regionalAndGlobalEstimates.csv")) %>% 
  filter(year==2020) %>% dplyr::select(regionName, estN, estNU, 
                                       estNL, est) %>% 
  rename(wpp_lb=estN) 

total<-rGLBW %>% summarise(wpp_lb=sum(wpp_lb)) %>% 
  mutate(regionName="Total")

#-----
#Applying global total to all regions

alt<-merge(x=rGLBW, y=lastestMetaEsts %>% filter(regionName=="Global") %>% 
             dplyr::select(-regionName))%>% 
  mutate(diff=(1-(LBWlt1000g_meta+LBW1000to1499g_meta+LBW1500to1999g_meta+
                    LBW2000to2499g_meta))/4) %>%
  mutate(LBWlt1000g_meta=LBWlt1000g_meta+diff, 
         LBW1000to1499g_meta=LBW1000to1499g_meta+diff, 
         LBW1500to1999g_meta=LBW1500to1999g_meta+diff,
         LBW2000to2499g_meta=LBW2000to2499g_meta+diff) %>%
  mutate(LBWlt1000gNL_meta=LBWlt1000gL_meta*wpp_lb,
         LBWlt1000gN_meta=LBWlt1000g_meta*wpp_lb,
         LBWlt1000gNU_meta=LBWlt1000gU_meta*wpp_lb,
         LBW1000to1499gNL_meta=LBW1000to1499gL_meta*wpp_lb,
         LBW1000to1499gN_meta=LBW1000to1499g_meta*wpp_lb,
         LBW1000to1499gNU_meta=LBW1000to1499gU_meta*wpp_lb,
         LBW1500to1999gNL_meta=LBW1500to1999gL_meta*wpp_lb,
         LBW1500to1999gN_meta=LBW1500to1999g_meta*wpp_lb,
         LBW1500to1999gNU_meta=LBW1500to1999gU_meta*wpp_lb,
         LBW2000to2499gNL_meta=LBW2000to2499gL_meta*wpp_lb,
         LBW2000to2499gN_meta=LBW2000to2499g_meta*wpp_lb,
         LBW2000to2499gNU_meta=LBW2000to2499gU_meta*wpp_lb) %>% 
  arrange(factor(regionName, levels=c("Latin America and the Caribbean",
                                      "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)",
                                      "Northern America, Australia and New Zealand, Central Asia and Europe",
                                      "Southern Asia",
                                      "Sub-Saharan Africa",
                                      "Western Asia and Northern Africa" ,
                                      "Total"))) %>% 
  mutate(LBWlt1000g=paste0(round(LBWlt1000g_meta*100, 1), " (", round(LBWlt1000gL_meta*100,1), ", ", round(LBWlt1000gU_meta*100,1), ")"), 
         LBWlt1000gN=paste0(round(LBWlt1000gN_meta, 0), " (", round(LBWlt1000gNL_meta,0), ", ", round(LBWlt1000gNU_meta,0), ")"),
         LBW1000to1499g=paste0(round(LBW1000to1499g_meta*100, 1), " (", round(LBW1000to1499gL_meta*100,1), ", ", round(LBW1000to1499gU_meta*100,1), ")"), 
         LBW1000to1499gN=paste0(round(LBW1000to1499gN_meta, 0), " (", round(LBW1000to1499gNL_meta,0), ", ", round(LBW1000to1499gNU_meta,0), ")"),
         LBW1500to1999g=paste0(round(LBW1500to1999g_meta*100, 1), " (", round(LBW1500to1999gL_meta*100,1), ", ", round(LBW1500to1999gU_meta*100,1), ")"), 
         LBW1500to1999gN=paste0(round(LBW1500to1999gN_meta, 0), " (", round(LBW1500to1999gNL_meta,0), ", ", round(LBW1500to1999gNU_meta,0), ")"),
         LBW2000to2499g=paste0(round(LBW2000to2499g_meta*100, 1), " (", round(LBW2000to2499gL_meta*100,1), ", ", round(LBW2000to2499gU_meta*100,1), ")"), 
         LBW2000to2499gN=paste0(round(LBW2000to2499gN_meta, 0), " (", round(LBW2000to2499gNL_meta,0), ", ", round(LBW2000to2499gNU_meta,0), ")")) %>% 
  dplyr::select(regionName, LBWlt1000g,LBWlt1000gN,
                LBW1000to1499g,LBW1000to1499gN, 
                LBW1500to1999g, LBW1500to1999gN,
                LBW2000to2499g, LBW2000to2499gN)
#write.csv(alt, "output/Table 10 - Meta Subgroup (applying total to LBW rates).csv")


#Figure 
alt2<-merge(x=rGLBW, y=lastestMetaEsts %>% filter(regionName=="Global") %>% 
              dplyr::select(-regionName))%>% 
  mutate(diff=(1-(LBWlt1000g_meta+LBW1000to1499g_meta+LBW1500to1999g_meta+
                    LBW2000to2499g_meta))/4) %>%
  mutate(LBWlt1000g_meta=LBWlt1000g_meta+diff, 
         LBW1000to1499g_meta=LBW1000to1499g_meta+diff, 
         LBW1500to1999g_meta=LBW1500to1999g_meta+diff,
         LBW2000to2499g_meta=LBW2000to2499g_meta+diff) %>%
  mutate(LBWlt1000gNL_meta=LBWlt1000gL_meta*wpp_lb,
         LBWlt1000gN_meta=LBWlt1000g_meta*wpp_lb,
         LBWlt1000gNU_meta=LBWlt1000gU_meta*wpp_lb,
         LBW1000to1499gNL_meta=LBW1000to1499gL_meta*wpp_lb,
         LBW1000to1499gN_meta=LBW1000to1499g_meta*wpp_lb,
         LBW1000to1499gNU_meta=LBW1000to1499gU_meta*wpp_lb,
         LBW1500to1999gNL_meta=LBW1500to1999gL_meta*wpp_lb,
         LBW1500to1999gN_meta=LBW1500to1999g_meta*wpp_lb,
         LBW1500to1999gNU_meta=LBW1500to1999gU_meta*wpp_lb,
         LBW2000to2499gNL_meta=LBW2000to2499gL_meta*wpp_lb,
         LBW2000to2499gN_meta=LBW2000to2499g_meta*wpp_lb,
         LBW2000to2499gNU_meta=LBW2000to2499gU_meta*wpp_lb) %>% 
  arrange(factor(regionName, levels=c("Latin America and the Caribbean",
                                      "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)",
                                      "Northern America, Australia and New Zealand, Central Asia and Europe",
                                      "Southern Asia",
                                      "Sub-Saharan Africa",
                                      "Western Asia and Northern Africa" ,
                                      "Total")))

transform<-alt2 %>% 
  dplyr::select(regionName, "LBW < 1000g"=LBWlt1000gN_meta,
                "LBW 1000 - 1499g"=LBW1000to1499gN_meta,
                "LBW 1500 - 1999g"=LBW1500to1999gN_meta,
                "LBW 2000 - 2499g"=LBW2000to2499gN_meta,
                estNU, estNL, estN=wpp_lb, est) %>% 
  pivot_longer(-c(regionName, estNU, estNL, estN, est), 
               names_to="Subgroups", 
               values_to = "values")

#In height order
transform$regionName<-factor(transform$regionName, c("Latin America and the Caribbean" ,
                                                     "Northern America, Australia and New Zealand, Central Asia and Europe" ,
                                                     "Western Asia and Northern Africa",
                                                     "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)",
                                                     
                                                     "Sub-Saharan Africa"      ,
                                                     
                                                     
                                                     "Southern Asia"    ,
                                                     "Global"))


ggplot(transform, aes(x=regionName, 
                      y=values, fill=Subgroups))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#006a4e',"#00C301",'#BFEEB7', "#DDF0DA"), 
                    name="")+
  geom_errorbar(aes(ymin=estNL, 
                    ymax=estNU), alpha=0.5, width=0.2
  )+
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.1, 0.9))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(x="", y="Number of LBW babies (millions)")+
  scale_y_continuous(lim=c(0, 25000000), 
                     labels = unit_format(unit="",scale = 1e-6))

transform<-alt2 %>% 
  dplyr::select(regionName, "LBW < 1000g"=LBWlt1000gN_meta,
                "LBW 1000 - 1499g"=LBW1000to1499gN_meta,
                "LBW 1500 - 1999g"=LBW1500to1999gN_meta,
                "LBW 2000 - 2499g"=LBW2000to2499gN_meta,
                estNU, estNL, estN=wpp_lb, est) %>% 
  pivot_longer(-c(regionName, estNU, estNL, estN, est), 
               names_to="Subgroups", 
               values_to = "values")
transform2<-alt2 %>% 
  dplyr::select(regionName, "LBW < 1000g"=LBWlt1000gNL_meta,
                "LBW 1000 - 1499g"=LBW1000to1499gNL_meta,
                "LBW 1500 - 1999g"=LBW1500to1999gNL_meta,
                "LBW 2000 - 2499g"=LBW2000to2499gNL_meta) %>% 
  pivot_longer(-c(regionName), 
               names_to="Subgroups", 
               values_to = "lower")
transform3<-alt2 %>% 
  dplyr::select(regionName, "LBW < 1000g"=LBWlt1000gNU_meta,
                "LBW 1000 - 1499g"=LBW1000to1499gNU_meta,
                "LBW 1500 - 1999g"=LBW1500to1999gNU_meta,
                "LBW 2000 - 2499g"=LBW2000to2499gNU_meta) %>% 
  pivot_longer(-c(regionName), 
               names_to="Subgroups", 
               values_to = "upper")

transformAll<-merge(x=merge(x=transform, y=transform2, 
                            by=c("regionName", "Subgroups"),
                            all.x=TRUE), y=transform3, by=c("regionName", "Subgroups"),all.x=TRUE)

transformAll %>% 
  filter(regionName=="Northern America, Australia and New Zealand, Central Asia and Europe") %>% 
  ggplot(aes(x=Subgroups, 
             y=values, fill=Subgroups))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#006a4e',"#00C301",'#BFEEB7', "#DDF0DA"), 
                    name="")+
  geom_errorbar(aes(ymin=lower, 
                    ymax=upper), alpha=0.5, width=0.2
  )+
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(x="", y="Number of LBW babies (thousands)")+
  scale_y_continuous(lim=c(0, 800000), 
                     labels = unit_format(unit="",scale = 1e-4))


#In height order
wpp<-merge(x=wpp2020 %>% 
             group_by(regionName2) %>% 
             summarise(wpp_lb=sum(wpp_lb)), 
           y=rGLBW %>% dplyr::select(regionName2=regionName, est) %>% 
             mutate(est=round(est*100, 1)), 
           by="regionName2", all.y=TRUE) %>% 
  mutate(wpp_lb=ifelse(regionName2=="Global", 
                       sum(wpp_lb, na.rm=TRUE), 
                       wpp_lb)) %>% 
  arrange(match(regionName2, c("Latin America and the Caribbean" ,
                               "Northern America, Australia and New Zealand, Central Asia and Europe" ,
                               "Western Asia and Northern Africa",
                               "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)",
                               
                               "Sub-Saharan Africa"      ,
                               
                               
                               "Southern Asia"    ,
                               "Global")))

#write.csv(wpp, "output/Figure4table-heightOrder.csv")
#-------------
# Using all the years available
metaPlot2<-metaprop(event=LBWlt1000g, n=LBWlt2500g, 
                    studlab = row,
                    subgroup=regionName, data=finalData2, 
                    random=TRUE, 
                    overall = TRUE)
metaPlot22<-metaprop(event=LBW1000to1499g, n=LBWlt2500g, 
                     studlab = row,
                     subgroup=regionName, data=finalData2, 
                     random=TRUE, 
                     overall = TRUE)
metaPlot32<-metaprop(event=LBW1500to1999g, n=LBWlt2500g, 
                     studlab = row,
                     subgroup=regionName, data=finalData2, 
                     random=TRUE, 
                     overall = TRUE)
metaPlot42<-metaprop(event=LBW2000to2499g, n=LBWlt2500g, 
                     studlab = row,
                     subgroup=regionName, data=finalData2, 
                     random=TRUE, 
                     overall = TRUE)

metaEstsR<-as.data.frame(list(LBWlt1000gL_meta=inv.logit(metaPlot2$TE.random.w-1.96*metaPlot2$seTE.random.w),
                              LBWlt1000g_meta=inv.logit(metaPlot2$TE.random.w),
                              LBWlt1000gU_meta=inv.logit(metaPlot2$TE.random.w+1.96*metaPlot2$seTE.random.w),
                              LBW1000to1499gL_meta=inv.logit(metaPlot22$TE.random.w-1.96*metaPlot22$seTE.random.w),
                              LBW1000to1499g_meta=inv.logit(metaPlot22$TE.random.w),
                              LBW1000to1499gU_meta=inv.logit(metaPlot22$TE.random.w+1.96*metaPlot22$seTE.random.w),
                              LBW1500to1999gL_meta=inv.logit(metaPlot32$TE.random.w-1.96*metaPlot32$seTE.random.w),
                              LBW1500to1999g_meta=inv.logit(metaPlot32$TE.random.w),
                              LBW1500to1999gU_meta=inv.logit(metaPlot32$TE.random.w+1.96*metaPlot32$seTE.random.w),
                              LBW2000to2499gL_meta=inv.logit(metaPlot42$TE.random.w-1.96*metaPlot42$seTE.random.w),
                              LBW2000to2499g_meta=inv.logit(metaPlot42$TE.random.w),
                              LBW2000to2499gU_meta=inv.logit(metaPlot42$TE.random.w+1.96*metaPlot42$seTE.random.w)))


metaEstsT<-as.data.frame(list(LBWlt1000gL_meta=inv.logit(metaPlot2$TE.random-1.96*metaPlot2$seTE.random),
                              LBWlt1000g_meta=inv.logit(metaPlot2$TE.random),
                              LBWlt1000gU_meta=inv.logit(metaPlot2$TE.random+1.96*metaPlot2$seTE.random),
                              LBW1000to1499gL_meta=inv.logit(metaPlot22$TE.random-1.96*metaPlot22$seTE.random),
                              LBW1000to1499g_meta=inv.logit(metaPlot22$TE.random),
                              LBW1000to1499gU_meta=inv.logit(metaPlot22$TE.random+1.96*metaPlot22$seTE.random),
                              LBW1500to1999gL_meta=inv.logit(metaPlot32$TE.random-1.96*metaPlot32$seTE.random),
                              LBW1500to1999g_meta=inv.logit(metaPlot32$TE.random),
                              LBW1500to1999gU_meta=inv.logit(metaPlot32$TE.random+1.96*metaPlot32$seTE.random),
                              LBW2000to2499gL_meta=inv.logit(metaPlot42$TE.random-1.96*metaPlot42$seTE.random),
                              LBW2000to2499g_meta=inv.logit(metaPlot42$TE.random),
                              LBW2000to2499gU_meta=inv.logit(metaPlot42$TE.random+1.96*metaPlot42$seTE.random)))

allMetaEsts<-rbind(metaEstsR %>% mutate(regionName=row.names(metaEstsR)),
                   metaEstsT %>% mutate(regionName="Global"))

#write.csv(lastestMetaEsts, "output/latestMetaEsts.csv")
#write.csv(allMetaEsts, "output/allMetaEsts.csv")                       



#------------------
#Figure with median and IQR

regional<-finalData2 %>% group_by(regionName) %>% 
  summarise(proplt1000gMedi=median(proplt1000g, na.rm=TRUE), 
            proplt1000gIQR1=quantile(proplt1000g, c(0.25), na.rm=TRUE),
            proplt1000gIQR2=quantile(proplt1000g, c(0.75), na.rm=TRUE),
            prop1000to1499gMedi=median(prop1000to1499g, na.rm=TRUE), 
            prop1000to1499gIQR1=quantile(prop1000to1499g, c(0.25), na.rm=TRUE),
            prop1000to1499gIQR2=quantile(prop1000to1499g, c(0.75), na.rm=TRUE),
            prop1500to1999gMedi=median(prop1500to1999g, na.rm=TRUE), 
            prop1500to1999gIQR1=quantile(prop1500to1999g, c(0.25), na.rm=TRUE), 
            prop1500to1999gIQR2=quantile(prop1500to1999g, c(0.75), na.rm=TRUE), 
            prop2000to2499gMedi=median(prop2000to2499g, na.rm=TRUE), 
            prop2000to2499gIQR1=quantile(prop2000to2499g, c(0.25), na.rm=TRUE),
            prop2000to2499gIQR2=quantile(prop2000to2499g, c(0.75), na.rm=TRUE))
global<-finalData2 %>%   summarise(proplt1000gMedi=median(proplt1000g, na.rm=TRUE), 
                                   proplt1000gIQR1=quantile(proplt1000g, c(0.25), na.rm=TRUE),
                                   proplt1000gIQR2=quantile(proplt1000g, c(0.75), na.rm=TRUE),
                                   prop1000to1499gMedi=median(prop1000to1499g, na.rm=TRUE), 
                                   prop1000to1499gIQR1=quantile(prop1000to1499g, c(0.25), na.rm=TRUE),
                                   prop1000to1499gIQR2=quantile(prop1000to1499g, c(0.75), na.rm=TRUE),
                                   prop1500to1999gMedi=median(prop1500to1999g, na.rm=TRUE), 
                                   prop1500to1999gIQR1=quantile(prop1500to1999g, c(0.25), na.rm=TRUE), 
                                   prop1500to1999gIQR2=quantile(prop1500to1999g, c(0.75), na.rm=TRUE), 
                                   prop2000to2499gMedi=median(prop2000to2499g, na.rm=TRUE), 
                                   prop2000to2499gIQR1=quantile(prop2000to2499g, c(0.25), na.rm=TRUE),
                                   prop2000to2499gIQR2=quantile(prop2000to2499g, c(0.75), na.rm=TRUE)) %>% 
  mutate(regionName="Global")
randG<-rbind(regional, global)

regionalTrans<-randG %>% 
  pivot_longer(!regionName, names_to="Prop", values_to="Values") %>% 
  mutate(which=substr(Prop, nchar(Prop)-3, nchar(Prop)), 
         Prop2=substr(Prop, 1, nchar(Prop)-4)) %>% dplyr::select(-Prop) %>% 
  pivot_wider( names_from = "which", values_from = "Values") %>% 
  mutate(Prop2=ifelse(Prop2=="proplt1000g", "<1000g", 
                      ifelse(Prop2=="prop1000to1499g", "1000g-1499g", 
                             ifelse(Prop2=="prop1500to1999g", "1500g-1999g", 
                                    ifelse(Prop2=="prop2000to2499g", "2000g-2499g", NA))))) %>% 
  mutate(Prop3=paste0(regionName, Prop2))

sA<-regionalTrans %>% filter(regionName=="Global") %>% 
  mutate(regionName="Southern Asia", 
         Medi=0, 
         IQR1=0, 
         IQR2=0) %>% 
  mutate(Prop3=paste0(regionName, Prop2))

regionalTrans<-rbind(regionalTrans, sA)

regionalTrans$regionName<-factor(regionalTrans$regionName, c("Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)","Latin America and the Caribbean" ,                                              
                                                             "Northern America, Australia and New Zealand, Central Asia and Europe" ,          "Southern Asia"    ,                                                             
                                                             "Sub-Saharan Africa"      ,                                                       "Western Asia and Northern Africa",                                              
                                                             "Global"))

#Separate regional plots

ggplot(regionalTrans, aes(x=Prop2, y=Medi, fill=Prop2))+
  geom_bar(stat="identity")+
  theme_bw()+
  geom_errorbar(aes(ymin=IQR1, ymax=IQR2), alpha=0.5, width=0.2)+
  facet_wrap(~regionName)+
  theme(legend.position = c(1,0), legend.justification = c(1,0))+
  ylab("Proportion of LBW babies")+
  xlab("")+
  labs(fill="")+
  scale_fill_manual(values=c('#006a4e',"#00C301",'#BFEEB7', "#DDF0DA"))+
  theme(legend.position = c(1, 0), legend.justification = c(1, 0))

#All one bar chart
regionalTrans$Prop3<-factor(regionalTrans$Prop3, 
                            c(  "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)<1000g",     
                                "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)1000g-1499g",
                                "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)1500g-1999g",
                                "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)2000g-2499g",
                                "Latin America and the Caribbean<1000g"                ,                                    
                                "Latin America and the Caribbean1000g-1499g"      ,                                         
                                "Latin America and the Caribbean1500g-1999g"      ,                                         
                                "Latin America and the Caribbean2000g-2499g"       ,                                        
                                "Northern America, Australia and New Zealand, Central Asia and Europe<1000g" ,              
                                "Northern America, Australia and New Zealand, Central Asia and Europe1000g-1499g" ,         
                                "Northern America, Australia and New Zealand, Central Asia and Europe1500g-1999g",          
                                "Northern America, Australia and New Zealand, Central Asia and Europe2000g-2499g",          
                                "Southern Asia<1000g"  ,                                                                    
                                "Southern Asia1000g-1499g"  ,                                                               
                                "Southern Asia1500g-1999g" ,                                                                
                                "Southern Asia2000g-2499g" ,                                                                
                                "Sub-Saharan Africa<1000g"  ,                                                               
                                "Sub-Saharan Africa1000g-1499g" ,                                                           
                                "Sub-Saharan Africa1500g-1999g" ,                                                           
                                "Sub-Saharan Africa2000g-2499g" ,                                                           
                                "Western Asia and Northern Africa<1000g" ,                                                  
                                "Western Asia and Northern Africa1000g-1499g" ,                                             
                                "Western Asia and Northern Africa1500g-1999g" ,                                             
                                "Western Asia and Northern Africa2000g-2499g" ,                                             
                                "Global<1000g"                ,                                                             
                                "Global1000g-1499g"      ,                                                                  
                                "Global1500g-1999g"    ,                                                                    
                                "Global2000g-2499g"))

finalData2 %>% filter(!is.na(proplt1000g)&!is.na(prop1000to1499g)&
                        !is.na(prop1500to1999g)&!is.na(prop2000to2499g)) %>% 
  group_by(regionName) %>% 
  summarise(nC=length(unique(ISO)))
finalData2 %>% filter(!is.na(proplt1000g)&!is.na(prop1000to1499g)&
                        !is.na(prop1500to1999g)&!is.na(prop2000to2499g)) %>%
  summarise(nC=length(unique(ISO)))

ggplot(regionalTrans, aes(x=Prop3, y=Medi, fill=Prop2))+
  geom_bar(stat="identity")+
  theme_bw()+
  #theme(legend.position = "bottom")+
  ylab("Proportion of LBW newborns (%)")+
  xlab("SDG* regions")+
  labs(fill="")+
  #theme(axis.text.x = element_text(angle=45))+
  geom_errorbar(aes(ymin=IQR1, ymax=IQR2), alpha=0.5, width=0.2)+
  scale_x_discrete(labels=c("Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)<1000g" = "", 
                            "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)1000g-1499g" = "Eastern Asia, South Eastern\n Asia and Oceania (excl.\n Australia and New Zealand)\n n=6",
                            "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)1500g-1999g" = "", 
                            "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)2000g-2499g"="",
                            "Latin America and the Caribbean<1000g" ="",                                                   
                            "Latin America and the Caribbean1000g-1499g"  ="Latin America\n and the\n Caribbean\n n=13",                                             
                            "Latin America and the Caribbean1500g-1999g" ="",                                               
                            "Latin America and the Caribbean2000g-2499g" ="",                                              
                            "Northern America, Australia and New Zealand, Central Asia and Europe<1000g"  ="",             
                            "Northern America, Australia and New Zealand, Central Asia and Europe1000g-1499g"  ="Northern America, Australia\n and New Zealand,\n Central Asia and Europe\n n=42",        
                            "Northern America, Australia and New Zealand, Central Asia and Europe1500g-1999g"  ="",         
                            "Northern America, Australia and New Zealand, Central Asia and Europe2000g-2499g" ="",         
                            "Southern Asia<1000g"     ="",                                                                 
                            "Southern Asia1000g-1499g"  ="Southern Asia \n n=0",                                                               
                            "Southern Asia1500g-1999g"  ="",                                                                
                            "Southern Asia2000g-2499g"  ="",                                                               
                            "Sub-Saharan Africa<1000g"  ="",                                                               
                            "Sub-Saharan Africa1000g-1499g"  ="Sub-Saharan Africa\n n=2",                                                          
                            "Sub-Saharan Africa1500g-1999g"  ="",                                                           
                            "Sub-Saharan Africa2000g-2499g"  ="",                                                          
                            "Western Asia and Northern Africa<1000g" ="",                                                  
                            "Western Asia and Northern Africa1000g-1499g"   ="Western Asia and\n Northern Africa \n n=10",                                           
                            "Western Asia and Northern Africa1500g-1999g"  ="",                                             
                            "Western Asia and Northern Africa2000g-2499g" ="",                                             
                            "Global<1000g"     ="",                                                                        
                            "Global1000g-1499g"  ="Global\n n=73",                                                                      
                            "Global1500g-1999g" ="",                                                                        
                            "Global2000g-2499g"=""))+
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  annotate("rect", xmin=24.5, xmax=28.5, ymin=0, ymax=0.8, alpha=0.3)+
  scale_fill_manual(values=c('#006a4e',"#00C301",'#BFEEB7', "#DDF0DA"))



#Separate bar charts by year
regionCodesOther<-readRDS("output/regionCodesOther.RDS") %>% 
  rename(regionName2=regionName, regionIndex2=regionIndex)

subgroupProp2<-merge(x=finalData2, y=regionCodesOther, 
                     by="ISO", all.x=TRUE)
regional<-subgroupProp2%>% group_by(regionName, Year) %>% 
  summarise(proplt1000gMedi=median(proplt1000g, na.rm=TRUE), 
            proplt1000gIQR1=quantile(proplt1000g, c(0.25), na.rm=TRUE),
            proplt1000gIQR2=quantile(proplt1000g, c(0.75), na.rm=TRUE),
            prop1000to1499gMedi=median(prop1000to1499g, na.rm=TRUE), 
            prop1000to1499gIQR1=quantile(prop1000to1499g, c(0.25), na.rm=TRUE),
            prop1000to1499gIQR2=quantile(prop1000to1499g, c(0.75), na.rm=TRUE),
            prop1500to1999gMedi=median(prop1500to1999g, na.rm=TRUE), 
            prop1500to1999gIQR1=quantile(prop1500to1999g, c(0.25), na.rm=TRUE), 
            prop1500to1999gIQR2=quantile(prop1500to1999g, c(0.75), na.rm=TRUE), 
            prop2000to2499gMedi=median(prop2000to2499g, na.rm=TRUE), 
            prop2000to2499gIQR1=quantile(prop2000to2499g, c(0.25), na.rm=TRUE),
            prop2000to2499gIQR2=quantile(prop2000to2499g, c(0.75), na.rm=TRUE))
global<-finalData2 %>% group_by(Year) %>%   summarise(proplt1000gMedi=median(proplt1000g, na.rm=TRUE), 
                                                      proplt1000gIQR1=quantile(proplt1000g, c(0.25), na.rm=TRUE),
                                                      proplt1000gIQR2=quantile(proplt1000g, c(0.75), na.rm=TRUE),
                                                      prop1000to1499gMedi=median(prop1000to1499g, na.rm=TRUE), 
                                                      prop1000to1499gIQR1=quantile(prop1000to1499g, c(0.25), na.rm=TRUE),
                                                      prop1000to1499gIQR2=quantile(prop1000to1499g, c(0.75), na.rm=TRUE),
                                                      prop1500to1999gMedi=median(prop1500to1999g, na.rm=TRUE), 
                                                      prop1500to1999gIQR1=quantile(prop1500to1999g, c(0.25), na.rm=TRUE), 
                                                      prop1500to1999gIQR2=quantile(prop1500to1999g, c(0.75), na.rm=TRUE), 
                                                      prop2000to2499gMedi=median(prop2000to2499g, na.rm=TRUE), 
                                                      prop2000to2499gIQR1=quantile(prop2000to2499g, c(0.25), na.rm=TRUE),
                                                      prop2000to2499gIQR2=quantile(prop2000to2499g, c(0.75), na.rm=TRUE)) %>% 
  mutate(regionName="Global")
sA<-global %>% 
  mutate(regionName="Southern Asia", 
         proplt1000gMedi=0,    proplt1000gIQR1=0,    proplt1000gIQR2 =0,   prop1000to1499gMedi=0, prop1000to1499gIQR1=0, prop1000to1499gIQR2=0,
         prop1500to1999gMedi=0, prop1500to1999gIQR1=0, prop1500to1999gIQR2=0, prop2000to2499gMedi=0, prop2000to2499gIQR1=0, prop2000to2499gIQR2=0)

randG<-rbind(regional, global, sA)

regionalTrans<-randG %>% 
  pivot_longer(!c(regionName, Year), names_to="Prop", values_to="Values") %>% 
  mutate(which=substr(Prop, nchar(Prop)-3, nchar(Prop)), 
         Prop2=substr(Prop, 1, nchar(Prop)-4)) %>% dplyr::select(-Prop) %>% 
  pivot_wider( names_from = "which", values_from = "Values") %>% 
  mutate(Prop2=ifelse(Prop2=="proplt1000g", "<1000g", 
                      ifelse(Prop2=="prop1000to1499g", "1000g-1499g", 
                             ifelse(Prop2=="prop1500to1999g", "1500g-1999g", 
                                    ifelse(Prop2=="prop2000to2499g", "2000g-2499g", NA))))) %>% 
  mutate(Prop3=paste0(regionName, Prop2)) %>% 
  filter(Year>=2000)%>% 
  mutate(regionName=ifelse(regionName=="Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)", "Eastern Asia, South Eastern Asia and Oceania \n(excl. Australia and New Zealand)\n n=6",
                           ifelse(regionName=="Latin America and the Caribbean","Latin America and the Caribbean\n n=13",
                                  ifelse(regionName=="Northern America, Australia and New Zealand, Central Asia and Europe", "Northern America, Australia and New Zealand, \nCentral Asia and Europe\n n=42",        
                                         ifelse(regionName=="Southern Asia", "Southern Asia\n n=0",
                                                ifelse(regionName=="Sub-Saharan Africa", "Sub-Saharan Africa\n n=2", 
                                                       ifelse(regionName=="Western Asia and Northern Africa", "Western Asia and Northern Africa\n n=10",
                                                              ifelse(regionName=="Global", "Global\n n=73", NA))))))))

regionalTrans$regionName<-factor(regionalTrans$regionName, c("Eastern Asia, South Eastern Asia and Oceania \n(excl. Australia and New Zealand)\n n=6",
                                                             "Latin America and the Caribbean\n n=13" ,                                              
                                                             "Northern America, Australia and New Zealand, \nCentral Asia and Europe\n n=42" ,          
                                                             "Southern Asia\n n=0"    ,                                                             
                                                             "Sub-Saharan Africa\n n=2"      ,                                                       
                                                             "Western Asia and Northern Africa\n n=10",                                              
                                                             "Global\n n=73"))

ggplot(regionalTrans, aes(x=factor(Year), y=Medi, fill=Prop2))+
  geom_bar(stat="identity", position="fill")+
  theme_bw()+
  #geom_errorbar(aes(ymin=IQR1, ymax=IQR2), alpha=0.5, width=0.2)+
  facet_wrap(~regionName)+
  theme(legend.position = c(1,0), legend.justification = c(1,0))+
  ylab("Proportion of LBW newborns")+
  xlab("")+
  labs(fill="")+
  scale_fill_manual(values=c('#006a4e',"#00C301",'#BFEEB7', "#DDF0DA"))+
  scale_x_discrete(labels=c("2000"="2000", "2001"="", "2002"="", "2003"="", "2004"="",
                            "2005"="2005", "2006"="", "2007"="", "2008"="", "2009"="",
                            "2010"="2010", "2011"="", "2012"="", "2013"="", "2014"="",
                            "2015"="2015", "2016"="", "2017"="", "2018"="","2019"="", "2020"="2020"
  ))+
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#------------------------------------
#NA only
#-----------------------------
# Latest time used

finalData4<-finalData3 %>% filter(regionName=="Northern America, Australia and New Zealand, Central Asia and Europe")

metaPlot<-metaprop(event=LBWlt1000g, n=LBWlt2500g, 
                   studlab = OfficialName,
                   subgroup=regionName, data=finalData4, 
                   random=TRUE, 
                   overall = TRUE, method="Inverse")

metaPlot2<-metaprop(event=LBW1000to1499g, n=LBWlt2500g, 
                    studlab = OfficialName,
                    subgroup=regionName, data=finalData4, 
                    random=TRUE, 
                    overall = TRUE)
metaPlot3<-metaprop(event=LBW1500to1999g, n=LBWlt2500g, 
                    studlab = OfficialName,
                    subgroup=regionName, data=finalData4, 
                    random=TRUE, 
                    overall = TRUE)
metaPlot4<-metaprop(event=LBW2000to2499g, n=LBWlt2500g, 
                    studlab = OfficialName,
                    subgroup=regionName, data=finalData4, 
                    random=TRUE, 
                    overall = TRUE)

#pdf(file = "output/LBW subgroup.pdf", width = 10, height = 21)
forest.meta(metaPlot, layout="JAMA", common=FALSE, 
            pooled.totals = FALSE, pooled.events = FALSE)
grid.text("LBW<1000g / LBW<2500g", 0.5, 0.97, gp=gpar(cex=1))
forest.meta(metaPlot2, layout="JAMA", common=FALSE, 
            pooled.totals = FALSE, pooled.events = FALSE)
grid.text("LBW 1000-1499g / LBW<2500g", 0.5, 0.97, gp=gpar(cex=1))
forest.meta(metaPlot3, layout="JAMA", common=FALSE, 
            pooled.totals = FALSE, pooled.events = FALSE)
grid.text("LBW 1500-1999g / LBW<2500g", 0.5, 0.97, gp=gpar(cex=1))
forest.meta(metaPlot4, layout="JAMA", common=FALSE, 
            pooled.totals = FALSE, pooled.events = FALSE)
grid.text("LBW 2000-2499g / LBW<2500g", 0.5, 0.97, gp=gpar(cex=1))
#dev.off()

metaEstsR<-as.data.frame(list(LBWlt1000gL_meta=inv.logit(metaPlot$TE.random.w-1.96*metaPlot$seTE.random.w),
                              LBWlt1000g_meta=inv.logit(metaPlot$TE.random.w),
                              LBWlt1000gU_meta=inv.logit(metaPlot$TE.random.w+1.96*metaPlot$seTE.random.w),
                              LBW1000to1499gL_meta=inv.logit(metaPlot2$TE.random.w-1.96*metaPlot2$seTE.random.w),
                              LBW1000to1499g_meta=inv.logit(metaPlot2$TE.random.w),
                              LBW1000to1499gU_meta=inv.logit(metaPlot2$TE.random.w+1.96*metaPlot2$seTE.random.w),
                              LBW1500to1999gL_meta=inv.logit(metaPlot3$TE.random.w-1.96*metaPlot3$seTE.random.w),
                              LBW1500to1999g_meta=inv.logit(metaPlot3$TE.random.w),
                              LBW1500to1999gU_meta=inv.logit(metaPlot3$TE.random.w+1.96*metaPlot3$seTE.random.w),
                              LBW2000to2499gL_meta=inv.logit(metaPlot4$TE.random.w-1.96*metaPlot4$seTE.random.w),
                              LBW2000to2499g_meta=inv.logit(metaPlot4$TE.random.w),
                              LBW2000to2499gU_meta=inv.logit(metaPlot4$TE.random.w+1.96*metaPlot4$seTE.random.w)))

lastestMetaEsts<-rbind(metaEstsR %>% mutate(regionName=row.names(metaEstsR)))

rGLBW<-read.csv(paste0("output/pCCFullModel","_", 
                       ((niter-nburnin)/nthin)*nchains,"_regionalAndGlobalEstimates.csv")) %>% 
  filter(year==2020) %>% dplyr::select(regionName, estN, estNU, 
                                       estNL, est) %>% 
  rename(wpp_lb=estN) 

total<-rGLBW %>% summarise(wpp_lb=sum(wpp_lb)) %>% 
  mutate(regionName="Total")

# wpp2<-readRDS("output/wpp2.RDS")

latestMetaEsts<-merge(x=lastestMetaEsts, y=rGLBW,
                      by="regionName", all.x=TRUE) %>%
  mutate(LBWlt1000gNL_meta=LBWlt1000gL_meta*wpp_lb,
         LBWlt1000gN_meta=LBWlt1000g_meta*wpp_lb,
         LBWlt1000gNU_meta=LBWlt1000gU_meta*wpp_lb,
         LBW1000to1499gNL_meta=LBW1000to1499gL_meta*wpp_lb,
         LBW1000to1499gN_meta=LBW1000to1499g_meta*wpp_lb,
         LBW1000to1499gNU_meta=LBW1000to1499gU_meta*wpp_lb,
         LBW1500to1999gNL_meta=LBW1500to1999gL_meta*wpp_lb,
         LBW1500to1999gN_meta=LBW1500to1999g_meta*wpp_lb,
         LBW1500to1999gNU_meta=LBW1500to1999gU_meta*wpp_lb,
         LBW2000to2499gNL_meta=LBW2000to2499gL_meta*wpp_lb,
         LBW2000to2499gN_meta=LBW2000to2499g_meta*wpp_lb,
         LBW2000to2499gNU_meta=LBW2000to2499gU_meta*wpp_lb) %>%
  arrange(factor(regionName, levels=c("Latin America and the Caribbean",
                                      "Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)",
                                      "Northern America, Australia and New Zealand, Central Asia and Europe",
                                      "Southern Asia",
                                      "Sub-Saharan Africa",
                                      "Western Asia and Northern Africa" ,
                                      "Total"))) %>%
  mutate(LBWlt1000g=paste0(round(LBWlt1000g_meta*100, 1), " (", round(LBWlt1000gL_meta*100,1), ", ", round(LBWlt1000gU_meta*100,1), ")"),
         LBWlt1000gN=paste0(round(LBWlt1000gN_meta, 0), " (", round(LBWlt1000gNL_meta,0), ", ", round(LBWlt1000gNU_meta,0), ")"),
         LBW1000to1499g=paste0(round(LBW1000to1499g_meta*100, 1), " (", round(LBW1000to1499gL_meta*100,1), ", ", round(LBW1000to1499gU_meta*100,1), ")"),
         LBW1000to1499gN=paste0(round(LBW1000to1499gN_meta, 0), " (", round(LBW1000to1499gNL_meta,0), ", ", round(LBW1000to1499gNU_meta,0), ")"),
         LBW1500to1999g=paste0(round(LBW1500to1999g_meta*100, 1), " (", round(LBW1500to1999gL_meta*100,1), ", ", round(LBW1500to1999gU_meta*100,1), ")"),
         LBW1500to1999gN=paste0(round(LBW1500to1999gN_meta, 0), " (", round(LBW1500to1999gNL_meta,0), ", ", round(LBW1500to1999gNU_meta,0), ")"),
         LBW2000to2499g=paste0(round(LBW2000to2499g_meta*100, 1), " (", round(LBW2000to2499gL_meta*100,1), ", ", round(LBW2000to2499gU_meta*100,1), ")"),
         LBW2000to2499gN=paste0(round(LBW2000to2499gN_meta, 0), " (", round(LBW2000to2499gNL_meta,0), ", ", round(LBW2000to2499gNU_meta,0), ")")) %>%
  dplyr::select(regionName, LBWlt1000g,LBWlt1000gN,
                LBW1000to1499g,LBW1000to1499gN,
                LBW1500to1999g, LBW1500to1999gN,
                LBW2000to2499g, LBW2000to2499gN)

write.csv(latestMetaEsts, "output/LBWsubgroupRegionalEstimates_NAappliedtoall.csv")


#Figure 
figure<-merge(x=lastestMetaEsts, y=rGLBW,
              by="regionName", all.x=TRUE) %>%
  mutate(LBWlt1000gNL_meta=LBWlt1000gL_meta*wpp_lb,
         LBWlt1000gN_meta=LBWlt1000g_meta*wpp_lb,
         LBWlt1000gNU_meta=LBWlt1000gU_meta*wpp_lb,
         LBW1000to1499gNL_meta=LBW1000to1499gL_meta*wpp_lb,
         LBW1000to1499gN_meta=LBW1000to1499g_meta*wpp_lb,
         LBW1000to1499gNU_meta=LBW1000to1499gU_meta*wpp_lb,
         LBW1500to1999gNL_meta=LBW1500to1999gL_meta*wpp_lb,
         LBW1500to1999gN_meta=LBW1500to1999g_meta*wpp_lb,
         LBW1500to1999gNU_meta=LBW1500to1999gU_meta*wpp_lb,
         LBW2000to2499gNL_meta=LBW2000to2499gL_meta*wpp_lb,
         LBW2000to2499gN_meta=LBW2000to2499g_meta*wpp_lb,
         LBW2000to2499gNU_meta=LBW2000to2499gU_meta*wpp_lb)

transform<-figure %>% 
  dplyr::select(regionName, "LBW < 1000g"=LBWlt1000gN_meta,
                "LBW 1000 - 1499g"=LBW1000to1499gN_meta,
                "LBW 1500 - 1999g"=LBW1500to1999gN_meta,
                "LBW 2000 - 2499g"=LBW2000to2499gN_meta,
                estNU, estNL, estN=wpp_lb, est) %>% 
  pivot_longer(-c(regionName, estNU, estNL, estN, est), 
               names_to="Subgroups", 
               values_to = "values")
transform2<-figure %>% 
  dplyr::select(regionName, "LBW < 1000g"=LBWlt1000gNL_meta,
                "LBW 1000 - 1499g"=LBW1000to1499gNL_meta,
                "LBW 1500 - 1999g"=LBW1500to1999gNL_meta,
                "LBW 2000 - 2499g"=LBW2000to2499gNL_meta) %>% 
  pivot_longer(-c(regionName), 
               names_to="Subgroups", 
               values_to = "lower")
transform3<-figure %>% 
  dplyr::select(regionName, "LBW < 1000g"=LBWlt1000gNU_meta,
                "LBW 1000 - 1499g"=LBW1000to1499gNU_meta,
                "LBW 1500 - 1999g"=LBW1500to1999gNU_meta,
                "LBW 2000 - 2499g"=LBW2000to2499gNU_meta) %>% 
  pivot_longer(-c(regionName), 
               names_to="Subgroups", 
               values_to = "upper")

transformAll<-merge(x=merge(x=transform, y=transform2, 
                            by=c("regionName", "Subgroups"),
                            all.x=TRUE), y=transform3, by=c("regionName", "Subgroups"),all.x=TRUE)

transformAll %>% 
  filter(regionName=="Northern America, Australia and New Zealand, Central Asia and Europe") %>% 
  ggplot(aes(x=Subgroups, 
             y=values, fill=Subgroups))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#006a4e',"#00C301",'#BFEEB7', "#DDF0DA"), 
                    name="")+
  geom_errorbar(aes(ymin=lower, 
                    ymax=upper), alpha=0.5, width=0.2
  )+
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(x="", y="Number of LBW babies (thousands)")+
  scale_y_continuous(lim=c(0, 800000), 
                     labels = unit_format(unit="",scale = 1e-4))
