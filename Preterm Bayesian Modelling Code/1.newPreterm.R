# library(dplyr)
# library(ggplot2)
# library(gridExtra)

newPreterm<-readxl::read_excel("inputs/FILLED IN POST COUNTRY CONSULTATION 05.12.2022 pretermDatabase example.xlsx")
newPreterm2<-tidyVariableNames(newPreterm) %>% 
  mutate(isoYear=paste0(IsoCode, Year),
         database="new") %>% 
  rename(ISO=IsoCode,
         Numberoftotalbirths22=Numberoftotalbirths,
         NumberofallLBs23=NumberofallLBs) %>% 
  filter(!is.na(ISO))

# pretermPreIncExc<-readRDS("PRETERM/output/pretermPreIncExc.rds")
# oldPreterm<-pretermPreIncExc %>% filter(isoYear %in% newPreterm2$isoYear &
#                                           Source=="Admin") %>% 
#   mutate(database="old",
#          Preterm32tolt37weeks=as.numeric(Preterm32tolt37weeks),
#          Preterm28tolt32weeks=as.numeric(Preterm28tolt32weeks),
#          Pretermlt28weeks=as.numeric(Pretermlt28weeks),
#          MissingGA=as.numeric(MissingGA),
#          SourcetypeforLBpreterm=as.numeric(SourcetypeforLBpreterm))
# 
# together<-bind_rows(newPreterm2, oldPreterm) %>% 
#   mutate(group_no = as.integer(factor(ISO))) %>% 
#   mutate(pretermRate=ifelse(!is.na(NumberofbabieswithGA), (Pretermlt37weeks/NumberofbabieswithGA)*100, 
#                             ifelse(!is.na(NumberofallLBs23), (Pretermlt37weeks/NumberofallLBs23)*100, 
#                                    (Pretermlt37weeks/Numberoftotalbirths22)*100)))
# 

checking<-function(i){
  
plot1<-ggplot(together %>% filter(group_no==i)) + 
  geom_point(aes(x="Total", y=Numberoftotalbirths22, 
                  colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  geom_point(aes(x="Livebirths", y=NumberofallLBs23, 
                  colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  geom_point(aes(x="with GA", y=NumberofbabieswithGA, 
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  theme(legend.position = "bottom")+
  xlab("")+
  labs(title=(together %>% filter(group_no==i) %>% 
                dplyr::select(OfficialName) %>% distinct() %>% 
                filter(!is.na(OfficialName)))$OfficialName)

plot21<-ggplot(together %>% filter(group_no==i)) + 
  geom_point(aes(x="Preterm<37\nweeks", y=Pretermlt37weeks, 
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.9))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=45, hjust=1))+
  xlab("")

plot22<-ggplot(together %>% filter(group_no==i)) +
  geom_point(aes(x="Preterm28-<32\nweeks", y=Preterm28tolt32weeks, 
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  geom_point(aes(x="Preterm24-<28\nweeks", y=Preterm24tolt28weeks, 
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  geom_point(aes(x="Preterm22-<24\nweeks", y=Preterm22tolt24weeks, 
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  geom_point(aes(x="Preterm<22\nweeks", y=Pretermlt22weeks, 
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  geom_point(aes(x="Preterm<25\nweeks", y=Pretermlt25weeks, 
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  geom_point(aes(x="Preterm<28\nweeks", y=Pretermlt28weeks, 
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  geom_point(aes(x="Preterm<32\nweeks", y=Pretermlt32weeks, 
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=45, hjust=1))+
  xlab("")



plot3<-ggplot(together %>% filter(group_no==i)) + 
  geom_point(aes(x="Sourcetype", y=SourcetypeforLBpreterm, 
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  geom_point(aes(x="MethodofGA", y=Methodofgestationalageassessment,
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  geom_point(aes(x="Sing/Mult", y=Allpretermbirths,
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  geom_point(aes(x="GAlowerthreshold", y=Gestationalagelowerthreshold,
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=45, hjust=1))+
  xlab("")+
  labs(title=(together %>% filter(group_no==i) %>% 
                dplyr::select(OfficialName) %>% distinct() %>% 
                filter(!is.na(OfficialName)))$OfficialName)

plot4<-ggplot(together %>% filter(group_no==i)) + 
 geom_point(aes(x="Missing GA", y=MissingGA, 
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  theme(legend.position = "bottom")+
  xlab("")+
  labs(title=(together %>% filter(group_no==i) %>% 
               dplyr::select(OfficialName) %>% distinct() %>% 
                filter(!is.na(OfficialName)))$OfficialName)

plot5<-ggplot(together %>% filter(group_no==i))+
  geom_point(aes(x="Preterm Rate", y=pretermRate, 
                 colour=factor(Year),shape=database),
             position = position_dodge(width = 0.5))+
  theme(legend.position = "bottom")+
  xlab("")+
  labs(title=(together %>% filter(group_no==i) %>% 
                dplyr::select(OfficialName) %>% distinct() %>% 
                filter(!is.na(OfficialName)))$OfficialName)

# return(grid.arrange(plot1, grid.arrange(plot21, plot22, nrow=1), 
#              plot3, plot4, top=(together %>% filter(group_no==i) %>% 
#                                     dplyr::select(OfficialName) %>% distinct() %>% 
#                                     filter(!is.na(OfficialName)))$OfficialName))

grid.arrange(plot1)
grid.arrange(plot21, plot22, nrow=1,
               top=(together %>% filter(group_no==i) %>% 
                             dplyr::select(OfficialName) %>% distinct() %>% 
                             filter(!is.na(OfficialName)))$OfficialName)
grid.arrange(plot3)
grid.arrange(plot4)
grid.arrange(plot5)


}

# pdf_name <- paste0("PRETERM/output/checking new preterm 3.pdf")
# pdf(pdf_name, width = 10, height = 5)
# unique(together$group_no) %>% lapply(checking)
# dev.off()

