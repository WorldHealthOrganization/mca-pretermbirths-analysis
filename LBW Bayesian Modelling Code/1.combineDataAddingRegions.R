##Adding admin and survey together, with the heaping index
adminAll<-adminAll %>% rename(iso=IsoCode)

allSourcesB<-bind_rows(adminAll, surveyAll) %>% rename(ISO=iso)
allSourcesRemoved<-allSourcesB %>% filter(Year<2000)
allSources<-allSourcesB %>% filter((Source=="Admin"  & Year>=2000) | (Source=="Survey" & Year>=1995))

#Dropping Kosovo
allSources2<-allSources %>% filter(ISO!="XKX")

#-----
#Adding in region codes
regionCodes=read.csv(paste0("input/",regionsName))%>% 
  dplyr::select(ISO3Code,SDGRegionrev1, sdg.rev1, OfficialName) %>% rename("regionIndex"="sdg.rev1",
                                                      "ISO"="ISO3Code", "regionName"="SDGRegionrev1")
saveRDS(regionCodes, "output/regionCodes.RDS")
allDataLBW<-merge(x=allSources2, y=regionCodes, by="ISO", all.x=TRUE)

regionCodes2=read.csv(paste0("input/wbregions.csv")) %>% rename("ISO"="iso3")
