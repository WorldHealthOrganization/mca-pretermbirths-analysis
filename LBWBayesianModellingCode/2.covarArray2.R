
#covarset.raw <- read.dta("input/allCovariatesTidy.dta")

covarset.raw<-readRDS("input/LBWcovariate1995.RDS") %>% rename(iso=iso3) %>% 
  dplyr::select(iso, whoname, year, abr=abr_sm, 
                anc4=anc4_sm, sab=sab_sm, birth_healthfacility=birth_healthfacility_sm,
                csection=csection_sm, contraception_met=contraception_met_sm, 
                edu_mean_f=edu_mean_f_sm, literacy_fem=literacy_fem_sm, 
                bmi_f_over=bmi_f_over_sm,
                bmi_f_mean=bmi_f_mean_sm,
                bmi_f_under=bmi_f_under_sm, anaemia=anaemia_sm, 
                smoking=smoking_sm, gfr=gfr_sm, tfr=tfr_sm, hiv_wra=hiv_wra_sm, 
                underweight=underweight_sm, stunting=stunting_sm, 
                mmr=mmr_sm, nmr=nmr_sm, pfpr=pfpr_sm, ITN=ITN, 
                urban=urban_sm, gni=gni_sm)

int_cov <- c("gni","literacy_fem","contraception_met","bmi_f_under",
             "urban")

#int_cov <- c("gni","literacy_fem", "urban")
#Sensitivity 1
#int_cov <- c("gni","literacy_fem","contraception_met","bmi_f_under", 
#             "urban", "nmr")
#Sensitivity 2
#int_cov <- c("nmr","underweight");/.

covarset <- covarset.raw %>% dplyr::select(c("iso", "year", int_cov)) %>%  
  dplyr::rename(ISO=iso)

#write.csv(covarset, "output/covariates.csv")

index<-data.frame(ISO=unique(d$ISO),
                  countryIndex2=1:length(unique(d$ISO)))
covarset<-merge(x=covarset, y=index,by="ISO", all.y=TRUE)%>% 
  dplyr::select(-countryIndex2) %>% distinct()

transform<-covarset %>% mutate(gniT=log(gni),
                            literacy_femT=ifelse(literacy_fem>100, log(100-99.99999), log(100-literacy_fem)),
                            contraception_metT=log(1-contraception_met),
                            bmi_f_underT=log(bmi_f_under),
                            urbanT=urban
                            #,nmrT=nmr-mean(covarset$nmr, na.rm=TRUE)
                            #underweightT=underweight
)


#centring the covariates
covarsetCentred<-transform %>% mutate(gniC=gniT-mean(transform$gniT, na.rm = TRUE),
                                     literacy_femC=literacy_femT-mean(transform$literacy_femT, na.rm=TRUE),
                                    contraception_metC=contraception_metT-mean(transform$contraception_metT, na.rm=TRUE),
                                    bmi_f_underC=bmi_f_underT-mean(transform$bmi_f_underT, na.rm=TRUE),
                                     urbanC=urbanT-mean(transform$urbanT, na.rm=TRUE)
                                     #,nmrC=nmr-mean(transform$nmr, na.rm=TRUE)
                                     #underweightC=underweight-mean(transform$underweight, na.rm=TRUE)
)



int_cov <- paste0(int_cov, "C")

#regionCodesAll2<-readRDS("output/regionCodesAll2.RDS")


covar_array <- create_covar_array(interest_cov = int_cov,estyears = estyears, 
                                  dataset = covarsetCentred)




# #-----------TRANSFORMING VARIABLES
# hist(covarset$gni) #very right skewed
# hist(covarset$literacy_fem) #very left skewed
# hist(covarset$contraception_met) #slightly less skewed
# hist(covarset$bmi_f_under) #very right skewed
# hist(covarset$urban) #quite normal
# 
# #Transforming them as alex suggested!
# 
# hist(log(covarset$gni)) #almost normal
# hist(log(100-covarset$literacy_fem)) #now slightly left skewed
# hist(log(1-covarset$contraception_met)) #more normal
# hist(log(covarset$bmi_f_under)) #more normal
# hist(covarset$urban) #quite normal
# 
# #Centring the transformed
# hist(covarsetCentred$gniC) #very right skewed
# hist(covarsetCentred$literacy_femC) #very left skewed
# hist(covarsetCentred$contraception_metC) #slightly less skewed
# hist(covarsetCentred$bmi_f_underC) #very right skewed
# hist(covarsetCentred$urbanC) #quite normal
# 
# 
# 
# 
# 
# 
# 
