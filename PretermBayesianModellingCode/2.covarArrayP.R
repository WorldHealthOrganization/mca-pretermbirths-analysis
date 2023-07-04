#Adding the latest LBW estimates as a covariate

Rfiles <- list.files(file.path(paste0(getwd(),"/functions/")), ".R") #gets names of r files
sapply(paste0(paste0(getwd(),"/functions/"), Rfiles), source) #gets functions from file

#Using the LBW estimates and SE so the model can sample from the distribution of the estimate
# rather than using just the point estimate
int_cov <- c("logLBW", "logLBWSe")

covarset<-read.csv(lbwEstimates) %>% 
  dplyr::select(ISO, year, LBW_2020_estimate=est, LBW_lower=estL) %>% 
  mutate(logLBW=log(LBW_2020_estimate),
         logLBWSe=(log(LBW_2020_estimate)-log(LBW_lower))/1.96)



index<-data.frame(ISO=unique(finalPreterm$ISO), 
                  countryIndex2=1:length(unique(finalPreterm$ISO)))
covarset<-merge(x=covarset, y=index,by="ISO", all.y=TRUE)%>% 
  dplyr::select(-countryIndex2) %>% distinct()

#Getting the covariates in the right format for JAGs
covar_array <- create_covar_array(interest_cov = int_cov,estyears = estyears, 
                                  dataset = covarset)

