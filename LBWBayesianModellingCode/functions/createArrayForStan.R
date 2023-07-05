#function to create standata for model

covarMatrix <-function(covar,estyears,dataset){
  yearLength <- length(estyears)
  #numcoun<- length(unique(regionCodesAll2$countryIndex))
  numcoun <- length(unique(dataset$ISO))
  cMatrix<- matrix(ncol=yearLength,nrow=numcoun)
  for (i in 1:numcoun){
    for ( j in 1: yearLength){
      cMatrix[i,j] <- as.numeric(dataset[dataset$ISO == unique(dataset$ISO)[i] & dataset$year== estyears[j],covar])
    }
  }
  return (cMatrix)
}
##i=65
#j=1
#as.numeric(dataset[dataset$ISO == regionCodesAll2$ISO[i] & dataset$year== estyears[j],covar])

#covarMatrix("gniC",estyears = estyears,dataset = covarset)

standardize <- function(x){
  return((x-mean(x))/sd(x))
}

create_covar_array <- function(dataset, interest_cov,estyears){
  ncovar <- length(interest_cov)
  #numcoun <- length(unique(regionCodesAll2$countryIndex))
  numcoun <- length(unique(dataset$ISO))
  yearLength <- length(estyears)
  covararray <- array(NA, dim = c(ncovar,numcoun,yearLength))
  
  #for(i in 1:ncovar){
  #  covararray[i,,] <- standardize(covarMatrix(interest_cov[i],estyears = estyears,dataset = covarset))
  #}
  for(i in 1:ncovar){
    #covararray[i,,] <- covarMatrix(interest_cov[i],estyears = estyears,dataset = covarset)
    covararray[i,,] <- covarMatrix(interest_cov[i],estyears = estyears,dataset = dataset)
  }
  
  
  return(covararray)
}
