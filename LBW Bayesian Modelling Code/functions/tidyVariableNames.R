tidyVariableNames<-function(data){
  
names(data) <- gsub(" ", "", names(data))
names(data) <- gsub("<", "lt", names(data))
names(data) <- gsub("[()]", "",names(data))
names(data) <- gsub("live_birth", "LB",names(data))
names(data) <- gsub("livebirth", "LB",names(data))
names(data) <- gsub("\\...", "",names(data))
names(data) <- gsub("-", "to",names(data))
names(data) <- gsub("%", "perc",names(data))

return(data)
}
