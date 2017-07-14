complete <- function(directory, id=1:332){
  # create an empty vector to hold the # of complete observations per file
  nobs <- vector(length=length(id))
  
  stationFiles <- list.files(directory, full.names = TRUE)
  index <- 1
  for(i in id){
    completeData <- complete.cases(read.csv(stationFiles[i]))
    nobs[index] <- sum(completeData)
    index <- index + 1
  }
  
  observationDataFrame <- data.frame( id, nobs )
  observationDataFrame
}