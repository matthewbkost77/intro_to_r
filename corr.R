corr <- function(directory, threshold=0){
  # first get a data frame of stations with complete data sets
  completeStationsF <- complete(directory)
  passingStations <- completeStationsF[completeStationsF$nobs > threshold,]
  if (nrow(passingStations) == 0){
    return (vector("numeric",length=0))
  }
  
  # open up the corresponding files
  numPassingStations <- length(passingStations)
  stationDataCors <- vector(length=numPassingStations)
  stationFiles <- list.files(directory, full.names = TRUE)
  index <- 1
  for(i in passingStations$id){
    # need to ensure filename is in format NNN.csv
    stationFilePrefix <- as.character(i)
    if (i < 100)
      stationFilePrefix <- paste("0",stationFilePrefix, sep="")
    if (i < 10)
      stationFilePrefix <- paste("0",stationFilePrefix, sep="")
    
    stationFilename <- list.files(directory,
                                  pattern=stationFilePrefix,
                                  full.names=TRUE)
    
    nextStationsData <-read.csv(stationFilename)
    stationDataCors[index] <- cor(nextStationsData$nitrate,
                                nextStationsData$sulfate,
                                use="complete.obs")
    
    index <- index + 1
  }
  stationDataCors
}