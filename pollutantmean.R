pollutantmean <- function(directory, pollutant, id = 1:332){
  stationFiles <- list.files(directory, full.names = TRUE)
  stationData <- vector("list", length=length(id))
  
  index <- 1
  for(i in id){
    stationData[[index]] <- read.csv(stationFiles[i])
    index <- index + 1
  }
  
  polluntData <- do.call(rbind, stationData)
  if(pollutant == "sulfate"){
    mean(polluntData$sulfate, na.rm=TRUE)
  }
  else if (pollutant == "nitrate"){
    mean(polluntData$nitrate, na.rm=TRUE)
  }
  else
    "Improper pollutant specified"
}