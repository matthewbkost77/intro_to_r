rankall <- function(outcome, num = "best") {
  # determine the column index from the csv for the given outcome
  index <- 0
  if (outcome == "heart attack")
    index <- 11
  else if (outcome == "heart failure")
    index <- 17
  else if (outcome == "pneumonia")
    index <- 23
  else
    stop("invalid outcome")
  
  ## Read outcome data
  ## index 2 = Hospital.Name
  ## index 7 = State
  caremeasures <- read.csv("outcome-of-care-measures.csv" )
  
  # filter out NAs returning just the Hospital name, State, and outcome
  hospitalData <- caremeasures[caremeasures[, index] !=  "Not Available",
                                    c(2, 7, index)]
  
  # convert rating strings to numbers
  hospitalData[, 3] <-
    #  as.numeric(levels(hospitalData[, 2])) [hospitalData[, 2]]
    as.numeric(as.character(hospitalData[, 3]))
  
  
  
  # sort the rankings by state
  #sortedData <- tapply(hospitalData[,3], # outcome
  #                     hospitalData[,2], # State
  #                     order)

  # loop through each state pulling the requested rank
  stateNames <- levels(hospitalData[,2])
  numStates <- length(stateNames)
  nthHospitalNames <- vector(length = numStates) 
  for(i in seq_along(stateNames)){
    stateData <- hospitalData[hospitalData[,2] == stateNames[i],]
    stateOrder <- order(stateData[,3], # order first by outcome
                        stateData[,1], # then by hospital name
                        na.last = TRUE)

    orderedStateData <- stateData[stateOrder,]

    # convert (if necessary) num to an actual number based on current state
    ranking <- -1
    if (num == "best")
      ranking <- 1
    
    else if (num == "worst")
      ranking <- nrow(orderedStateData)
    
    else
      ranking <- as.numeric(num)

    if (ranking > nrow(orderedStateData))
      nthHospitalNames[i] <- NA
    else 
      nthHospitalNames[i] <- as.vector(orderedStateData[[ranking,1]])
      
  } # end for in stateNames
  
  ## Return a data frame with the hospital and state names
  return (data.frame(hospital=nthHospitalNames, state=stateNames))
}
