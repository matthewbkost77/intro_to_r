rankhospital <- function(statename, outcome, num = "best") {
  ## Check that state and outcome are valid before doing anything
  data(state)
  if( !(statename %in% state.abb)){
    stop("invalid state")
  }
  
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
  caremeasures <- read.csv("outcome-of-care-measures.csv" )
  
  # filter out only the state of interest and remove NAs
  stateHospitalData <- caremeasures[caremeasures$State == statename &
                                      caremeasures[, index] !=  "Not Available",
                                    c(2, index)]
  
  # convert rating strings to numbers
  stateHospitalData[, 2] <-
    #  as.numeric(levels(stateHospitalData[, 2])) [stateHospitalData[, 2]]
    as.numeric(as.character(stateHospitalData[, 2]))
  
  # finally sort by desired outcome then hospital name
  sortOrder <- order(stateHospitalData[, 2],
                     stateHospitalData[, 1],
                     na.last = TRUE)
  rankedHospitalNames <- stateHospitalData[sortOrder,  ]
  
  ## Return hospital name in that state with the num-th 30-day death rate
  # convert (if necessary) num to an actual number
  ranking <- -1
  if (num == "best")
    ranking <- 1
  
  else if (num == "worst")
    ranking <- nrow(rankedHospitalNames)
  
  else
    ranking <- as.numeric(num)

  # see if we were asked to return an index greater than the # of hospitals
  if (ranking > nrow(rankedHospitalNames))
    return (NA)
  
  return (as.vector(rankedHospitalNames[[ranking,1]]))
}