## The function reads the outcome-of-care-measures.csv
## file then returns a character vector with the name of the state hospital
## that has the best (i.e. lowest) 30-day mortality for the specied outcome
best <- function(statename, outcome) {
  ## Check that state and outcome are valid before doing anything
  data(state)
  if( !(statename %in% state.abb)){
    stop("invalid state")
  }

  index <- 0    # index in outcome csv data
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
  stateHosptialData <- caremeasures[caremeasures$State == statename &
                                      caremeasures[, index] !=  "Not Available",
                                    c(2, index)]
  
  # convert strings to numbers
  stateHosptialData[, 2] <-
  #  as.numeric(levels(stateHosptialData[, 2])) [stateHosptialData[, 2]]
    as.numeric(as.character(stateHosptialData[, 2]))
  
  # finally sort by desired outcome then hospital name
  sortOrder <- order(stateHosptialData[, 2],
                     stateHosptialData[, 1],
                     na.last = TRUE)
  rankedHospitalNames <- stateHosptialData[sortOrder,  ]
  
  ## Return hospital name in that state with lowest 30-day death rate
  return (as.vector(rankedHospitalNames[[1,1]]))
}