rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  bestdata <- data.frame
  bestdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  tolower(outcome)
  toupper(state)
  
  #Set column to read from based on outcome
  if (outcome=="heart attack") {
    MortalityColumn <- 11
  } else if (outcome=="heart failure") {
    MortalityColumn <- 17
  } else if (outcome=="pneumonia") {
    MortalityColumn <- 23
  } else {
    #Throw error
    stop("Invalid Disease")
  }
  #Grab all state values and check against entry, throw error is needed.
  allstates <- unique(bestdata$State)
  #If statement for states
  if (is.na(match(state, allstates))) {
    stop("Invalid State")
  } else { message("State is valid")}
  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  statedata <- subset(bestdata, bestdata$State=="TX", select = c(2,7, MortalityColumn))
  na.omit(statedata)
  statedata[,3] <- as.numeric(statedata[,3])
  #Sort data by lowest 30-day death
  statedata <- statedata[order(statedata[,3], statedata[,1]), ]
  
  #num must be a number, so if num is best or worst, that will need to be changed to 1 for best, and nrow for worst
  if (num=="best") {
    rownumber <- 1
  } else if (num=="worst") {
    rownumber <- nrow(statedata)
  } else {
    rownumber <- num
  }
  ## 30-day death rate
  return(statedata[rownumber,1])
}
