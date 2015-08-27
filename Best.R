best <- function(state, outcome) {
  #data being made inside function
  bestdata <- data.frame
  bestdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  tolower(outcome)
  toupper(state)
  #Grab all state values and check against entry, throw error is needed.
  allstates <- unique(bestdata$State)
  #If statement for states
  if (is.na(match(state, allstates))) {
    stop("Invalid State")
  } else { message("State is valid")}
  
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
  
  #Subset of information based on state given
  statedata <- subset(bestdata, bestdata$State==state, select = c(2,7, MortalityColumn))
  na.omit(statedata)
  statedata[,3] <- as.numeric(statedata[,3])
  #Sort data by lowest 30-day death
  statedata <- statedata[order(statedata[,3], statedata[,1]), ]
  ## Return hospital name in that state with lowest 30-day death
  return(statedata[1,1])
  ## rate
}