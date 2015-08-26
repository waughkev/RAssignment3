best <- function(state, outcome) {
  #data being made inside function
  bestdata <- data.frame
  bestdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #Subset of information based on state given
  statedata <- subset(bestdata, bestdata$State==state)
  ## Read outcome data
  
  #Set column to read from based on outcome
  if (outcome=="Heart attack") {
    MortalityColumn <- statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  } else if (outcome=="Heart Failure") {
    MortalityColumn <- statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  } else if (outcome=="Pneumonia") {
    MortalityColumn <- statedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  } else {
    #Throw error
    stop("invalid state")
  }
  #Grab all state values and check against entry, throw error is needed.
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
