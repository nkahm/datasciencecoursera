best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings ="Not Available")
  
  ## Check that state and outcome are valid
  possibleOutcome = c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% possibleOutcome) {stop("invalid outcome")}
  
  possibleState = unique(outcomeData[,7])
  if (!state %in% possibleState) {stop("invalid state")}
  
  ## determining column to be checked from outcome requested
  fullName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullName[match(outcome,possibleOutcome)]
  
  ## Return hospital name in that state with lowest 30-day death
  outcomeDataState <- outcomeData[outcomeData$State==state,]
  id <- which.min(as.double(outcomeDataState[,colName]))
  outcomeDataState[id, "Hospital.Name"]
  ## rate
}