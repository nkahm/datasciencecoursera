rankall <- function(outcome, num = "best"){
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings ="Not Available")
  
  ## Check that state and outcome are valid
  possibleOutcome = c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% possibleOutcome) {stop("invalid outcome")}
  
  
  ## determining column to be checked from outcome requested
  fullName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullName[match(outcome,possibleOutcome)]
  
  allStates = sort(unique(outcomeData[,7]))
  
  hospital <- character(0)
  
  for (i in seq_along(allStates)) {
    outcomeDataState <- outcomeData[outcomeData$State==allStates[i],]
    
    # sort the data and then return the num being asked
    outcomeDataStateSorted <- outcomeDataState[order(as.numeric(outcomeDataState[[colName]]),outcomeDataState[["Hospital.Name"]],decreasing=FALSE,na.last=NA),]
    
    # return according to num
    this.num = num
    if (this.num == "best") this.num = 1
    if (this.num == "worst") this.num = nrow(outcomeDataStateSorted)
    
    hospital[i] <- outcomeDataStateSorted[this.num,"Hospital.Name"]
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data.frame(hospital=hospital,state=allStates,row.names=allStates)
}