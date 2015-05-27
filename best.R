best <- function(state, outcome) {
  
  # Read outcome data in and coerce the mortality rates to a numeric data type.
  outcometable <- read.csv("outcome-of-care-measures.csv", 
                            na.strings="Not Available",
                            stringsAsFactors=FALSE)
  
  outcomecheck <- c("heart attack", "heart failure", "pneumonia") # Outcomes
  statecheck <- as.character(unique(outcometable$State)) # States
  statecheck <- statecheck[order(statecheck)] # States in A-Z order
  
  outcometable[,11] <- as.numeric(outcometable[,11]) # 30 day heart atck drate
  outcometable[,17] <- as.numeric(outcometable[,17]) # 30 day heart fail drate
  outcometable[,23] <- as.numeric(outcometable[,23]) # 30 day pneumonia drate
  outcometable <- outcometable[,c(1:2,7,11,17,23)] # Drop all unnecessary info
  names(outcometable) <- c("Provider.Number","Hospital.Name","State",
                           "Heart.Attack","Heart.Failure","Pneumonia")
  
  # Check that state and outcome are valid
  if (!state %in% statecheck) {
    stop("invalid state")
  }
  if (!outcome %in% outcomecheck) {
    stop("invalid outcome")
  }
  
  # Return hospital name in that state with lowest 30-day death rate
  stateresults <- subset(outcometable, State==state)
  # build dataframe of just the interesting stuff & remove NAs
  if(outcome == "heart attack") {
    stateresults <- stateresults[complete.cases(stateresults[,c(1:3,4)]),c(1:3,4)]
  }
  if(outcome == "heart failure") {
    stateresults <- stateresults[complete.cases(stateresults[,c(1:3,5)]),c(1:3,5)]
  }
  if(outcome == "pneumonia") {
    stateresults <- stateresults[complete.cases(stateresults[,c(1:3,6)]),c(1:3,6)]
  }
  
  # order
  orderindex <- with(stateresults, order(stateresults[,4],stateresults[,2]))
  return(stateresults[orderindex,][1,2])
}