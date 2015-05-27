best <- function(state, outcome) {
  
  # Read outcome data in and coerce the mortality rates to a numeric data type.
  outcometable <- read.csv("outcome-of-care-measures.csv", 
                            na.strings="Not Available",
                            stringsAsFactors=FALSE)
  outcometable[,11] <- as.numeric(outcometable[,11]) # 30 day heart atck drate
  outcometable[,17] <- as.numeric(outcometable[,17]) # 30 day heart fail drate
  outcometable[,23] <- as.numeric(outcometable[,23]) # 30 day pneumonia drate
  outcometable <- outcometable[,c(1:2,7,11,17,23)] # Drop all unnecessary info
  names(outcometable) <- c("Provider.Number","Hospital.Name","State",
                           "Heart.Attack","Heart.Failure","Pneumonia")
  
  outcomecheck <- c("heart attack", "heart failure", "pneumonia")
    #message("Set up outcome check.")
  
  # Pull in hospital data and create a vector of unique state codes.
  hospitaldata <- read.csv("hospital-data.csv")
    #message("Pulled hospital info in.")
  hospitalstate <- as.character(unique(hospitaldata$State))
    #message("Grabbed all unique states for hospitals.")
  
  # Check that state and outcome are valid
  if (!state %in% hospitalstate) {
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