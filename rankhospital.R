rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
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
  
  ## Check that state and outcome are valid
  
  if(!state %in% statecheck) {
    stop("invalid state")
  }
  
  if(!outcome %in% outcomecheck) {
    stop("invalid outcome")
  }
  
  stateresults <- subset(outcometable, State==state)
  
  if(outcome == "heart attack") {
    stateresults <- stateresults[complete.cases(stateresults[,c(1:3,4)]),c(1:3,4)]
  }
  if(outcome == "heart failure") {
    stateresults <- stateresults[complete.cases(stateresults[,c(1:3,5)]),c(1:3,5)]
  }
  if(outcome == "pneumonia") {
    stateresults <- stateresults[complete.cases(stateresults[,c(1:3,6)]),c(1:3,6)]
  }
  
  if (class(num)=="character"){
    if(num == "best"){
      rval <- 1
    } else if(num == "worst"){
      rval <- nrow(stateresults)
    } else {
      rval <- NA
    }
  }
  
  if(class(num)=="numeric"){
    if(num > nrow(stateresults) | num < 1){
      return(NA)
    } else if(num > 0 && num < nrow(stateresults)) {
      rval <- num
    } else {
      rval <- NA
    }
  }
  
  if((class(num)=="numeric"|class(num)=="character")==FALSE){
    rval <- NA
  }
  
  ## Return hospital name in that state with the given rank
  orderindex <- with(stateresults, order(stateresults[,4],stateresults[,2]))
  return(stateresults[orderindex,][rval,2])
}