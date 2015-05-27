## Part 1: Plot the 30-day mortality rates for heart attack

partone <- function(){
# Read in CSV of outcome data into data frame.
outcome <- read.csv("outcome-of-care-measures.csv")

# Translate column 11 (30 Day mortality rates for heart attack) from 
# factor to numeric for plotting purposes. NAs exist and are acceptable here.
outcome[,11] <- as.numeric(outcome[,11])

# Plot histogram of the 30 day mortality rates for heart attack.
hist(outcome[,11], xlab="Mortality Rate", ylab="Frequency", 
     main="30-Day Mortality Rate for Heart Attack")
}

###############################################################################

## Part 2: Finding the best hospital in a state

#source("best.R")

###############################################################################

## Part 3: Ranking hospitals by outcome in a state

#source("rankhospital.R")

###############################################################################

## Part 4: Ranking hospitals by outcome in a state

#source("rankall.R")