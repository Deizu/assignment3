## Examples provided for Programming Assignment 3.
runexamples <- function(){
  
# Part 1 (NA)
source("workingspace.R")
partone()
  message("Graph produced!")

# Part 2
source("best.R")

print(best("TX", "heart attack"))
  message("Should have returned CYPRESS FAIRBANKS MEDICAL CENTER.")

print(best("TX", "heart failure"))
  message("Should have returned FORT DUNCAN MEDICAL CENTER.")

print(best("MD", "heart attack"))
  message("Should have returned JOHNS HOPKINS HOSPITAL, THE.")

print(best("MD", "pneumonia"))
  message("Should have returned GREATER BALTIMORE MEDICAL CENTER.")

message("Skipping tests which cause errors - try them individually!")
#print(best("BB", "heart attack"))
#  message("Should have returned an invalid state error")

#print(best("NY", "hert attack"))
#  message("Should have returned an invalid outcome error.")

# Part 3
source("rankhospital.R")

print(rankhospital("TX", "heart failure", 4))
  message("Should have returned DETAR HOSPITAL NAVARRO.")

print(rankhospital("MD", "heart attack", "worst"))
  message("Should have returned HARFORD MEMORIAL HOSPITAL.")

print(rankhospital("MN", "heart attack", 5000))
  message("Should have returned NA.")

# Part 4
source("rankall.R")

print(head(rankall("heart attack", 20), 10))
  message("Should have returned the following data frame:
                              hospital state
1                                 <NA>    AK
2       D W MCMILLAN MEMORIAL HOSPITAL    AL
3    ARKANSAS METHODIST MEDICAL CENTER    AR
4  JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
5                SHERMAN OAKS HOSPITAL    CA
6             SKY RIDGE MEDICAL CENTER    CO
7              MIDSTATE MEDICAL CENTER    CT
8                                 <NA>    DC
9                                 <NA>    DE
10      SOUTH FLORIDA BAPTIST HOSPITAL    FL")

print(tail(rankall("pneumonia", "worst"), 3))
  message("Should have returned the following data frame:
                                     hospital state
52 MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
53                     PLATEAU MEDICAL CENTER    WV
54           NORTH BIG HORN HOSPITAL DISTRICT    WY")

print(tail(rankall("heart failure"), 10))
  message("Should have returned the following data frame:
                                                              hospital state
45                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
46                                        FORT DUNCAN MEDICAL CENTER    TX
47 VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
48                                          SENTARA POTOMAC HOSPITAL    VA
49                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
50                                              SPRINGFIELD HOSPITAL    VT
51                                         HARBORVIEW MEDICAL CENTER    WA
52                                    AURORA ST LUKES MEDICAL CENTER    WI
53                                         FAIRMONT GENERAL HOSPITAL    WV
54                                        CHEYENNE VA MEDICAL CENTER    WY")

}