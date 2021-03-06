best <- function(state,outcome){
  
  ##Read outcome data
  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that the outcome entered is valid
  outcomes = c("heart attack","heart failure","pneumonia")
  if (outcome %in% outcomes == FALSE) {
    stop("invalid outcome")
  }
  
  ##checking if entered state exists
  states <- file[,7]
  states <- unique(states)
  if (state %in% states == FALSE) {
    stop("invalid state")
  }
  
  ##extracting only needed columns from the file 
  data <- file[c(2,7,11,17,23)]
  
  ##setting vectors names
  names(data)[1] = "Hospital.Name"
  names(data)[2] = "State"
  names(data)[3] = "heart attack"
  names(data)[4] = "heart failure"
  names(data)[5] = "pneumonia" 
    
  ##data for specific state excluding the not available measurements for outcome
  data <- data[data$State==state & data[outcome] != 'Not Available', ]
  vals <- data[, outcome]
  ##getting row number of the hospital with the lowet mortaility 
  ##rate for the outcome
  rowNum <- which.min(vals)
  
  ##Return hospital name in that state with the lowest 30-day death rate
  return(data[rowNum,]$Hospital.Name)
}