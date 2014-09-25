rankall <- function(outcome,num="best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##get the hospital name, state, and outcome measurements
  data <- data[c(2, 7, 11, 17, 23)]
  ##name the columns for easy identification
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  ## check if outcome is one of the options measured
  outcomes = c("heart attack", "heart failure", "pneumonia")
  if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
  
  ## Check if num given is a number, best or worse
  if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
  
  ##Clean up the outcomes columns to have no NAs
  data <- data[data[outcome] != 'Not Available', ]
  
  ## chnage the data for the outcome to numeric values
  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  ##order the data by hospital name A to Z
  data <- data[order(data$name, decreasing = FALSE), ]
  ##order the data by outcome rate smallest to biggest
  data <- data[order(data[outcome], decreasing = FALSE), ]
  
  ##Function to get the hospital according to num for each state
  getHosp <- function(d, s, n) {
    d <- d[d$state==s, ]
    values <- d[, outcome]
    if( n == "best" ) {
      rowNum <- which.min(values)
    } else if( n == "worst" ) {
      rowNum <- which.max(values)
    } else {
      rowNum <- n
    }
    d[rowNum, ]$name
  }
  
  ## For each state, find the hospital which is according to num in the list
  ##get al the unique states into a vector
  states <- data[, 2]
  states <- unique(states)
  ##create an empty data frame to store the answer
  answer <- data.frame("hospital"=character(), "state"=character())
  
  ##for every state get the num state
  for(st in states) {
    hosp <- getHosp(data, st, num)
    answer <- rbind(answer, data.frame(hospital=hosp, state=st))
  }
  
  ## Return a data frame with the hospital names and the state name
  answer <- answer[order(answer['state'], decreasing = FALSE), ]
  ##order the unique states
  states <- states[order(states,decreasing=FALSE)]
  ##set the states as the row names
  row.names(answer) <- states
  answer
}