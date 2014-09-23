best <- function(state,outcome){
        ##Read outcome data
        file <- read.csv("outcome-of-care-measures.csv")
        ## Check that state and outcome are valid
        
        if (any(file[,7] == state)){
                if( outcome == "heart attack"){
                      toRank <- file[file$State == state,
                                     c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
                }
                else if (outcome == "heart failure"){
                        toRank <- file[file$State == state,
                                       c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
                }
                else if (outcome == "pneumonia"){
                        toRank <- file[file$State == state,
                                       c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
                }
                else return()
        }
        else
                return()
        ##Return hospital name in that state with the lowest 30-day death rate
        lowest <-  toRank[order(toRank$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.last=TRUE,decreasing=FALSE),]
        
        answer <- lowest[1,]
        return(answer)
}