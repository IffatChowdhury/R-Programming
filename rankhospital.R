rankHelper <- function(stateSubset, outcomeArr, num) {
    result <- stateSubset[, 2][order(outcomeArr, stateSubset[, 2])[num]]
    return(result)
}

rankhospital <- function(state, outcome, num = "best") {
    
    directory <- "outcome-of-care-measures.csv"
    data <- read.csv(directory, colClasses="character")
   
    data[, 11] <- as.numeric(data[, 11]) 
    data[, 17] <- as.numeric(data[, 17]) 
    data[, 23] <- as.numeric(data[, 23]) 
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
	
    if (!state %in% data$State) {
        stop("invalid state")
    } else if(!outcome %in% validOutcomes) {
        stop("invalid outcome")
    } else {
        if (num == "best") {
            rank <- best(state, outcome)
        } else {
            if(outcome == "heart attack") {
                
				stateSubset <- data[data[, 7]==state, ]
				outcomeArr <- stateSubset[, 11]
				len <- dim(stateSubset[!is.na(outcomeArr), ])[1]
				if (num == "worst") {
					rank <- rankHelper(stateSubset, outcomeArr, len)
				} else if (num > len) {
					rank <- NA
				} else {
					rank <- rankHelper(stateSubset, outcomeArr, num)
				}
				
            } else if(outcome == "heart failure") {
                
				stateSubset <- data[data[, 7]==state, ]
				outcomeArr <- stateSubset[, 17]
				len <- dim(stateSubset[!is.na(outcomeArr), ])[1]
				if (num == "worst") {
					rank <- rankHelper(stateSubset, outcomeArr, len)
				} else if (num > len) {
					rank <- NA
				} else {
					rank <- rankHelper(stateSubset, outcomeArr, num)
				}
				
            } else {
                
				stateSubset <- data[data[, 7]==state, ]
				outcomeArr <- stateSubset[, 23]
				len <- dim(stateSubset[!is.na(outcomeArr), ])[1]
				if (num == "worst") {
					rank <- rankHelper(stateSubset, outcomeArr, len)
				} else if (num > len) {
					rank <- NA
				} else {
					rank <- rankHelper(stateSubset, outcomeArr, num)
				}
            }
        }
        result <- rank
        return(result)
    }
}
