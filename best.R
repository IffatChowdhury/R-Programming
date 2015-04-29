
best <- function(state, outcome) {
   
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
        if(outcome == "heart attack") {
            #hosp_name <- helper(data, 11, state)
			
			stateSubset <- data[data[, 7]==state, ]
			outcomeArr <- stateSubset[, 11]
			min <- min(outcomeArr, na.rm=T)
			minIndex <- which(outcomeArr == min)
			hospitalName <- stateSubset[minIndex, 2]
			
        } else if(outcome == "heart failure") {
            #hospitalName <- helper(data, 17, state)
			
			stateSubset <- data[data[, 7]==state, ]
			outcomeArr <- stateSubset[, 17]
			min <- min(outcomeArr, na.rm=T)
			minIndex <- which(outcomeArr == min)
			hospitalName <- stateSubset[minIndex, 2]
			
        } else {
            #hospitalName <- helper(data, 23, state)
			
			stateSubset <- data[data[, 7]==state, ]
			outcomeArr <- stateSubset[, 23]
			min <- min(outcomeArr, na.rm=T)
			minIndex <- which(outcomeArr == min)
			hospitalName <- stateSubset[minIndex, 2]
        }
        result <- hospitalName
        return(result)
    }
}
