numHelper <- function(stateSubset, colNum, num) {
    outcomeArr <- as.numeric(stateSubset[, colNum])
    len <- dim(stateSubset[!is.na(outcomeArr), ])[1]
    if (num == "best") {
        rank <- rankHelper(stateSubset, outcomeArr, 1)
    } else if (num == "worst") {
        rank <- rankHelper(stateSubset, outcomeArr, len)
    } else if (num > len) {
        rank <- NA
    } else {
        rank <- rankHelper(stateSubset, outcomeArr, num)
    }
    result <- rank
    return(result)
}

rankHelper <- function(stateSubset, outcomeArr, num) {
    result <- stateSubset[, 2][order(outcomeArr, stateSubset[, 2])[num]]
    return(result)
}

rankall <- function(outcome, num = "best") {
   
    directory <- "outcome-of-care-measures.csv"
    data <- read.csv(directory, colClasses="character")
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    stateArr <- sort(unique(data$State))
    arrLen <- length(stateArr)
    hospital <- rep("", arrLen)
    
    if (!outcome %in% validOutcomes) {
        stop("invalid outcome")
    } else {
        for(i in 1:arrLen) {
            stateSubset <- data[data[, 7]==stateArr[i], ]
            if(outcome == "heart attack") {
                hospital[i] <- numHelper(stateSubset, 11, num) 
            } else if (outcome == "heart failure") {
                hospital[i] <- numHelper(stateSubset, 17, num) 
            } else {
                hospital[i] <- numHelper(stateSubset, 23, num) 
            }
        }
    }
   
    DF <- data.frame(hospital=hospital, state=stateArr)
    result <- DF
    return(result)
}