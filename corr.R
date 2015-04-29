corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations

    correlation <- function(a) {
        dataset <- read.csv(file.path(directory, a))
        nobs <- sum(complete.cases(dataset))
        if (nobs > threshold) {
            return (cor(dataset$nitrate, dataset$sulfate, use="complete.obs"))
        }
    }
    correlations <- sapply(list.files(directory), correlation) 
    correlations <- unlist(correlations[!sapply(correlations, is.null)])
    return (correlations)
}