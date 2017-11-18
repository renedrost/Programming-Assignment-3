rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
    
    ## Create outcome to column convertion
    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    
    ## Check correct outcome-input
    if (is.na(outcomes[outcome])) {
        stop("invalid outcome")
    }

    ## Remove all NA's
    data <- data[!is.na(data[,outcomes[outcome]]),]

    ## Convert to numeric, so we can sort the numbers correctly
    data[,outcomes[outcome]] <- as.numeric(data[,outcomes[outcome]])

    ## Sort data on state, outcome, hospitale name
    sortedData <- data[order(data$State, data[outcomes[outcome]], data$Hospital.Name),]

    ## Splitting data by State
    statesData <- split(sortedData, sortedData$State)

    ret <- do.call(rbind, lapply(statesData, 
                      function(x) {
                          ## Which row do we have to return
                          if (num == "best") {
                              num <- 1
                          }
                          if (num == "worst") {
                              num <- nrow(x)
                          }
                          
                          ## Create named vector with Hospitalname and State
                          c(hospital = x[num, "Hospital.Name"], state = x[1, "State"])
                      }
                  ))

    ## Return data.frame with results
    data.frame(ret)
}