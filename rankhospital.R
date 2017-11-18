rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
    
    ## Create outcome to column convertion
    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    
    ## Check correct state-input
    if (!state %in% data$State) {
        stop("invalid state")
    }
    
    ## Check correct outcome-input
    if (is.na(outcomes[outcome])) {
        stop("invalid outcome")
    }
    
    ## Subset data by State
    statesData <- data[data$State == state,]
    
    ## Remove NA's
    statesData <- statesData[!is.na(statesData[,outcomes[outcome]]),]
    ## Convert to numeric, so we can sort the numbers correctly
    statesData[,outcomes[outcome]] <- as.numeric(statesData[,outcomes[outcome]])
    
    ## Sort data by outcome
    statesData <- statesData[order(statesData[outcomes[outcome]], statesData$Hospital.Name),]
    
    ## Return first Hospital
    if (num == "best") {
        num <- 1
    }
    if (num == "worst") {
        num <- nrow(statesData)
    }
    statesData[num,"Hospital.Name"]
}