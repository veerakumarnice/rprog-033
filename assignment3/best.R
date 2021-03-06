## returns a character vector with the name of the hospital
## that has the best (i.e lowest) 30-day mortality for the 
## speified outcome in that state
best <- function(state, outcome) {
    ## Read the outcome data
    my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    hospital_names <- read.csv("hospital-data.csv")
    valid_outcome <- c("heart attack", "heart failure", "pneumonia")
    getcol <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    
    ## Check state and outcome are valid
    if(!(state %in% hospital_names$State)) {
        stop("invalid state")
    }
    if(!(outcome %in% valid_outcome) ) {
        stop("invalid outcome")
    }
    ## Return the hospital name in that state with 
    ## the lowest death 30 -day death rate
    colnumber <- getcol[[outcome]]
    my_data <- subset(my_data, my_data$State == state)
    my_data[, colnumber] <- as.numeric(my_data[, colnumber])
    my_data <- subset(my_data, !is.na(my_data[, colnumber]))
    minimum_value <- min(my_data[, colnumber])
    required <- subset(my_data, my_data[,colnumber] == minimum_value)
    sort(required$Hospital.Name)
} 