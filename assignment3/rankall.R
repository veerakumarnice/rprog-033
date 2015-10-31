## returns a character vector with the name of the hospital
## that has the best (i.e lowest) 30-day mortality for the 
## speified outcome in that state
rankall <- function(outcome, num = "best") {
    ## Read the outcome data
    my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    hospital_names <- read.csv("hospital-data.csv")
    valid_outcome <- c("heart attack", "heart failure", "pneumonia")
    getcol <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    
    ## Check state and outcome are valid
    
    if(!(outcome %in% valid_outcome) ) {
        stop("invalid outcome")
    }
    ## Return the hospital name in that state with 
    ## the lowest death 30 -day death rate
    colnumber <- getcol[[outcome]]
    my_data[, colnumber] <- as.numeric(my_data[, colnumber])
    my_data <- subset(my_data, !is.na(my_data[, colnumber]))
    spare <- my_data
    rankedframe <- data.frame(hospital = character(),state = character())
    state_names <- unique(hospital_names$State)
    state_names <- sort(state_names)
    for (name in state_names) {
       ## print("enetered loop")
        my_data <- subset(spare, spare$State == name)
    
        required <- my_data[order(my_data[,colnumber], my_data$Hospital.Name),]
    
        ## setting the ronumber for the result
        get <- if(num == "best") {
            1
        }
        else if (num == "worst"){
            nrow(required)
        }
        else if(num > nrow(my_data)) {
            NA
        }
        else if (num >0) {
            num    
        }
        else
            NA
        
        if(!is.na(get) && get!= 0) {
            val <- required$Hospital.Name[get]
            newframe <- data.frame(hospital = val, state = name)
     ##       print(newframe$hospital, newframe$state)
        }
        else {
            newframe <- data.frame(hospital = NA, state = name)
        }
        rankedframe <- rbind(rankedframe , newframe)
    }
    rankedframe
} 