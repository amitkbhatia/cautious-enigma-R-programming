rank <- function(state, outcome, num)
    {
## Read outcome data
## outcome_name: "heart attack", "heart failure", "pneumonia"     
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death rate
        
#read in the desired data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings= "Not Available")
        
#check wheather the state and outcomes are valid
        states <- data[ , 7]
        outcomes_names <- c("heart attack", "heart failure", "pneumonia")
        if (!(state %in% states) == TRUE) {
                stop(print("invalid state"))
        }
        else if (!(outcome %in% outcomes_names) == TRUE) {
                stop(print("invalid outcome"))
        }
       
        #get the subset of the data with the desired state
        subset_data <- subset(data, State == state)
        
        #get the desired outcome column from the data file
        names(subset_data)[c(11,17,23)] <- outcomes_names       
        
        #get rid of the NA's in the desired outcome column
        required_columns <- as.numeric(subset_data[,outcome])
        desired_data <- subset_data[!is.na(required_columns), ]
        
        #find the hospitals in the rows with the rank outcome value
        columns_considered <- as.numeric(desired_data[, outcome])
        
        if (num == "best") {
                desired_rows <- as.numeric(1)
        }
        else if (num == "worst") {
                desired_rows <- as.numeric(nrow(desired_data))
        }
        else {
                desired_rows = as.numeric(num)
        }
        sorted_data <- desired_data[order(columns_considered),]
        desired_hospitals <- sorted_data[desired_rows, 2]
        desired_hospitals[1]
}
