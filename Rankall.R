## rankall.R
     rankall <- function(outcome, num = "best") {
## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings= "Not Available")
  
## Check that outcome is valid
    outcome.names <- c("heart attack","heart failure","pneumonia")
if (!outcome %in% outcome.names) { stop("invalid outcome")}
  
names(data)[c(11,17,23)] <- outcome.names
  
# take just the columns we need.
  data <- data[,c("State","Hospital.Name",outcome)]

# convert outcome column to numeric and get rid of the NA's in the desired outcome column
     required_columns <- as.numeric(data[,outcome])
     desired_data <- data[!is.na(required_columns),]

# sort data by state name, then  by outcome, then by hospital name
  final_data <- desired_data[order(data$State, data[outcome], data$Hospital.Name),]

# aggregate by state, choosing the row that corresponds to the rank num
  ranksbystate <- aggregate(final_data, by=list(final_data$State), function(x) {
      if (!is.numeric(num)) {
        if (num == "best") {
          num <- 1
        } else if (num == "worst") {
          num <- length(x)
        } else {
          stop("invalid num")
        } 
      }
      x[num]
    })
# get just the columns we need and rename them
  out <- ranksbystate[,c(3,1)]
  names(out) <- c("hospital","state")
  return(out)
}