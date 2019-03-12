# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an outcome name. The 
# function reads the outcome-of-care-measures.csv le and returns a character vector with the name of the hospital that has the 
# best (i.e. lowest) 30-day mortality for the specied outcome in that state. The outcomes can be one of "heart attack", 
# "heart failure", or "pneumonia". Hospitals that do not have data on a particular outcome should be excluded from the set of 
# hospitals when deciding the rankings.

# The function should check the validity of its arguments. If an invalid state value is passed to best, the function should throw 
# an error via the stop function with the exact message "invalid state". If an invalid outcome value is passed to best, the 
# function should throw an error via the stop function with the exact message "invalid outcome".


best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  names(data)[c(11, 17, 23)] <- c("heart attack", "heart failure", "pneumonia")
  data$State <- as.factor(data$State)
  state_data <- data %>%
    filter(State == state)
  if (nrow(state_data) == 0) {
    stop('invalid state')
  }
  res <- tryCatch( {result <- as.data.table(state_data)[order(as.numeric(get(outcome)))][1, 2]; 
              print(result)}, error = function(x) "1")
  if (res == "1") {
    stop("invalid output")
  }
}
