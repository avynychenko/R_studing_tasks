# Task 1

# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an outcome name. The 
# function reads the outcome-of-care-measures.csv le and returns a character vector with the name of the hospital that has the 
# best (i.e. lowest) 30-day mortality for the specied outcome in that state. The outcomes can be one of "heart attack", 
# "heart failure", or "pneumonia". Hospitals that do not have data on a particular outcome should be excluded from the set of 
# hospitals when deciding the rankings.

# The function should check the validity of its arguments. If an invalid state value is passed to best, the function should throw 
# an error via the stop function with the exact message "invalid state". If an invalid outcome value is passed to best, the 
# function should throw an error via the stop function with the exact message "invalid outcome".

library(data.table)
library(dplyr)

best <- function(state, outcome) {
  data <- read.csv("https://raw.githubusercontent.com/avynychenko/R_studing_tasks/master/Data%20for%20tasks/Coursera%20Week%204%20debugging/outcome-of-care-measures.csv", 
  colClasses = "character")
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
    stop("invalid outcome")
  }
}

# Task 2

# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a state (state), an outcome 
# (outcome), and the ranking of a hospital in that state for that outcome (num). The function reads the outcome-of-care-measures.csv 
# file and returns a character vector with the name of the hospital that has the ranking specied by the num argument. The num 
# argument can take values \best", \worst", or an integer indicating the ranking (smaller numbers are better). If the number given 
# by num is larger than the number of hospitals in that state, then the function should return NA.

# The function should check the validity of its arguments. If an invalid state value is passed to rankhospital, the function should 
# throw an error via the stop function with the exact message \invalid state". If an invalid outcome value is passed to rankhospital,
#  the function should throw an error via the stop function with the exact message \invalid outcome".

library(data.table)
library(dplyr)

rankhospital <- function(state, outcome, num) {
  data <- read.csv("https://raw.githubusercontent.com/avynychenko/R_studing_tasks/master/Data%20for%20tasks/Coursera%20Week%204%20debugging/outcome-of-care-measures.csv", 
  colClasses = "character")
  names(data)[c(11, 17, 23)] <- c("heart attack", "heart failure", "pneumonia")
  data$State <- as.factor(data$State)
  state_data <- data %>%
    filter(State == state)
  if (nrow(state_data) == 0) {
    stop('invalid state')
  }
  res <- tryCatch( {as.data.table(state_data)[!is.na(as.numeric(get(outcome)))][order(as.numeric(get(outcome)), Hospital.Name)]}, error = function(x) "1")
  if (res == "1") {
    stop("invalid outcome")
  } else {
    sorted_data <- as.data.table(state_data)[!is.na(as.numeric(get(outcome)))][order(as.numeric(get(outcome)), Hospital.Name)]
    if (num == "best") {
      print(sorted_data[1, 2])
    } else if (num == "worst") {
      print(sorted_data[nrow(sorted_data), 2])
    } else if (num > nrow(sorted_data)) {
      print(NA)
    } else {
      print(sorted_data[num, 2])
    }
  } 
}

# Task 3

# Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num). The function 
# reads the outcome-of-care-measures.csv le and returns a 2-column data frame containing the hospital in each state that has the 
# ranking specied in num. The function should return a value for every state (some may be NA). The rst column in the data frame is 
# named hospital, which contains the hospital name, and the second column is named state, which contains the 2-character abbreviation 
# for the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when 
# deciding the rankings.

# The function should check the validity of its arguments. If an invalid outcome value is passed to rankall, the function should 
# throw an error via the stop function with the exact message "invalid outcome". The num variable can take values "best", "worst", 
# or an integer indicating the ranking (smaller numbers are better). If the number given by num is larger than the number of 
# hospitals in that state, then the function should return NA.

library(data.table)
library(dplyr)

rankall <- function(outcome, num = "best") {
  data <- read.csv("https://raw.githubusercontent.com/avynychenko/R_studing_tasks/master/Data%20for%20tasks/Coursera%20Week%204%20debugging/outcome-of-care-measures.csv", 
  colClasses = "character")
  names(data)[c(11, 17, 23)] <- c("heart attack", "heart failure", "pneumonia")
  data$State <- as.factor(data$State)
  options(warn = -1)
  res <- tryCatch( {as.data.table(data)[!is.na(as.numeric(get(outcome)))][order(as.numeric(get(outcome)), Hospital.Name)]}, error = function(x) "1")
  if (res == "1") {
    stop("invalid outcome")
  } else {
    sorted <- as.data.table(data)[!is.na(as.numeric(get(outcome)))][order(as.numeric(get(outcome)), Hospital.Name)]
    if (num == "best") {
      sorted %>% 
        group_by(State) %>% 
        slice(1) %>% 
        select(hospital = Hospital.Name,
                  state = State)
    } else if (num == "worst") {
      sorted %>% 
        group_by(State) %>% 
        summarize(hospital = tail(Hospital.Name, 1)) %>% 
        select(hospital, state = State)
    } else {
    sorted %>%
        group_by(State) %>% 
        summarize(hospital = Hospital.Name[num]) %>% 
        select(hospital, state = State)
  } } }