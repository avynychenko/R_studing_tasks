# Data
# The zip file contains 332 comma-separated-value (CSV) files containing pollution monitoring data for fine particulate matter 
# (PM) air pollution at 332 locations in the United States. Each file contains data from a single monitor and the ID number for 
# each monitor is contained in the file name.

#  Task 1
# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of 
# monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID 
# numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument 
# and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. 

pollutantmean <- function(dir, pollutant, id = 1:332) {
  if (getwd() != "C:/Users/Nastya/Desktop/specdata") {                # you can change on your personal wd
    setwd(dir) }
  files <- paste(formatC(id, width=3, flag="0"), ".csv", sep="")      #set files name in right format with zeros if it needs
  df <- do.call(rbind, lapply(files, read.csv))
  mean(df[,pollutant], na.rm = T)
}

# Task 2

# Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. 
# The function should return a data frame where the first column is the name of the file and the second column is the number of 
# complete cases. 

library(dplyr)
complete <- function(dir, id = 1:332) {
  if (getwd() != "C:/Users/Nastya/Desktop/specdata") {
    setwd(dir) }
  files <- paste(formatC(id, width=3, flag="0"), ".csv", sep="")
  df <- do.call(rbind, lapply(files, read.csv))
  data <- df[complete.cases(df),]
  data %>% 
    group_by(ID) %>% 
    summarise (n = n())
}

# Task 3

# Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between 
# sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the 
# threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no 
# monitors meet the threshold requirement, then the function should return a numeric vector of length 0. 

corr <- function(dir, threshold = 0) {
  selected_ids <- complete(dir) %>% 
    filter(n > threshold) %>% 
    select(ID)
  if (length(selected_ids[[1]] > 0)) {
    files <- paste(formatC(selected_ids[[1]], width=3, flag="0"), ".csv", sep="")
    df <- do.call(rbind, lapply(files, read.csv))
    df <- df[complete.cases(df),]
     result <- df %>% 
        group_by(ID) %>% 
        summarise(cor = cor(sulfate, nitrate))
    return(result[[2]])
  } else {
    return(c()) 
     }
}