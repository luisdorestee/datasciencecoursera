# Files information:
#   outcome-of-care-measures: info 30-day mortality and readmission rates for
#                             heart attacks & failure, and pneumonia (4000 hosp)
#   hospital-data:            info about each hospital
#   Hospital_Revised_Flatfiles: describes variables in each file

### 0. Set working directory and load files
setwd("C:/Users/User/Desktop/RStudio/datascience")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hos_data <- read.csv("hospital-data.csv")
str(outcome) #for looking at the structure of the files


#### 1. 30-day mortality rates for heart attack
outcome[, 11] <- as.numeric(outcome[,11])
hist(outcome[, 11], xlab = "Mortality rates", main = "30-day mortality rates for  heart attack")


#### 2. Finding best hospital in a state
# Function that gets 2 arguments: 'state' & 'out'.
# 'state': 2-char name of state.
# 'out': "Heart attack", "Heart failure" & "Pneumonia".
# It returns a char vector with name of the hospital with best (lowest) 30 day mortality.
# In case of tie, alfabetic order
#### 2. Finding the best hospital in a state
best <- function(state, outcome_name) {
  # Load the outcome data inside the function to work with fresh data each time
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Define valid outcomes and their corresponding column indices
  valid_outcomes <- c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                      "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                      "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  # Convert state to upper case for comparison
  state <- toupper(state)
  
  # Check that state and outcome are valid
  if (!state %in% unique(outcome_data$State)) {
    stop("invalid state")
  }
  outcome_name <- tolower(outcome_name)
  if (!outcome_name %in% names(valid_outcomes)) {
    stop("invalid outcome")
  }
  
  # Get the index of the column for the requested outcome
  outcome_column <- valid_outcomes[outcome_name]
  
  # Convert the outcome column to numeric, handling NAs
  outcome_data[[outcome_column]] <- as.numeric(outcome_data[[outcome_column]])
  
  # Filter hospitals by the given state and valid outcome data
  hospitals <- outcome_data[outcome_data$State == state & !is.na(outcome_data[[outcome_column]]), ]
  
  # If no hospitals meet the criteria, return NA
  if (nrow(hospitals) == 0) {
    return(NA)
  }
  
  # Order hospitals by outcome data, then by hospital name to handle ties
  hospitals <- hospitals[order(hospitals[[outcome_column]], hospitals$Hospital.Name), ]
  
  # Return the name of the hospital with the best (lowest) outcome
  best_hospital <- hospitals$Hospital.Name[1]
  
  return(best_hospital)
}
# Call the best function with the state abbreviation and outcome name
best_hospital_in_nj_for_heart_attack <- best("MD", "heart attack")

# Print the result
print(best_hospital_in_nj_for_heart_attack)


# Example usage:
result <- best("AK", "pneumonia")
cat("The best hospital for heart attack in MD:", result, "\n")



#### 3. Ranking hospitals by outcomes in a state
# Function that gets 3 arguments: 'state', 'out' (as 2.) & num.
# num: the ranking of the hospital at that state for that ranking
# Rank hospitals based on the 30-day mortality rate for the specified outcome
# Handle ties by sorting hospitals with the same mortality rate alphabetically.
rankhospital <- function(state, outcome_name, num = "best") {
  # We assume that 'outcome' data frame is already in the environment
  
  # Define valid outcomes and their corresponding column names
  valid_outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  # Convert state to upper case for comparison
  state <- toupper(state)
  
  # Check that state and outcome are valid
  if (!state %in% unique(outcome$State)) {
    stop("invalid state")
  }
  
  outcome_name <- tolower(outcome_name)
  if (!outcome_name %in% names(valid_outcomes)) {
    stop("invalid outcome")
  }
  
  # Get the index of the column for the requested outcome
  outcome_index <- valid_outcomes[outcome_name]
  
  # Convert the outcome column to numeric, handling NAs
  outcome[, outcome_index] <- as.numeric(outcome[, outcome_index])
  
  # Filter hospitals by the given state and valid outcome data
  hospitals <- outcome[outcome$State == state & !is.na(outcome[, outcome_index]), ]
  
  # If no hospitals meet the criteria, return NA
  if (nrow(hospitals) == 0) {
    return(NA)
  }
  
  # Order hospitals by outcome data, then by hospital name to handle ties
  hospitals <- hospitals[order(hospitals[, outcome_index], hospitals$Hospital.Name), ]
  
  # Determine the rank requested
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- nrow(hospitals)
  } else if (!is.numeric(num) || num > nrow(hospitals)) {
    return(NA)
  }
  
  # Return the hospital name at the requested rank
  hospital_name <- hospitals$Hospital.Name[num]
  return(hospital_name)
}

# Example usage of rankhospital:
ranked_hospital <- rankhospital("NY", "heart attack", 7)
print(ranked_hospital)


#### 4. Ranking hospitals in all states
# Function that gets 2 arguments: 'out' & num: hospital ranking, returns a data frame
# containing hospital in each state that has that ranking specified in 'num'.
# It must return a value for every state (some NAs). 
# First colmumn: Hospital, Second column: State
rankall <- function(outcome, num = "best") {
  # Read outcome data within the function
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Define valid outcomes and their corresponding column names, update these as per your dataset
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  outcome_columns <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  # Check that outcome is valid
  outcome_name <- tolower(outcome)
  if (!outcome_name %in% valid_outcomes) {
    stop("invalid outcome")
  }
  
  # Get the index of the column for the requested outcome
  outcome_index <- outcome_columns[outcome_name]
  
  # Convert the outcome column to numeric, handling NAs
  outcome_data[, outcome_index] <- as.numeric(outcome_data[, outcome_index])
  
  # Initialize an empty data frame to store results
  results <- data.frame(hospital = character(), state = character(), stringsAsFactors = FALSE)
  
  # Loop over all states
  for (st in unique(outcome_data$State)) {
    # Filter hospitals by the given state and valid outcome data
    state_data <- outcome_data[outcome_data$State == st & !is.na(outcome_data[, outcome_index]), ]
    
    # If no hospitals meet the criteria, add an NA entry and move to the next state
    if (nrow(state_data) == 0 || is.na(num)) {
      results <- rbind(results, data.frame(hospital = NA, state = st))
      next
    }
    
    # Order hospitals by outcome data, then by hospital name to handle ties
    ordered_hospitals <- state_data[order(state_data[, outcome_index], state_data$Hospital.Name), ]
    
    # Determine the rank requested
    if (num == "best") {
      rank_num <- 1
    } else if (num == "worst") {
      rank_num <- nrow(ordered_hospitals)
    } else {
      rank_num <- as.integer(num)
    }
    
    # Check if the requested rank is within the number of hospitals
    if (rank_num > nrow(ordered_hospitals)) {
      hospital_name <- NA
    } else {
      hospital_name <- ordered_hospitals$Hospital.Name[rank_num]
    }
    
    # Append the results to the data frame
    results <- rbind(results, data.frame(hospital = hospital_name, state = st))
  }
  
  # Return the results data frame
  return(results)
}

# Call the rankall function for 'heart attack' and the 'best' hospital
best_hospitals_heart_attack <- rankall("heart attack", "best")

# View the first few rows of the resulting data frame
head(best_hospitals_heart_attack, 3)
