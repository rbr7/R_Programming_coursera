best <- function(state, outcome) {
  
  # Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##str(outcome_data)
  ##head(outcome_data)
  
  outcome <- tolower(outcome)
  
  # Checking that state and outcome are valid
  x = state %in% outcome_data$State
  
  outcomes <- c("heart attack","heart failure","pneumonia")
  y <- outcome %in% outcomes
  
  if(x == FALSE)
    stop("invalid state")
  if(y == FALSE)
    stop("invalid outcome")
  
  
  #Filter by state
  sub_data <- subset(outcome_data, State == state)
  ##head(sub_data)
  
  # Columns indices to keep
  if(outcome == "heart attack"){
    sub_col <- sub_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  }
  if(outcome == "heart failure"){
    sub_col = sub_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  }
  if(outcome == "pneumonia"){
    sub_col <- sub_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  }
  
  # Filtering out extra data 
  Hospital_Name <- sub_data$Hospital.Name
  Mortality_rate <- as.numeric(sub_col)
  
  df <- data.frame(Hospital_Name, Mortality_rate)
  
  # Removing missing values to keep numerical ones
  df <- df[!is.na(df$Mortality_rate),]
  
  min_rate <- min(df$Mortality_rate, na.rm = TRUE)
  
  result_df <- df[df$Mortality_rate == min_rate, ]
  
  # Order Column to Top 
  result_df <- result_df[order(result_df$Hospital_Name), ]
  
  print(as.character(result_df[, "Hospital_Name"][1]))

}

# Sample runs

#> source("best.R")
#> best("TX", "heart attack")
#[1] "CYPRESS FAIRBANKS MEDICAL CENTER"

#> best("TX", "heart failure")
#[1] "FORT DUNCAN MEDICAL CENTER"

#> best("MD", "heart attack")
#[1] "JOHNS HOPKINS HOSPITAL, THE"

#> best("MD", "pneumonia")
#[1] "GREATER BALTIMORE MEDICAL CENTER"

#> best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state
#> best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome
#>

