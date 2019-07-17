rankhospital <- function(state, outcome, num = "best") {
  
  # Read the outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
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
  
  # Filtering out unneccesary data 
  Hospital_Name <- sub_data$Hospital.Name
  Mortality_rate <- as.numeric(sub_col)
  
  df <- data.frame(Hospital_Name, Mortality_rate)
  
  # Removing missing values
  df <- df[!is.na(df$Mortality_rate),]
  
  # Order Column to Top 
  df <- df[with(df, order(Mortality_rate, Hospital_Name)),]
  
  # compute Rank
  df["Rank"] <- c(1:nrow(df))
  
  
  # return result based on value of   num
  
  if(is.numeric(num)){
    if(num < nrow(df)){
      res <- df[df$Rank == num, ]
      return(as.character(res$Hospital_Name))
    }else{
      return(NA)
    }
  }
  if(num == "best"){
    return(as.character(df[1,"Hospital_Name"]))
  }
  if(num == "worst"){
    return(as.character(df[nrow(df), "Hospital_Name"]))
  }  

}

## Sample runs

#> source("rankhospital.R")

#> rankhospital("TX", "heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"

#> rankhospital("MD", "heart attack", "worst")
#[1] "HARFORD MEMORIAL HOSPITAL"

#> rankhospital("MN", "heart attack", 5000)
#[1] NA

