## Coursera - R Programming - Project 3

The zip file containing the data can be downloaded here:
[Assignment 3 Data](https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip)

###  1 : Plot the 30-day mortality rates for heart attack ([Plot_30_day_M_rate.R](https://github.com/rbr7/coursera_r/blob/master/projects/Plot_30_day_M_rate.R))

```R
# install.packages("data.table")
library("data.table")

# Reading in data
outcome <- data.table::fread('outcome-of-care-measures.csv')
outcome[, (11) := lapply(.SD, as.numeric), .SDcols = (11)]
outcome[, lapply(.SD
                 , hist
                 , xlab= "Deaths"
                 , main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack"
                 , col="lightgreen")
        , .SDcols = (11)]
```
![](https://github.com/rbr7/coursera_r/blob/master/projects/Hospital_30_day_death_rate.png)

###  2 : Finding the best hospital in a state ([best.R](https://github.com/rbr7/coursera_r/blob/master/projects/best.R))
```R
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

> source("best.R")
> best("TX", "heart attack")
[1] "CYPRESS FAIRBANKS MEDICAL CENTER"

> best("TX", "heart failure")
[1] "FORT DUNCAN MEDICAL CENTER"

> best("MD", "heart attack")
[1] "JOHNS HOPKINS HOSPITAL, THE"

> best("MD", "pneumonia")
[1] "GREATER BALTIMORE MEDICAL CENTER"

> best("BB", "heart attack")
Error in best("BB", "heart attack") : invalid state
> best("NY", "hert attack")
Error in best("NY", "hert attack") : invalid outcome
```

###  3 : Ranking hospitals by outcome in a state ([rankhospital.R](https://github.com/rbr7/coursera_r/blob/master/projects/rankhospital.R))
```R
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

# Sample runs

> source("rankhospital.R")

> rankhospital("TX", "heart failure", 4)
[1] "DETAR HOSPITAL NAVARRO"

> rankhospital("MD", "heart attack", "worst")
[1] "HARFORD MEMORIAL HOSPITAL"

> rankhospital("MN", "heart attack", 5000)
[1] NA
```

###  4 : Ranking hospitals in all states ([rankall.R](https://github.com/rbr7/coursera_r/blob/master/projects/rankall.R))
```R
rankall <- function(outcome, num = "best") {
  
  # Read the outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  outcome <- tolower(outcome)
  
  # Checking that outcome is valid
  outcomes <- c("heart attack","heart failure","pneumonia")
  y <- outcome %in% outcomes
  
  if(y == FALSE)
    stop("invalid outcome")
  
  
  st <- unique(outcome_data$State)
  state <- sort(st)
  
  ## Split Apply Combine strategy
  
  d <- split(outcome_data, outcome_data$State)
  
  # function to generate ranks
  
  generate_rank <- function(x){
  
  # Columns indices to keep
  hospital <- x$Hospital.Name
  state <- x$State
  
  if(outcome == "heart attack"){
    col <- x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  }
  if(outcome == "heart failure"){
    col = x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  }
  if(outcome == "pneumonia"){
    col <- x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  }
  
  Mortality_rate <- as.numeric(col)
  
  # Filtering out unwanted data 
  df <- data.frame(hospital, Mortality_rate)
  
  # removing missing values
  df <- df[!is.na(df$Mortality_rate),]
  
  # Order data by rate and then Hospital Name to ensure readability
  df <- df[with(df, order(Mortality_rate, hospital)),]
  
  # creating Rank
  df["Rank"] <- c(1:nrow(df))
  
  
  if(num == "best"){
    num = 1
  }else if(num == "worst"){
    num = nrow(df)
  }
  
  
  if(num <= nrow(df)){
      res <- df[df$Rank == num, ]
      
      return(as.character(res[1, "hospital"]))
  }else{
      return(NA)
  }
  
  }
  
  # applying function to every State
  hospital <- sapply(d, generate_rank)
  
  # Result of DFs stored as single DF depending upon vaue of    num    !!
  all_df <- data.frame(hospital, state)
  
  return(all_df)
  
}

# Sample runs

> source("rankall.R")

> head(rankall("heart attack", 20), 10)
        hospital state
AK <NA> AK
AL D W MCMILLAN MEMORIAL HOSPITAL AL
AR ARKANSAS METHODIST MEDICAL CENTER AR
AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
CA SHERMAN OAKS HOSPITAL CA
CO SKY RIDGE MEDICAL CENTER CO
CT MIDSTATE MEDICAL CENTER CT
DC <NA> DC
DE <NA> DE
FL SOUTH FLORIDA BAPTIST HOSPITAL FL

> tail(rankall("pneumonia", "worst"), 3)
        hospital state
WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
V PLATEAU MEDICAL CENTER WV
WY NORTH BIG HORN HOSPITAL DISTRICT WY

> tail(rankall("heart failure"), 10)
        hospital state
TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
TX FORT DUNCAN MEDICAL CENTER TX
UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
VA SENTARA POTOMAC HOSPITAL VA
VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
VT SPRINGFIELD HOSPITAL VT
WA HARBORVIEW MEDICAL CENTER WA
WI AURORA ST LUKES MEDICAL CENTER WI
WV FAIRMONT GENERAL HOSPITAL WV
WY CHEYENNE VA MEDICAL CENTER WY
```
