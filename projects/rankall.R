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


##  Sample runs

#> source("rankall.R")

#> head(rankall("heart attack", 20), 10)
#         hospital state
#AK <NA> AK
#AL D W MCMILLAN MEMORIAL HOSPITAL AL
#AR ARKANSAS METHODIST MEDICAL CENTER AR
#AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
#CA SHERMAN OAKS HOSPITAL CA
#CO SKY RIDGE MEDICAL CENTER CO
#CT MIDSTATE MEDICAL CENTER CT
#DC <NA> DC
#DE <NA> DE
#FL SOUTH FLORIDA BAPTIST HOSPITAL FL

#> tail(rankall("pneumonia", "worst"), 3)
#         hospital state
#WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
#V PLATEAU MEDICAL CENTER WV
#WY NORTH BIG HORN HOSPITAL DISTRICT WY

#> tail(rankall("heart failure"), 10)
#         hospital state
#TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
#X FORT DUNCAN MEDICAL CENTER TX
#UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
#VA SENTARA POTOMAC HOSPITAL VA
#VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
#VT SPRINGFIELD HOSPITAL VT
#WA HARBORVIEW MEDICAL CENTER WA
#WI AURORA ST LUKES MEDICAL CENTER WI
#WV FAIRMONT GENERAL HOSPITAL WV
#WY CHEYENNE VA MEDICAL CENTER WY

