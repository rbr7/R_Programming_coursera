complete <- function(directory, id = 1:332)
{
  # create a list of files
  
  complete_files <- list.files(directory, full.names = TRUE) 
  
  # create an empty data frame
  dat <- data.frame()
  
  for (i in id)
  {
    # Read files
    temp <- read.csv(complete_files[i])
    
    # nobs are sum of all complete cases
    nobs <- sum(complete.cases(temp))
    
    # Enumerates complete cass by index
    dat <- rbind(dat, data.frame(i, nobs))
    
  }
  
  colnames(dat) <- c("id", "nobs")
  
  return(dat)
}

# OUTPUT

#source("complete.R")

#complete("specdata", c(2, 4, 8, 10, 12))
#  id nobs
#1  2 1041
#2  4  474
#3  8  192
#4 10  148
#5 12   96

#complete("specdata", 1)
#  id nobs
#1  1  117

#complete("specdata", 30:25)
#  id nobs
#1 30  932
#2 29  711
#3 28  475
#4 27  338
#5 26  586
#6 25  463

#complete("specdata", 3)
#  id nobs
#1  3  243
