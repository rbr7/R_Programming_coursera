pollutantmean <- function(directory, pollutant, id = 1:332)
{
  # create a list of files
  
  complete_files <- list.files(directory, full.names = TRUE) 
  
  # create an empty data frame
  dat <- data.frame()
  
  for (i in id)
  {
    # add files to main data
    dat <- rbind(dat, read.csv(complete_files[i]))
    
  }
  
  # Calulate mean
  mean_data <- mean(dat[, pollutant], na.rm = TRUE)
  return(mean_data)
}

# OUTPUT

#source(pollutantmean.R) 

#pollutantmean("specdata", "sulfate", 1:10)
#[1] 4.064128

#pollutantmean("specdata", "nitrate", 70:72)
#[1] 1.706047

#pollutantmean("specdata", "nitrate", 23)
#[1] 1.280833
