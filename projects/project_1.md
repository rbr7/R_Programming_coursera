## Coursera - R Programming - Project 1

For this first programming assignment you will write three functions that are meant to interact with dataset that accompanies this assignment. The dataset is contained in a zip file specdata.zip that you can download from the Coursera web site.

Although this is a programming assignment, you will be assessed using a separate quiz.



The zip file containing the data can be downloaded here:
[specdata.zip](https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip) [2.4MB]
</br>Description: The zip file contains 332 comma-separated-value (CSV) files containing pollution monitoring data. 
Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. For example, data for monitor 200 is contained in the file "200.csv". Each file contains three variables:

Date: the date of the observation in YYYY-MM-DD format (year-month-day)
sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)

### Part 1 ([pollutantmean.R](https://github.com/rbr7/coursera_r/blob/master/projects/pollutantmean.R))

```R

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


# Sample runs
source(pollutantmean.R) 

pollutantmean("specdata", "sulfate", 1:10)
[1] 4.064128

pollutantmean("specdata", "nitrate", 70:72)
[1] 1.706047

pollutantmean("specdata", "nitrate", 23)
[1] 1.280833
```
