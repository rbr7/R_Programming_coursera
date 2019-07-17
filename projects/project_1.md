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

### Part 2 ([complete.R](https://github.com/rbr7/coursera_r/blob/master/projects/complete.R))
```R
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

# Sample runs

source("complete.R")

complete("specdata", c(2, 4, 8, 10, 12))
  id nobs
1  2 1041
2  4  474
3  8  192
4 10  148
5 12   96

complete("specdata", 1)
  id nobs
1  1  117

complete("specdata", 30:25)
  id nobs
1 30  932
2 29  711
3 28  475
4 27  338
5 26  586
6 25  463

complete("specdata", 3)
  id nobs
1  3  243
```

### Part 3 ([corr.R](https://github.com/rbr7/coursera_r/blob/master/projects/corr.R))
```R
corr <- function(directory, threshold = 0)
{
  # create list of files
  complete_files <- list.files(directory, full.names= TRUE)
  
  # create empty data set
  dat <- vector(mode = "numeric", length = 0)
  
  for(i in 1:length(complete_files))
  {
    # Read File
    tmp <- read.csv(complete_files[i])
    
    #Calculate csum    
    csum <- sum((!is.na(tmp$sulfate)) & (!is.na(tmp$nitrate)))
    if (csum > threshold)
    {
      #Extract data of nitrate and sulfate
      
      sul <- tmp[which(!is.na(tmp$sulfate)), ]
      nit <- sul[which(!is.na(sul$nitrate)), ]
      
      # calculate correlation between them
      dat <- c(dat, cor(nit$sulfate, nit$nitrate))
    }
  }
  
  dat
}

# Sample runs

source("corr.R")
 cr <- corr("specdata", 150)
 head(cr)
[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
 summary(cr)
   Min.   1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.21060 -0.04999  0.09463  0.12530  0.26840  0.76310 


 cr <- corr("specdata", 400)
 head(cr)
[1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
 summary(cr)
   Min.   1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.17620 -0.03109  0.10020  0.13970  0.26850  0.76310 


 cr <- corr("specdata", 5000)
 summary(cr)
  Min.  1st Qu.  Median    Mean 3rd Qu.    Max. 
length (cr)
[1] 0

 cr <- corr("specdata")
 summary(cr)
   Min.    1st Qu.   Median     Mean  3rd Qu.     Max. 
 -1.00000 -0.05282  0.10720  0.13680  0.27830  1.00000 
length(cr)
[1] 323
```
