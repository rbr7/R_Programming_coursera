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

# OUTPUT

#source("corr.R")
# cr <- corr("specdata", 150)
# head(cr)
#[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
# summary(cr)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.21060 -0.04999  0.09463  0.12530  0.26840  0.76310 


# cr <- corr("specdata", 400)
# head(cr)
#[1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
# summary(cr)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.17620 -0.03109  0.10020  0.13970  0.26850  0.76310 


# cr <- corr("specdata", 5000)
# summary(cr)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#length (cr)
#[1] 0

# cr <- corr("specdata")
# summary(cr)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -1.00000 -0.05282  0.10720  0.13680  0.27830  1.00000 
#length(cr)
#[1] 323