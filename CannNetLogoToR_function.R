## trying to write a function that will do the cleaning of netlogo data

## TO DO: change it so that an argument is the name of the variable changing in the trials, and so it
## gets referenced as fd2$VariableChanged

CannNetLogoToR <- function (inputfile, rowstodelete8isNow1){
  library(dplyr)
  library(tidyr)
  library(zoo)
  
  
  ## get rid of the header portion, which doesn't have column titles, which messes stuff up
  d <- read.csv(file = inputfile, skip = 6, na.strings = "")
  
  ## this deletes all the other rows of blah information
  d1 <- d[-(rowstodelete8isNow1),]
    
  ## change the rownames to a column called time, then subtract 29 to get it to start from 0
  d1$time <- rownames(d1)
  d1$time <- as.numeric(d1$time)
  d1$time <- (d1$time - 29)
  rownames(d1) <- d1$time
  
  ## flip the script!
  fd1 <- t(d1)
  
  ## rename the first column and change whole thing to a dataframe
  colnames(fd1)[1:2] <- c("VariableChanged", "type")
  
  fd1 <- as.data.frame(fd1)
  
  ## replace all the NAs for the values with the previous ones, since they only go every other.
  fd1$VariableChanged <- na.locf(fd1$VariableChanged, fromLast = T)
  
  ## get rid of row with nothing in it
  fd2 <- fd1[-1,]
  
  ## create a Run column
  fd2$Run <- rownames(fd2)
  
  # make the run column just numbers
  fd2$Run <- gsub("\\.[0-9]*$", "", fd2$Run)
  fd2$Run <- gsub("X", "", fd2$Run)
  fd2$Run <- as.numeric(fd2$Run)
  
  # get rid of "count " from the type column
  fd2$type <- gsub("count\\s", "", fd2$type)
  
  # time to TIDY IT UP
  fd3 <- gather(fd2, Time, Count, 3:10003)
  
  # arrange the data just so
  fd4 <- arrange(fd3, Run, VariableChanged, type, Time)
  
  return(fd4)
}