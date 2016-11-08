## trying to write a function that will do the cleaning of netlogo data

## TO DO: change it so that an argument is the name of the variable changing in the trials, and so it
## gets referenced as fd2$VariableChanged

CannNetLogoToR <- function (inputfile, rowstodelete8isNow1, VariableChanged){
  library(dplyr)
  library(tidyr)
  library(zoo)
  
  
  ## get rid of the header portion, which doesn't have column titles, which messes stuff up
  d <- read.csv(file = inputfile, skip = 6, stringsAsFactors = F, na.strings = "")
  
  ## this deletes all the other rows of blah information
  d1 <- d[-rowstodelete8isNow1,]
  
  ## change the rownames to a column called time, then subtract 29 to get it to start from 0
  d1$time <- rownames(d1)
  d1$time <- as.numeric(d1$time)
  d1$time <- (d1$time - 29)
  d1$time[[1]] <- VariableChanged
  rownames(d1) <- d1$time
  d1$time <- as.numeric(d1$time)
  d1 <- d1[,-1]
  
  ## flip the script!
  fd1 <- t(d1)
  
  ## rename the first column and change whole thing to a dataframe
  colnames(fd1)[1:2] <- c(VariableChanged, "type")
  
  fd1 <- as.data.frame(fd1)
  
  ## replace all the NAs for the values with the previous ones, since they only go every other.
  fd1[VariableChanged] <- na.locf(fd1[VariableChanged])
  
  ## get rid of the VariableChanged entry in the time row
  fd1[,1] <- as.numeric(fd1[,1])
  fd1[length(fd1[,1]),1] <- NA
  
  ## create a Run column
  fd1$Run <- rownames(fd1)
  
  # make the run column just numbers
  fd1$Run <- gsub("\\.[0-9]*$", "", fd1$Run)
  fd1$Run <- gsub("X", "", fd1$Run)
  fd1$Run <- as.numeric(fd1$Run)
  
  # get rid of "count " from the type column
  fd1$type <- gsub("count\\s", "", fd1$type)
  
  # time to TIDY IT UP
  fd2 <- gather(fd1, Time, Count, -c(1:2,Run))
  
  # arrange the data just so
  fd3 <- arrange(fd2, Run, fd2[,1], type, Time)
  
  fd3$Run <- as.numeric(fd3$Run)
  fd3$Time <- as.numeric(fd3$Time)
  fd3$Count <- as.numeric(fd3$Count)
  fd3[,1] <- as.numeric(fd3[,1])
  return(fd3)
}