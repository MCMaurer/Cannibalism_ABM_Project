CannNetLogoToR <- function (inputfile, rowstodelete8isNow1, VariableChanged){
  library(dplyr)
  library(tidyr)
  library(zoo)
  
  
  ## get rid of the header portion, which doesn't have column titles, which messes stuff up
  d <- read.csv(file = inputfile, skip = 6, na.strings = "")
  
  ## this deletes all the other rows of blah information. Go to 27 for big file, 28 for small.csv
  d1 <- d[-rowstodelete8isNow1,]

  ## flip the script!
  fd1 <- t(d1)
  fd1 <- fd1[-1,]
  
  ## rename the first column and change whole thing to a dataframe
  colnames(fd1) <- c(VariableChanged, "type", seq(1, (length(fd1[1,])-2), 1))
  
  fd1 <- as.data.frame(fd1)
  
  ## replace all the NAs for the values with the previous ones, since they only go every other.
  fd1[VariableChanged] <- na.locf(fd1[VariableChanged])

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
  
  # make stuff numeric
  fd3$Run <- as.numeric(fd3$Run)
  fd3$Time <- as.numeric(fd3$Time)
  fd3$Count <- as.numeric(fd3$Count)
  return(fd3)
}