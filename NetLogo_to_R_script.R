## attempt to read csv directly from NetLogo output and clean it up
## goal is to get rid of all the info about other parameters and stuff, but not delete them from the csv
## csv should remain intact, with all of the relevant info, this just parses out stuff to plot or analyze

library(dplyr)
library(tidyr)
library(zoo)


## get rid of the header portion, which doesn't have column titles, which messes stuff up
d <- read.csv(file = "/Users/MJ/Github/Cann_ABM_Outputs/movementodds.csv", skip = 6, na.strings = "")

## this deletes all the other rows of blah information. Go to 27 for big file, 28 for small.csv
d1 <- d[-c(1:4, 6:27),]

View(d1)

## change the rownames to a column called time, then subtract 29 to get it to start from 0
# d1$time <- rownames(d1)
# d1$time <- as.numeric(d1$time)
# d1$time <- (d1$time - 29)
# d1$time[[1]] <- "inf_death_modifier"
# rownames(d1) <- d1$time
# d1$time <- as.numeric(d1$time)
# d1 <- d1[,-1]


## flip the script!
fd1 <- t(d1)

View(fd1)


fd1 <- fd1[-1,]

## rename the first column and change whole thing to a dataframe
colnames(fd1) <- c("Inf_Death_Modifier", "type", seq(1, (length(fd1[1,])-2), 1))

fd1 <- as.data.frame(fd1)

## replace all the NAs for the values with the previous ones, since they only go every other.
fd1["Inf_Death_Modifier"] <- na.locf(fd1["Inf_Death_Modifier"])

fd1$Inf_Death_Modifier <- as.numeric(fd1$Inf_Death_Modifier)

# fd1[length(fd1[,1]),1] <- NA
# fd1[length(fd1[,2]),2] <- NA


## create a Run column
fd1$Run <- rownames(fd1)

# make the run column just numbers
fd1$Run <- gsub("\\.[0-9]*$", "", fd1$Run)
fd1$Run <- gsub("X", "", fd1$Run)
fd1$Run <- as.numeric(fd1$Run)

# get rid of "count " from the type column
fd1$type <- gsub("count\\s", "", fd1$type)

View(fd1)

# time to TIDY IT UP
fd2 <- gather(fd1, Time, Count, -c(1:2,Run))

# arrange the data just so
fd3 <- arrange(fd2, Run, fd2[,1], type, Time)

fd3$Run <- as.numeric(fd3$Run)
fd3$Time <- as.numeric(fd3$Time)
fd3$Count <- as.numeric(fd3$Count)
View(fd3)
