## attempt to read csv directly from NetLogo output and clean it up
## goal is to get rid of all the info about other parameters and stuff, but not delete them from the csv
## csv should remain intact, with all of the relevant info, this just parses out stuff to plot or analyze

library(dplyr)
library(tidyr)
library(zoo)


## get rid of the header portion, which doesn't have column titles, which messes stuff up
d <- read.csv(file = "Github/Cannibalism_ABM_Project/Cannibalism_inf_death_small.csv", skip = 6, stringsAsFactors = F, na.strings = "")

## this deletes all the other rows of blah information
d1 <- d[-c(1, 3:28),]

## change the rownames to a column called time, then subtract 29 to get it to start from 0
d1$time <- rownames(d1)
d1$time <- as.numeric(d1$time)
d1$time <- (d1$time - 29)
d1$time[[1]] <- "inf_death_modifier"
rownames(d1) <- d1$time
d1$time <- as.numeric(d1$time)
d1 <- d1[,-1]
head(d1$time)

## flip the script!
fd1 <- t(d1)

View(fd1)

## rename the first column and change whole thing to a dataframe
colnames(fd1)[1:2] <- c("Inf_Death_Modifier", "type")

fd1 <- as.data.frame(fd1)

## replace all the NAs for the values with the previous ones, since they only go every other.
fd1["Inf_Death_Modifier"] <- na.locf(fd1["Inf_Death_Modifier"])

fd1$Inf_Death_Modifier <- as.numeric(fd1$Inf_Death_Modifier)

fd1[length(fd1[,1]),1] <- NA


## create a Run column
fd1$Run <- rownames(fd1)

# make the run column just numbers
fd1$Run <- gsub("\\.[0-9]*$", "", fd1$Run)
fd1$Run <- gsub("X", "", fd1$Run)
fd1$Run <- as.numeric(fd1$Run)

# get rid of "count " from the type column
fd1$type <- gsub("count\\s", "", fd1$type)

fd1$Run

# time to TIDY IT UP

## This works now!!

fd2 <- gather(fd1, Time, Count, -c(1:2,Run))

?gather

# arrange the data just so
fd3 <- arrange(fd2, Run, fd2[,1], type, Time)

fd4$Run <- as.numeric(fd4$Run)
fd4$type <- as.numeric(fd4$type)
fd4$Time <- as.numeric(fd4$Time)