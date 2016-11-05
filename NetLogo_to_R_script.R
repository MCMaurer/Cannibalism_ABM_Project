## attempt to read csv directly from NetLogo output and clean it up
## goal is to get rid of all the info about other parameters and stuff, but not delete them from the csv
## csv should remain intact, with all of the relevant info, this just parses out stuff to plot or analyze

library(dplyr)
library(tidyr)
library(zoo)

## get rid of the header portion, which doesn't have column titles, which messes stuff up
d <- read.csv("Github/Cannibalism_ABM_Project/Cannibalism_sensitivity_infected-death-modifier-spreadsheet.csv", skip = 6, na.strings = "")

## this deletes all the other rows of blah information
d1 <- d[-c(1, 3:27),]

#d <- d[-1,]
#d1 <- d[-(2:26),]

## change the rownames to a column called time, then subtract 29 to get it to start from 0
d1$time <- rownames(d1)
d1$time <- as.numeric(d1$time)
d1$time <- (d1$time - 29)
rownames(d1) <- d1$time

## flip the script!
fd1 <- t(d1)

## rename the first column and change whole thing to a dataframe
colnames(fd1)[1:2] <- c("inf_death_modifier", "type")

fd1 <- as.data.frame(fd1)

## replace all the NAs for the values with the previous ones, since they only go every other.
fd1$inf_death_modifier <- na.locf(fd1$inf_death_modifier, fromLast = T)

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
fd4 <- arrange(fd3, Run, inf_death_modifier, type, Time)

# check to make sure it looks right
View(fd4[1:1000,])

