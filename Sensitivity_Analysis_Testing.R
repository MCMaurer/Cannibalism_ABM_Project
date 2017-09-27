#### Super simple first try ===================
parRange.list <- list()
parRange.list$Movement <- c(0,10)
parRange.list$UninfStart <- c(0,50)
library(lhs)
library(ggplot2)
library(reshape2)
library(tidyverse)

# nSamples <- 100000
# rawSamples <- randomLHS(nSamples,length(parRange.list)) # this creates a raw latin hypercube, with 100 iterations of 2 variables, each variable sampled uniformly from 0 to 1. You then have to transform this based on the actual parameter ranges you give you the LHC from my variables
# 
# paramSet <- matrix(-1, nrow = nSamples, ncol = length(parRange.list))
# for( i in 1:length(parRange.list)){
#   paramSet[,i] <- qunif(rawSamples[,i], min = parRange.list[[i]][1], max = parRange.list[[i]][2])
#   # this part takes the lhs values from 0 to 1 and transforms them based on the parameter ranges
# }

## this function does everything done above
sampleLHS.func <- function(nSamples,paramRanges){ # number of samples and the paramater ranges as a list
  rawSamples <- randomLHS(nSamples,length(paramRanges))
  paramSet <- matrix(-1, nrow = nSamples, ncol = length(paramRanges))
  for( i in 1:length(paramRanges)){
    paramSet[,i] <- qunif(rawSamples[,i], min = paramRanges[[i]][1], max = paramRanges[[i]][2])
  }
  return(as.data.frame(paramSet))
}




testParams %>% ggplot(aes(x=V1,y=V2))+{ # plot the LHS, two variables against each other
  if(length(testParams[,1]) >1500) geom_point(alpha=1/log(length(testParams[,1])),stroke=0,size=2) else geom_point(alpha=1,stroke=0,size=2)}+ # this section plots points with varying alpha values based on the number of points
  theme(panel.background = element_rect(fill = "gray100"))

testParams <- sampleLHS.func(100,paramRanges = parRange.list)

testParams %>% ggplot(aes(x=V1,y=V2))+ # plot the LHS, two variables against each other
  geom_point(alpha=(1/log(length(testParams[,1])))*2,stroke=0,size=2)+ # this section plots points with varying alpha values based on the number of points
  theme(panel.background = element_rect(fill = "gray100"))

x <- seq(from=1,to=100000)
y <- (1/log(x))+0.3 
plot(log(x),y,type="l")


#### now trying it with a bigger parameter set ==============
parRange.list <- list()
parRange.list$InfStart <- c(0,50)
parRange.list$UninfStart <- c(0,50)
parRange.list$Movement <- c(0,10)
parRange.list$InfCann <- c(0,100)
parRange.list$InfDeath <- c(0,5)

#### now how to read it out to a csv ======================
parRange.list <- list()
parRange.list$Movement <- c(0,10)
parRange.list$UninfStart <- c(0,50)
testParams <- sampleLHS.func(50,paramRanges = parRange.list)
write.csv(testParams,"GitHub/Cannibalism_ABM_Project/testParams.csv")

#### finally, let's do the real thing =============

#### testing creating the Experiments in an XML file ==============
testParams <- sampleLHS.func(100000,paramRanges = parRange.list)
testParams$V1 <- round(testParams$V1,2)
testParams$V2 <- round(testParams$V2,2)

start.time <- Sys.time()
for(i in 1:length(testParams$V1)){
  line = rep(0,13)
  
  line[1] = paste('<experiment name="Test',i,'" repetitions="1" runMetricsEveryStep="true">',sep = '')
  line[2] = '\t<setup>setup</setup>'
  line[3] = '\t<go>go</go>'
  line[4] = '\t<timeLimit steps="10000"/>'
  line[5] = '\t<metric>count uninfecteds</metric>'
  line[6] = '\t<metric>count infecteds</metric>'
  
  line[7] = '\t<enumeratedValueSet variable="V1">'
  line[8] = paste('\t\t<value value="',testParams$V1[i],'"/>',sep="")
  line[9] = '\t</enumeratedValueSet>'
  line[10] = '\t<enumeratedValueSet variable="V2">'
  line[11] = paste('\t\t<value value="',testParams$V2[i],'"/>',sep="")
  line[12] = '\t</enumeratedValueSet>'
  
  line[13] = '</experiment>'

  write(line,file="GitHub/Cannibalism_ABM_Project/GSAExperiments.xml",append=T)
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# jesus thank god this works. Ok now I don't wanna have to copy out the entire line by line format for 20 variables. Gotta figure out how to automate that process.

# also, just a quick modification could make this create a single XML file for each experiment, just changing what the file that's written to is.

