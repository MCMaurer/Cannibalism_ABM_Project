library(lhs)
library(ggplot2)
library(reshape2)
library(tidyverse)

## This will be a script to generate the XML files needed to run the FULL cannibalism model, WITHOUT uninfected cannibalism. I will run a second version of this protocol which will include that component.

sampleLHS.func <- function(nSamples,paramRanges){ # number of samples and the paramater ranges as a list
  rawSamples <- randomLHS(nSamples,length(paramRanges))
  paramSet <- matrix(-1, nrow = nSamples, ncol = length(paramRanges))
  for( i in 1:length(paramRanges)){
    paramSet[,i] <- qunif(rawSamples[,i], min = paramRanges[[i]][1], max = paramRanges[[i]][2])
  }
  return(as.data.frame(paramSet))
}

#### now trying it with a bigger parameter set ==============

parRange.list <- list()
parRange.list$initial_number_uninfecteds <- c(0,100)
parRange.list$initial_number_infecteds <- c(0,100)
parRange.list$cannibalism_level <- c(0,10)
parRange.list$inf_cannibalism_level <- c(0,100)
parRange.list$infectious_level <- c(0,100)
parRange.list$maturation_time <- c(0,200)
parRange.list$death_odds <- c(0,10)
parRange.list$movement_odds <- c(0,100)
parRange.list$can_move_odds <- c(0,100)
parRange.list$juv_move_odds <- c(0,100)
parRange.list$juv_death_odds <- c(0,10)
parRange.list$vert_trans_odds <- c(0,100)
parRange.list$inf_mature_time <- c(0,200)
parRange.list$inf_death_odds <- c(0,10)

## need to think about how the death modifiers are coded. Should probably code them up as an added percentage of the current death odds. OR just code them as two separate, hard values

# need to think about this for movement modifiers too. I guess the question is whether it's the absolute value that I'm interested in, or the relative difference between infected and uninfected. A sensitivity analysis is gonna give two different views depending on this choice. Although if it includes interaction terms, then the interaction between the two might still be captured...


#### now how to read it out to a csv ======================
# parRange.list <- list()
# parRange.list$Movement <- c(0,10)
# parRange.list$UninfStart <- c(0,50)
# testParams <- sampleLHS.func(50,paramRanges = parRange.list)
# write.csv(testParams,"GitHub/Cannibalism_ABM_Project/testParams.csv")

#### finally, let's do the real thing =============

#### testing creating the Experiments in an XML file ==============
testParams <- sampleLHS.func(10000,paramRanges = parRange.list)
for(i in 1:length(parRange.list)){
  testParams[i] <- round(testParams[i],2)
}

write("<experiments>",file="GSAExperiments_full.xml",append=T)
for(i in 1:length(testParams$V1)){
  line = rep(0,49)
  
  line[1] = paste('<experiment name="Test',i,'" repetitions="1" runMetricsEveryStep="true">',sep = '')
  line[2] = '\t<setup>setup</setup>'
  line[3] = '\t<go>go</go>'
  line[4] = '\t<timeLimit steps="10000"/>'
  line[5] = '\t<exitCondition>count turtles &gt; 10000</exitCondition>'
  line[6] = '\t<metric>count uninfecteds</metric>'
  line[7] = '\t<metric>count infecteds</metric>'
  line[8] = '\t<metric>count juveniles</metric>'
  line[9] = '\t<metric>count inf-juveniles</metric>'
  # now need a new for loop in here to generate the strings with each different parameter
  for(x in 1:length(names(parRange.list))){
    line[3*x +7] = paste('\t<enumeratedValueSet variable="',names(parRange.list)[x],'">',sep="")
    line[3*x +8] = paste('\t\t<value value="',testParams[i,x],'"/>',sep="")
    line[3*x +9] = '\t</enumeratedValueSet>'
  }

  line[52] = '</experiment>'
  
  write(line,file="GSAExperiments_full.xml",append=T)
}
write("</experiments>",file="GSAExperiments_full.xml",append=T)


## Ok this works! I can also tweak it pretty easily to make it do multiple XML files, one per experiment.
# Things to think about:
# 1) Should I record numbers of juvs and infected juvs as well?
# 2) 

