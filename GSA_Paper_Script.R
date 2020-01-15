####################################################################################################################
## 01. Sensitivity Analysis. R
## This script uses functions in the script '02. Functions.R' to:
## (1) run a global sensitivity analysis for the toy population model
## (2) emulate the sensitivity-analysis output using boosted regression trees with different interaction depths
## (3) confirm that parameter sampling has been sufficient for the emulator to capture the model behaviour
## Details of each function are provided in '02. Functions.R'
## Remember to set the working directory to the folder holding these scripts before running this code
## R packages required are: compiler, iterators, snow, doSNOW, foreach, lhs, dismo, MDM
####################################################################################################################

rm(list=ls())
library(compiler)
library(iterators)
library(snow)
library(doSNOW)
library(foreach)
source('GSA_Paper_Functions.R')

############################################################################
## Set parameter defaults and ranges for testing with sensitivity analyses
############################################################################

## Parameter defaults (see Table 1 of main text)
parDefault.list <- list()
parDefault.list$nStart <- 50
parDefault.list$ageMature <- 2
parDefault.list$sr <- 0.5
parDefault.list$m <- 6
parDefault.list$m.Imult <- 0.5 
parDefault.list$s0.mult <- 0.75 
parDefault.list$s1plus <- 0.75
parDefault.list$s0.Imult <- 0.5 
parDefault.list$s1plus.Imult <- 0.5
parDefault.list$pBreed <- 0.75
parDefault.list$pBreed.Imult <- 0.5
parDefault.list$pMatTrans <- 0.5
parDefault.list$pIoutside <- 0.05
parDefault.list$beta <- 0.1
parDefault.list$pRecover <- 0.75
parDefault.list$pResistant <- 0.5
parDefault.list$pLoseResistance <- 0.25
parDefault.list$aPred <- 0.05
parDefault.list$hPred <- 0.25
parDefault.list$P <- 5

## Parameter ranges (see Table 1 of main text)
parRange.list <- list()
parRange.list$nStart <- c(1,101) ## this will be converted to integer multiples of 2 (range: 2 to 100)
parRange.list$ageMaturity <- c(0.5,3.5) ## this will be converted to integers (range: 1 to 3)
parRange.list$sr <- c(0.25,0.75)
parRange.list$m <- c(2,10)
parRange.list$m.Imult <- c(0,1)
parRange.list$s0.mult <- c(0.5,1)
parRange.list$s1plus <- c(0.5,1)
parRange.list$s0.Imult <- c(0,1)
parRange.list$s1plus.Imult <- c(0,1)
parRange.list$pBreed <- c(0.5,1)
parRange.list$pBreed.Imult <- c(0,1)
parRange.list$pMatTrans <- c(0,1)
parRange.list$pIoutside <- c(0,0.5)
parRange.list$beta <- c(0,0.2)
parRange.list$pRecover <- c(0.5,1)
parRange.list$pResistant <- c(0,1)
parRange.list$pLoseResistance <- c(0,0.5)
parRange.list$aPred <- c(0,0.1)
parRange.list$hPred <- c(0,0.5)
parRange.list$P <- c(0.5,10.5) ## this will be converted to integers (range: 2 to 10)

###########################################################
## Set up parallel processing
## This will reduce simulation time and emulation time
## nproc is the number of processing cores you want to use
###########################################################

nproc <- 3
cl.tmp = makeCluster(rep('localhost',nproc), type='SOCK')
registerDoSNOW(cl.tmp)
getDoParWorkers()

############################################################################
## Sensitivity analysis
############################################################################

## Parameters to include:
## Note that we tested sensitivity-analysis designs of different complexities in the paper
## For simplicity and speed in this example code, however, I have just included the 7 demographic parameters
SAvars <- c('nStart','ageMaturity','sr','pBreed','m','s1plus','s0.mult')

## number of simulations (nSims) and number of parameter samples (nSamples)
## note that this code assumes you are running a single simulation iteration per parameter sample (as recommended in the paper)
## hence nSims = nSamples
nSims <- 5000
nSamples <- nSims

## run the global sensitivity analysis
## on my machine, this took about 5 seconds
sa <- SA.func(SAvars=SAvars, nSims=nSims, nSamples=nSamples, type='latin')


#### this doesn't actually do anything ####
########################################################################################
## Emulation
########################################################################################

## specify:
## the focus response variable - 'Extant' (indicator or population persistence) or 'r' (population growth rate)
## tree.complexities to test with the BRT emulators (just testing no interactions [tc=1] and first-order interactions [tc=2] below)
resp <- 'r'
tree.complexities <- 1:2

## subsample sizes to test during the emulation step
(subsamples <- seq(250,nSims,by=250))

## emulate the sensitivity analysis output for the required response variable
## on my machine, split across 3 processors, this took about 5 minutes (BRTs take a little time to fit)
foreach (i=length(subsamples):1, .verbose=T) %dopar% {
  emulation.func(data=sa, SAvars=SAvars, resp=resp, subsample=subsamples[i], tree.complexities=tree.complexities)
}

#### this is where the problem happens ####

## summarise the emulation by collating:
## (1) cross-validation deviance for each emulation
## (2) stability of sensitivity measures (relative influence) as sampling is increased
emulation.results <- emulation.summary.func(resp=resp, subsamples=subsamples, tree.complexities=tree.complexities)

#############################################
## Plot summary of the emulation study
#############################################

emulationPlot.func(data=emulation.results, plot.name='Example emulation plot')
