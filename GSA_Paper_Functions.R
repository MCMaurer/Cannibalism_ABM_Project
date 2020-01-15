##############################################################################################
## 02. Functions.R
## This script specifies the following functions:
## (1) popProj.func.comp - a compiled version of the population projection model
## (2) SA.func - runs a global sensitivity analysis for population model
## (3) emulation.func - emulates the sensitivity analysis output with boosted regression trees
##############################################################################################

###################################################################################################################################################
## Population Projection Function
## This function runs the simulation model, and requires the 20 input parameters as arguments (each has a default value, see Table 1 in the paper)
## It also requires 3 other arguments that were fixed for simulations conducted in the paper:
## demog.stoch - a logical argument specifying whether demographic stochasticity is implemented (0=FALSE, 1=TRUE [1 is the default])
## EV.surv - a numeric argument for controlling the level of environmental stochasticity affecting survival rates (default = 0.15 [standard deviation])
## nYear - the number of years to run simulations (default=10) 
## Further, the argument nIter can be specified if more than one iteration is required for each parameter sample
####################################################################################################################################################

popProj.func <- function(nStart, ageMaturity, sr, pBreed, m, s1plus, s0.mult,
                         pBreed.Imult, m.Imult, s1plus.Imult, s0.Imult, 
                         pMatTrans, pIoutside, beta, pRecover, pResistant, pLoseResistance,
                         P, aPred, hPred,
                         demog.stoch=1, EV.surv=0.15, nYear=10, nIter=NULL,...) {
  
  ## create storage matrix for results
  res.mat <- matrix(NA,nr=nIter,ncol=29)
  colnames(res.mat) <- c('nStart', 'ageMaturity', 'sr', 'pBreed', 'm', 's1plus', 's0.mult',
                         'pBreed.Imult', 'm.Imult', 's1plus.Imult', 's0.Imult', 
                         'pMatTrans', 'pIoutside', 'beta', 'pRecover', 'pResistant', 'pLoseResistance',
                         'P', 'aPred', 'hPred',
                         'demog.stoch', 'EV.surv', 'nYear', 'nIter',
                         'Nfinal', 'Extant', 'Nmin', 'lambda', 'r')
  
  ## define age.max
  age.max <- ageMaturity+1
  
  ## Loop through iterations
  for (iter in 1:nIter) {
    
    ## initiate disease vectors, females first then males
    Susceptible.vec <- Infected.vec <- Resistant.vec <- rep(0,2*age.max)
    Susceptible.vec[c(ageMaturity,2*ageMaturity+1)] <- nStart/2
    
    ## population vector is the sum of the all the disease classes
    (n.vec <- Susceptible.vec + Infected.vec + Resistant.vec)
    
    ## Storage vector for population size each year
    N <- nStart
    N.vec <- c(N,rep(NA, nYear))
    
    ## function to limit survival to 0 <= x <= 1
    survLimit.func <- function(x) {min(max(0, x),1)}
    
    for (year in 1:nYear) {
      
      #####################################
      ## Survival rates for the year
      ## Incorporates disease-dependent mortality and predation
      #####################################
      
      ## Sample disease-dependent survival for this year
      (s.rdev <- rnorm(1,mean=0,sd=EV.surv))
      (s0.UI.year <- survLimit.func(s0.mult*(s1plus + s.rdev)))
      (s1plus.UI.year <- survLimit.func(s1plus + s.rdev))
      (s0.I.year <- survLimit.func(s0.UI.year*s0.Imult))
      (s1plus.I.year <- survLimit.func(s1plus.UI.year*s1plus.Imult))
      
      ## Calculate mean number predated given prey and predator population sizes
      (num.pred <- ((aPred*N^2)/(P^2+(aPred*hPred*N^2)))*P)
      (p.survive.pred <- ifelse(N>0,survLimit.func((N-num.pred)/N),0))
      
      ## Calculate survival vectors for this year
      (s0.UI.year.vec <- rep(c(s0.UI.year,rep(s1plus.UI.year,ageMaturity)),2)*p.survive.pred)
      (s0.I.year.vec <- rep(c(s0.I.year,rep(s1plus.I.year,ageMaturity)),2)*p.survive.pred)
      
      #####################################
      ## Reproduction
      #####################################
      
      ## Determine if there are any males to allow breeding
      (num.male <- sum(n.vec[(age.max+ageMaturity):(2*age.max)]))
      if (num.male==0) {
        m.year <- 0 ; ma.I.year <- 0
      } else {
        m.year <- m ; ma.I.year <- m.Imult*m
      }
      
      ## Number of mature females in uninfected (UI) or infected (I) categories
      (n.UI.vec <- Susceptible.vec + Resistant.vec)
      (num.mature.fem.UI <- sum(n.UI.vec[ageMaturity:(ageMaturity+1)]))
      (num.mature.fem.I <- sum(Infected.vec[ageMaturity:(ageMaturity+1)]))
      
      ## Number of females breeding in UI class
      (numBreed.UI.year <- if (demog.stoch==0) {round(num.mature.fem.UI*pBreed)} else {rbinom(1, num.mature.fem.UI, pBreed)})
      
      ## Number of females breeding in I class
      (numBreed.I.year <- if (demog.stoch==0) {round(num.mature.fem.I*pBreed*pBreed.Imult)} else {rbinom(1, num.mature.fem.I, pBreed*pBreed.Imult)})
      
      ## Reproduction
      (juv.from.UI.mother <- if (demog.stoch==0) {round(numBreed.UI.year*m.year)} else {sum(rpois(numBreed.UI.year,ma.I.year))})
      (juv.from.I.mother <- if (demog.stoch==0) {round(numBreed.I.year*ma.I.year)} else {sum(rpois(numBreed.I.year,ma.I.year))})
      
      ## Maternal disease transfer
      (juv.I <-  if (demog.stoch==0) {round(juv.from.I.mother*pMatTrans)} else {rbinom(1, juv.from.I.mother, pMatTrans)})
      (juv.UI <- (juv.from.I.mother-juv.I) + juv.from.UI.mother)
      
      #####################################
      ## Disease transmission and recovery for the year
      #####################################
      
      ## number infected
      (num.I <- juv.I + sum(Infected.vec))
      
      ## derive parameters for logistic disease transmission function
      ## intercept alpha will be used to define fixed probability of infected when num.I=0 (i.e., equivalent to infection from outside source)
      ## slope beta then controls how pI increases as num.I increases (e.g., beta=5 will give a very steep rise to pI=100% almost immediatedly
      (alpha <- -(log((1/pIoutside)-1)))
      
      ## probability of exposure to disease from either outside or inside source
      ## curve(plogis(alpha+beta*x),from=0,to=100,ylim=c(0,1))
      (pE <- plogis(alpha+beta*num.I))
      
      ## Stochastic disease transmission and recovery
      ## This will always be stochastic, to allow introduction of disease from outside source when pIoutside is low
      (Exposed.juv <- rbinom(1, juv.UI, pE))
      (Exposed.vec <- rbinom(length(Susceptible.vec), Susceptible.vec, pE))
      (Susceptible.juv <- juv.UI-Exposed.juv)
      (Susceptible.vec <- Susceptible.vec - Exposed.vec)
      (Infected.juv <- juv.I + Exposed.juv)
      (Infected.vec <- Infected.vec + Exposed.vec)
      
      ## Disease recovery
      (Recovered.juv <- rbinom(1, Infected.juv, pRecover))
      (Recovered.vec <- rbinom(length(Infected.vec), Infected.vec, pRecover))
      (Resistant.juv <- rbinom(1, Recovered.juv, pResistant))
      (Resistant.vec.temp <- rbinom(length(Recovered.vec), Recovered.vec, pResistant))
      (Infected.juv <- Infected.juv - Recovered.juv)
      (Infected.vec <- Infected.vec - Recovered.vec)
      (Susceptible.vec <- Susceptible.vec + Recovered.vec)
      (Resistant.vec <- Resistant.vec + Resistant.vec.temp)
      
      #####################################
      ## Mortality for the year
      #####################################
      
      ## need to allocate juveniles to sexes based on sex ratios first
      (Susceptible.juv.male <- if (demog.stoch==0) {round(Susceptible.juv*sr)} else {rbinom(1, Susceptible.juv, sr)})
      (Susceptible.juv.fem <- Susceptible.juv - Susceptible.juv.male)
      (Infected.juv.male <- if (demog.stoch==0) {round(Infected.juv*sr)} else {rbinom(1, Infected.juv, sr)})
      (Infected.juv.fem <- Infected.juv - Infected.juv.male)
      (Resistant.juv.male <- if (demog.stoch==0) {round(Resistant.juv*sr)} else {rbinom(1, Resistant.juv, sr)})
      (Resistant.juv.fem <- Resistant.juv - Resistant.juv.male)
      
      ## contruct new population vectors
      Susceptible.vec.new <- Infected.vec.new <- Resistant.vec.new <- rep(NA,2*age.max)
      
      ## Susceptible vector and then implement mortality
      Susceptible.vec.new[c(1,(age.max+1))] <- c(Susceptible.juv.fem, Susceptible.juv.male)
      Susceptible.vec.new[c(2:age.max,((age.max+2):(2*age.max)))] <- c(Susceptible.vec[1:ageMaturity], Susceptible.vec[(age.max+1):(age.max+ageMaturity)])
      Susceptible.vec.new[c(age.max,2*age.max)] <- Susceptible.vec.new[c(age.max,2*age.max)]+Susceptible.vec[c(age.max,2*age.max)]
      (Susceptible.vec <- if (demog.stoch==0) {round(Susceptible.vec.new*s0.UI.year.vec)} else {rbinom(length(Susceptible.vec.new), Susceptible.vec.new, s0.UI.year.vec)})
      
      ## Infected vector and then implement mortality
      Infected.vec.new[c(1,(age.max+1))] <- c(Infected.juv.fem, Infected.juv.male)
      Infected.vec.new[c(2:age.max,((age.max+2):(2*age.max)))] <- c(Infected.vec[1:ageMaturity], Infected.vec[(age.max+1):(age.max+ageMaturity)])
      Infected.vec.new[c(age.max,2*age.max)] <- Infected.vec.new[c(age.max,2*age.max)]+Infected.vec[c(age.max,2*age.max)]
      (Infected.vec <- if (demog.stoch==0) {round(Infected.vec.new*s0.I.year.vec)} else {rbinom(length(Infected.vec.new), Infected.vec.new, s0.I.year.vec)})
      
      ## Resistant vector and then implement mortality
      Resistant.vec.new[c(1,(age.max+1))] <- c(Resistant.juv.fem, Resistant.juv.male)
      Resistant.vec.new[c(2:age.max,((age.max+2):(2*age.max)))] <- c(Resistant.vec[1:ageMaturity], Resistant.vec[(age.max+1):(age.max+ageMaturity)])
      Resistant.vec.new[c(age.max,2*age.max)] <- Resistant.vec.new[c(age.max,2*age.max)]+Resistant.vec[c(age.max,2*age.max)]
      (Resistant.vec <- if (demog.stoch==0) {round(Resistant.vec.new*s0.UI.year.vec)} else {rbinom(length(Resistant.vec.new), Resistant.vec.new, s0.UI.year.vec)})
      
      ## implement transition from Resistant to Susceptible state based on pLoseResistance, and adjust Suceptible vec as necessary
      (nLoseResistance.vec <- if (demog.stoch==0) {round(Resistant.vec*pLoseResistance)} else {rbinom(length(Resistant.vec), Resistant.vec, pLoseResistance)})
      (Resistant.vec <- Resistant.vec - nLoseResistance.vec)
      (Susceptible.vec <- Susceptible.vec + nLoseResistance.vec)
      
      #####################################
      ## Census
      #####################################
      
      (n.vec <- Susceptible.vec + Infected.vec + Resistant.vec)
      (N <- sum(n.vec))
      N.vec[year+1] <- N
      
    }
    
    ## calculate mean lambda and r
    (lambdas <- N.vec[2:(nYear+1)]/N.vec[1:nYear])
    (lambda <- mean(lambdas,na.rm=T))
    (r <- log(lambda))
    if(r%in%c('-Inf','Inf')) {r <- NA}
    
    ## store outputs
    res.mat[iter,'nStart'] <- nStart
    res.mat[iter,'ageMaturity'] <- ageMaturity
    res.mat[iter,'sr'] <- sr
    res.mat[iter,'m'] <- m    
    res.mat[iter,'m.Imult'] <- m.Imult
    res.mat[iter,'s0.mult'] <- s0.mult
    res.mat[iter,'s1plus'] <- s1plus
    res.mat[iter,'s0.Imult'] <- s0.Imult
    res.mat[iter,'s1plus.Imult'] <- s1plus.Imult
    res.mat[iter,'pBreed'] <- pBreed
    res.mat[iter,'pBreed.Imult'] <- pBreed.Imult
    res.mat[iter,'pMatTrans'] <- pMatTrans
    res.mat[iter,'pIoutside'] <- pIoutside
    res.mat[iter,'beta'] <- beta
    res.mat[iter,'pRecover'] <- pRecover
    res.mat[iter,'pResistant'] <- pResistant
    res.mat[iter,'pLoseResistance'] <- pLoseResistance
    res.mat[iter,'aPred'] <- aPred
    res.mat[iter,'hPred'] <- hPred
    res.mat[iter,'P'] <- P
    res.mat[iter,'demog.stoch'] <- demog.stoch
    res.mat[iter,'EV.surv'] <- EV.surv
    res.mat[iter,'nYear'] <- nYear
    res.mat[iter,'nIter'] <- nIter
    res.mat[iter,'Nfinal'] <- N
    res.mat[iter,'Extant'] <- ifelse(N>0,1,0)
    res.mat[iter,'Nmin'] <- min(N.vec)
    res.mat[iter,'lambda'] <- lambda
    res.mat[iter,'r'] <- r
  }
  
  return(res.mat)
}

## compile the function so it runs faster
popProj.func.comp <- popProj.func

##########################################################################################
## Sensitivity Analysis Function
## This function requires the package 'lhs', and specification of the following arguments:
## SAvars - vector of parameter names to be included
## nSims - total number of simulations
## nSamples - number of parameter samples to draw
## type - type of sampling to implement, currently must be 'random' or 'latin'
## NOTE: the number of model iterations per sample (nIter) is calculated as nSims/nSample
##########################################################################################

SA.func <- function(SAvars, nSims, nSamples, type, ...) {
  
  dir.create('sensitivity_analysis', showWarnings = F)
  
  ## load  packages
  if(type=='latin') library(lhs)
  
  ## calculate number of iterations per sample
  nIter <- nSims/nSamples
  
  ####################################################
  ## Generate parameter samples
  ####################################################
  
  ## first set up template with default values
  samples <- expand.grid(parDefault.list)
  samples <- samples[rep(seq_len(nrow(samples)), each=nSamples),]
  
  nVars <- length(SAvars)
  
  ## generate uniform samples between 0 and 1 for required parameters
  if (type=='random') {
    raw.samples <- matrix(NA, nrow=nSamples, ncol=nVars)
    for(i in 1:nVars) {raw.samples[,i] <- runif(nSamples,0,1)}
  } else if (type=='latin') {
    raw.samples <- randomLHS(n=nSamples, k=nVars)
  }
  
  ## transform using required ranges
  for(i in 1:length(SAvars)) {
    if (SAvars[i]=='nStart') {
      temp.nStart.vec <- qunif(raw.samples[,i], min=parRange.list[[SAvars[i]]][1], max=parRange.list[[SAvars[i]]][2])
      samples[,SAvars[i]] <- (temp.nStart.vec%/%2)*2 + ifelse(temp.nStart.vec%%2<1,0,2)
    } else if (SAvars[i]%in%c('ageMaturity','P')) {
      samples[,SAvars[i]] <- round(qunif(raw.samples[,i], min=parRange.list[[SAvars[i]]][1]-0.5, max=parRange.list[[SAvars[i]]][2])+0.5)
    } else {
      samples[,SAvars[i]] <- qunif(raw.samples[,i], min=parRange.list[[SAvars[i]]][1], max=parRange.list[[SAvars[i]]][2])
    }
  }
  
  samples$nSims <- nSims
  samples$nSamples <- nSamples
  samples$nIter <- nIter
  
  ## run the population projection
  result <- foreach(row=1:nrow(samples), .combine=rbind, .packages=c('MASS'), .export=c('popProj.func.comp')) %dopar% {
    popProj.func.comp(nStart=samples$nStart[row],ageMaturity=samples$ageMaturity[row],sr=samples$sr[row],m=samples$m[row],    
                      m.Imult=samples$m.Imult[row],s0.mult=samples$s0.mult[row],s1plus=samples$s1plus[row],
                      s0.Imult=samples$s0.Imult[row],s1plus.Imult=samples$s1plus.Imult[row],pBreed=samples$pBreed[row],
                      pBreed.Imult=samples$pBreed.Imult[row],pMatTrans=samples$pMatTrans[row],pIoutside=samples$pIoutside[row],
                      beta=samples$beta[row],pRecover=samples$pRecover[row],pResistant=samples$pResistant[row],pLoseResistance=samples$pLoseResistance[row],
                      aPred=samples$aPred[row],hPred=samples$hPred[row],P=samples$P[row],nIter=samples$nIter[row]) 
  }
  
  ## save the results
  save.nm <- paste('sensitivity_analysis/nSims=',nSims,'_nSamples=',nSamples,'_nIter=',nIter,'_type=',type,'.results',sep='')
  assign(save.nm, result)
  save(list=save.nm, file=save.nm)
  
  ## return the results
  return(result)
}

################################################################################################################################
## Emulation Function
## This function emulates the sensitivity analysis output with boosted regression trees with different interaction depths
## It takes the following arguments:
## data - the sensitivity analysis output to use (produced by the function SA.func above)
## SAvars - vector of parameter names to be included
## resp - the focal response variable
## subsample - vector of subsamples (i.e., number of data rows) for which emulation will be performed
## tree.complexities - vector of tree complexities (interaction depths) to test
################################################################################################################################

emulation.func <- function(data, SAvars, resp, subsample, tree.complexities, ...) {
  
  require(dismo)
  
  ## clean up and create folders
  gc()
  dir.create('emulation',showWarnings=F)
  dir.create('emulation/brts',showWarnings=F)
  dir.create('emulation/results',showWarnings=F)
  
  ## subset required simulation results
  dataset <- data.frame(data[1:subsample,])
  dataset$r[dataset$r%in%c('-Inf','Inf')] <- NA
  
  ## statistical distribution for fitting BRTs
  brt.dist <- ifelse(resp=='Extant','bernoulli','gaussian')
  
  ## fit BRT emulators of different tree complexities for given response variable
  for (i in 1:length(tree.complexities)) {
    
    tc <- tree.complexities[i]
    
    ## BRT 
    brt.fit <- NULL
    if((resp=='Extant' & length(unique(dataset$Extant))>1) | resp=='r') {
      x.col <- which(names(dataset)%in%SAvars)
      y.col <- which(names(dataset)==resp)
      brt.dataset <- dataset[!is.na(dataset[,y.col]),]
      brt.fit <- try(gbm.step(data=brt.dataset, gbm.x=x.col, gbm.y=y.col,family=brt.dist,tree.complexity=i,n.folds=5,tolerance.method='auto',max.trees=200000))
      ## try again with a decreased learning rate if necessary
      indic <- 1
      while((indic <= 49) & (inherits(brt.fit,'try-error')|is.null(brt.fit))) {
        lr <- 0.01*0.9
        brt.fit <- try(gbm.step(data=brt.dataset, gbm.x=x.col, gbm.y=y.col,family=brt.dist,tree.complexity=i,n.folds=5,tolerance.method='auto',tolerance=tol,learning.rate=lr,step.size=ss,max.trees=200000))
        indic <- indic + 1
      } 
      if (!inherits(brt.fit,'try-error') & !is.null(brt.fit)) {
        ## save brt file
        (brt_save.nm <- paste('subsample.',subsample,'.brt',tc,'.',resp,sep=''))
        assign(brt_save.nm,brt.fit)
        save(list=brt_save.nm,file=paste('emulation/brts/',brt_save.nm,sep=''))
        rm(list=c(brt_save.nm,'brt.fit'))
      }
      gc()
      
    }
  }
  return('Done')
}

###################################################################################################################################
## Emulation Summary Function
## This function summarises the emulation results produced by 'emulation.func' above
## It returns a list of 2 data frames that store:
## the cross-validation deviance for each emulation
## the stability of relative influence metrics results as the subsample size is increased
###################################################################################################################################

emulation.summary.func <- function(resp=resp, subsamples=subsamples, tree.complexities=tree.complexities) {
  
  #require(MDM)
  
  cvDev.df <- betaDiv.df <- data.frame(matrix(NA, nrow=length(subsamples), ncol=length(tree.complexities)+1))
  names(cvDev.df) <- names(betaDiv.df) <- c('subsample',paste('tc',tree.complexities,sep='.'))
  cvDev.df$subsample <- betaDiv.df$subsample <- subsamples
  ri.df <- NULL
  
  for (i in 1:length(tree.complexities)) {
    
    tc <- tree.complexities[i]
    
    file.nms <- paste('subsample.',subsamples,'.brt',tc,'.',resp,sep='')
    ri.temp <- data.frame(Parameter=sort(SAvars))
    
    for (j in 1:length(file.nms)) {
      load(paste('emulation/brts/',file.nms[j],sep=''))
      
      ## Relative influence part
      brt <- eval(as.name(file.nms[j]))
      ri <- brt$contributions
      ri <- ri[order(ri$var),]
      ri.temp <- cbind(ri.temp,ri[,2,drop=F])
      
      ## cv deviance part
      dev <- brt$cv.statistics$deviance.mean
      cvDev.df[j,paste('tc',tc,sep='.')] <- dev
      
      ## Clean up
      rm(list=c(file.nms[j],'brt'))
    }
    
    ri.temp$tc <- tc
    rownames(ri.temp) <- 1:nrow(ri.temp)
    ri.df <- rbind(ri.df,ri.temp)
  }
  
  ## Calculate beta diversities between pairs
  for (i in 1:length(tree.complexities)) {
    tc <- tree.complexities[i]
    df <- ri.df[ri.df$tc==tc,]
    betaDiv.dummy <- data.frame(subsample=subsamples,resp=NA)
    for (j in 2:length(subsamples)) {
      beta.div <- as.numeric(ed(t(as.matrix(df[,j:(j+1)])),q=1,retq=T)['beta'])
      betaDiv.dummy[j,2] <- beta.div    
    }
    betaDiv.df[,paste('tc',tc,sep='.')] <- betaDiv.dummy[,2]
  }
  
  return(list(cvDev = cvDev.df, betaDiv=betaDiv.df))
}

###################################################################################################################################
## Emulation Plotting Function
## This function creates a pdf plot of the results produced the 'emulation summary'
## the plot.name argument allows you to name the pdf
###################################################################################################################################

emulationPlot.func <- function(data, plot.name) {
  
  cvDev <- data[['cvDev']]
  betaDiv <- data[['betaDiv']]
  
  pdf(paste(plot.name,'pdf',sep='.'),width=6,height=8)
  par(mfrow=c(2,1),mar=c(4,5,2,2),oma=c(1,1,1,1),mgp=c(2.4,0.6,0))
  ylimits <- range(cvDev[,2:ncol(cvDev)],na.rm=T)
  plot(cvDev[,1:2],type='o',ylab='Cross-validation deviance',xlab='',pch=21,bg=1,lty=2,ylim=ylimits)
  for (i in 3:ncol(cvDev)) lines(cvDev[,c(1,i)],col=i-1,type='o',pch=21,bg=i-1,lty=2)
  legend('topright',gsub('tc.','',names(cvDev)[-1]),col=1:(ncol(cvDev)-1),lty=1,pch=21,pt.bg=1:(ncol(cvDev)-1), title='Tree Complexity')
  ylimits <- range(betaDiv[,2:ncol(betaDiv)],na.rm=T)
  plot(betaDiv[,1:2],type='o',ylab='Beta-diversity of\nrelative influence metrics',xlab='Subsample Size',pch=21,bg=1,lty=2,ylim=ylimits)
  for (i in 3:ncol(betaDiv)) lines(betaDiv[,c(1,i)],col=i-1,type='o',pch=21,bg=i-1,lty=2)
  legend('topright',gsub('tc.','',names(betaDiv)[-1]),col=1:(ncol(betaDiv)-1),lty=1,pch=21,pt.bg=1:(ncol(betaDiv)-1), title='Tree Complexity')
  dev.off()
  
}