fullInput <- function(inputDirectory, numFiles){
  data <- NULL
  for(x in 1:numFiles){
  new.data <- as.data.frame(read.csv(file = paste(inputDirectory,"/GSA_Output_Test",x,".csv",sep=''), skip=6))
  new.data[,1] <- x
  names(new.data)[1] <- "experiment"
  names(new.data)[16] <- "time"
  names(new.data)[17] <- "count_uninfecteds"
  names(new.data)[18] <- "count_infecteds"
  names(new.data)[19] <- "count_juveniles"
  names(new.data)[20] <- "count_inf_juveniles"
  data <- rbind(data,new.data)
  }
  return(data)
}

tenthInput <- function(inputDirectory, numFiles){
  data <- NULL
  for(x in 1:numFiles){
    new.data <- as.data.frame(read.csv(file = paste(inputDirectory,"/GSA_Output_Test",x,".csv",sep=''), skip=6))[seq(from=1,to=10001,by=10),]
    new.data[,1] <- x
    names(new.data)[1] <- "experiment"
    names(new.data)[16] <- "time"
    names(new.data)[17] <- "count_uninfecteds"
    names(new.data)[18] <- "count_infecteds"
    names(new.data)[19] <- "count_juveniles"
    names(new.data)[20] <- "count_inf_juveniles"
    data <- rbind(data,new.data)
  }
  return(data)
}

data <- fullInput("/Users/MJ/GitHub/Cannibalism_ABM_Project/GSA_Outputs",10)
data <- tenthInput("/Users/MJ/GitHub/Cannibalism_ABM_Project/GSA_Outputs",10)


## this all looks solid. Next step is looking into parallelization of NetLogo runs. Later down the line it might be worth paring down some of the parameter ranges based on what can produce reasonable results. For example, if maturation time is too long and everyone dies immediately no matter what, it might not make sense wasting runs exploring that part of parameter space, and the max level could be brought down