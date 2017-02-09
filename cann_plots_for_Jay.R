inf_cann <- CannNetLogoToR("GitHub/Cann_ABM_Outputs/inf-cann-level.csv",c(1:7, 9:27), "inf_cannibalism")

inf_cann <- CannNetLogoToR("GitHub/Cann_ABM_Outputs/Cannibalism_Infection_Model 11_1_16 current_experiment-spreadsheet.csv",c(1:7, 9:27), "inf_cannibalism")



inf_cann2 <- inf_cann %>% 
  filter(grepl("[0]$", Time)) %>% 
  group_by(inf_cannibalism, type, Time) %>% 
  summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n())


inf_cann2$Time <- as.numeric(inf_cann2$Time)
inf_cann2$avgCount <- as.numeric(inf_cann2$avgCount)
inf_cann2$variance <- as.numeric(inf_cann2$variance)
inf_cann2$stdev <- as.numeric(inf_cann2$stdev)
inf_cann2$type <- as.factor(inf_cann2$type)
inf_cann2$n <- as.numeric(inf_cann2$n)
inf_cann2$inf_cannibalism <- as.numeric(inf_cann2$inf_cannibalism)


inf_cann2 %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, inf_cannibalism), colour=inf_cannibalism, linetype=type))+
  geom_line()+#aes(alpha=0.9, size=(n/10)))+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 50)
  
  
## do the summarizing without the grep, so it's using all data points.

inf_cann3 <- inf_cann %>% 
  #filter(grepl("[0]$", Time)) %>% 
  group_by(inf_cannibalism, type, Time) %>% 
  summarise(avgCount = mean(Count))

inf_cann3$inf_cannibalism <- as.numeric(inf_cann3$inf_cannibalism)
inf_cann3$type <- as.factor(inf_cann3$type)
inf_cann3$Time <- as.numeric(inf_cann3$Time)
inf_cann3$avgCount <- as.numeric(inf_cann3$avgCount)

inf_cann4 <- tidyr::spread(data = inf_cann3,key = type,value = avgCount)
head(inf_cann4)
inf_cann4$inf_perc <- (inf_cann4$infecteds/(inf_cann4$infecteds + inf_cann4$uninfecteds))*100
inf_cann4$total_pop <- inf_cann4$infecteds + inf_cann4$uninfecteds

inf_cann5 <- inf_cann4 %>% 
  filter(Time >= 5000)

inf_cann6 <- inf_cann5 %>% 
  group_by(inf_cannibalism) %>% 
  summarise(mean_inf_perc = mean(inf_perc))

## plot infection against cannibalism rate
inf_cann6 %>% 
  ggplot(aes(x=inf_cannibalism,y=mean_inf_perc))+
  geom_line()+
  theme_bw()

## now

inf_cann7 <- inf_cann5 %>% 
  group_by(inf_cannibalism) %>% 
  summarise(mean_total_pop = mean(total_pop))

inf_cann7 %>% 
  ggplot(aes(x=inf_cannibalism,y=mean_total_pop))+
  geom_line()+
  theme_bw()


## try plotting both together
par(mar=c(5,3,4,3))
plot(inf_cann7$inf_cannibalism,inf_cann7$mean_total_pop,type="l",xlab="infected cannibalism rate",
     ylab=NA, main="movement = 3, 10,0000 time steps")
mtext(side=2,line=2,"mean total population size")
par(new=T)
plot(inf_cann7$inf_cannibalism,inf_cann6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue")
axis(side=4)
mtext(side=4,line=2,"mean infection rate")
legend("topright", col=c("black","blue"),
       legend=c("pop size","inf rate"),lty = rep(1,2),cex=0.75)


#### new movement runs ####

source("GitHub/Cannibalism_ABM_Project/CannNetLogoToR_function.R")
inf_cann_mov_1 <- CannNetLogoToR("GitHub/Cann_ABM_Outputs/Cannibalism_Infection_Model 11_1_16 current_experiment-spreadsheet.csv",c(1:7, 9:27), "inf_cannibalism")

inf_cann_mov_1_2 <- inf_cann_mov_1 %>% 
  filter(grepl("[0]$", Time)) %>% 
  group_by(inf_cannibalism, type, Time) %>% 
  summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n())


inf_cann_mov_1_2$Time <- as.numeric(inf_cann_mov_1_2$Time)
inf_cann_mov_1_2$avgCount <- as.numeric(inf_cann_mov_1_2$avgCount)
inf_cann_mov_1_2$variance <- as.numeric(inf_cann_mov_1_2$variance)
inf_cann_mov_1_2$stdev <- as.numeric(inf_cann_mov_1_2$stdev)
inf_cann_mov_1_2$type <- as.factor(inf_cann_mov_1_2$type)
inf_cann_mov_1_2$n <- as.numeric(inf_cann_mov_1_2$n)
inf_cann_mov_1_2$inf_cannibalism <- as.numeric(inf_cann_mov_1_2$inf_cannibalism)


inf_cann_mov_1_2 %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, inf_cannibalism), colour=inf_cannibalism, linetype=type))+
  geom_line()+#aes(alpha=0.9, size=(n/10)))+
  scale_x_continuous(breaks = seq(0, 15000, 1000))+
  scale_y_continuous(breaks = seq(0, 700, 50))+
  scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 50)+
  theme_bw()

# generating other plots

inf_cann_mov_1_3 <- inf_cann_mov_1 %>% 
  group_by(inf_cannibalism, type, Time) %>% 
  summarise(avgCount = mean(Count))

inf_cann_mov_1_3$inf_cannibalism <- as.numeric(inf_cann_mov_1_3$inf_cannibalism)
inf_cann_mov_1_3$type <- as.factor(inf_cann_mov_1_3$type)
inf_cann_mov_1_3$Time <- as.numeric(inf_cann_mov_1_3$Time)
inf_cann_mov_1_3$avgCount <- as.numeric(inf_cann_mov_1_3$avgCount)

inf_cann_mov_1_4 <- tidyr::spread(data = inf_cann_mov_1_3,key = type,value = avgCount)
head(inf_cann_mov_1_4)
inf_cann_mov_1_4$inf_perc <- (inf_cann_mov_1_4$infecteds/(inf_cann_mov_1_4$infecteds + inf_cann_mov_1_4$uninfecteds))*100
inf_cann_mov_1_4$total_pop <- inf_cann_mov_1_4$infecteds + inf_cann_mov_1_4$uninfecteds

inf_cann_mov_1_5 <- inf_cann_mov_1_4 %>% 
  filter(Time >= 5000)

inf_cann_mov_1_6 <- inf_cann_mov_1_5 %>% 
  group_by(inf_cannibalism) %>% 
  summarise(mean_inf_perc = mean(inf_perc))

## plot infection against cannibalism rate
inf_cann_mov_1_6 %>% 
  ggplot(aes(x=inf_cannibalism,y=mean_inf_perc))+
  geom_line()+
  theme_bw()

## now

inf_cann_mov_1_7 <- inf_cann_mov_1_5 %>% 
  group_by(inf_cannibalism) %>% 
  summarise(mean_total_pop = mean(total_pop))

inf_cann_mov_1_7 %>% 
  ggplot(aes(x=inf_cannibalism,y=mean_total_pop))+
  geom_line()+
  theme_bw()


## try plotting both together
par(mar=c(5,3,4,3))
plot(inf_cann_mov_1_7$inf_cannibalism,inf_cann_mov_1_7$mean_total_pop,type="l",xlab="infected cannibalism rate",
     ylab=NA, main="movement = 1")
mtext(side=2,line=2,"mean total population size")
par(new=T)
plot(inf_cann_mov_1_7$inf_cannibalism,inf_cann_mov_1_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue")
axis(side=4)
mtext(side=4,line=2,"mean infection rate")
legend("topright", col=c("black","blue"),
       legend=c("pop size","inf rate"),lty = rep(1,2),cex=0.75)








#### next movement level ####


inf_cann_mov_2 <- CannNetLogoToR("GitHub/Cann_ABM_Outputs/current_exp2_output2017_02_06_13:33:37.csv",c(1:7, 9:27), "inf_cannibalism")

inf_cann_mov_2_2 <- inf_cann_mov_2 %>% 
  filter(grepl("[0]$", Time)) %>% 
  group_by(inf_cannibalism, type, Time) %>% 
  summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n())


inf_cann_mov_2_2$Time <- as.numeric(inf_cann_mov_2_2$Time)
inf_cann_mov_2_2$avgCount <- as.numeric(inf_cann_mov_2_2$avgCount)
inf_cann_mov_2_2$variance <- as.numeric(inf_cann_mov_2_2$variance)
inf_cann_mov_2_2$stdev <- as.numeric(inf_cann_mov_2_2$stdev)
inf_cann_mov_2_2$type <- as.factor(inf_cann_mov_2_2$type)
inf_cann_mov_2_2$n <- as.numeric(inf_cann_mov_2_2$n)
inf_cann_mov_2_2$inf_cannibalism <- as.numeric(inf_cann_mov_2_2$inf_cannibalism)


inf_cann_mov_2_2 %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, inf_cannibalism), colour=inf_cannibalism, linetype=type))+
  geom_line()+#aes(alpha=0.9, size=(n/10)))+
  scale_x_continuous(breaks = seq(0, 15000, 1000))+
  scale_y_continuous(breaks = seq(0, 700, 50))+
  scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 50)+
  theme_bw()

# generating other plots

inf_cann_mov_2_3 <- inf_cann_mov_2 %>% 
  group_by(inf_cannibalism, type, Time) %>% 
  summarise(avgCount = mean(Count))

inf_cann_mov_2_3$inf_cannibalism <- as.numeric(inf_cann_mov_2_3$inf_cannibalism)
inf_cann_mov_2_3$type <- as.factor(inf_cann_mov_2_3$type)
inf_cann_mov_2_3$Time <- as.numeric(inf_cann_mov_2_3$Time)
inf_cann_mov_2_3$avgCount <- as.numeric(inf_cann_mov_2_3$avgCount)

inf_cann_mov_2_4 <- tidyr::spread(data = inf_cann_mov_2_3,key = type,value = avgCount)
head(inf_cann_mov_2_4)
inf_cann_mov_2_4$inf_perc <- (inf_cann_mov_2_4$infecteds/(inf_cann_mov_2_4$infecteds + inf_cann_mov_2_4$uninfecteds))*100
inf_cann_mov_2_4$total_pop <- inf_cann_mov_2_4$infecteds + inf_cann_mov_2_4$uninfecteds

inf_cann_mov_2_5 <- inf_cann_mov_2_4 %>% 
  filter(Time >= 5000)

inf_cann_mov_2_6 <- inf_cann_mov_2_5 %>% 
  group_by(inf_cannibalism) %>% 
  summarise(mean_inf_perc = mean(inf_perc))

## plot infection against cannibalism rate
inf_cann_mov_2_6 %>% 
  ggplot(aes(x=inf_cannibalism,y=mean_inf_perc))+
  geom_line()+
  theme_bw()

## now

inf_cann_mov_2_7 <- inf_cann_mov_2_5 %>% 
  group_by(inf_cannibalism) %>% 
  summarise(mean_total_pop = mean(total_pop))

inf_cann_mov_2_7 %>% 
  ggplot(aes(x=inf_cannibalism,y=mean_total_pop))+
  geom_line()+
  theme_bw()


## try plotting both together
par(mar=c(5,3,4,3))
plot(inf_cann_mov_2_7$inf_cannibalism,inf_cann_mov_2_7$mean_total_pop,type="l",xlab="infected cannibalism rate",
     ylab=NA, main="movement = 6")
mtext(side=2,line=2,"mean total population size")
par(new=T)
plot(inf_cann_mov_2_7$inf_cannibalism,inf_cann_mov_2_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue")
axis(side=4)
mtext(side=4,line=2,"mean infection rate")
legend("topleft", col=c("black","blue"),
       legend=c("pop size","inf rate"),lty = rep(1,2),cex=0.75)




#### Movement = 9 ####

inf_cann_mov_3 <- CannNetLogoToR("GitHub/Cann_ABM_Outputs/current_exp3_output2017_02_07_13:26:14.csv",c(1:7, 9:27), "inf_cannibalism")

inf_cann_mov_3_2 <- inf_cann_mov_3 %>% 
  filter(grepl("[0]$", Time)) %>% 
  group_by(inf_cannibalism, type, Time) %>% 
  summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n())


inf_cann_mov_3_2$Time <- as.numeric(inf_cann_mov_3_2$Time)
inf_cann_mov_3_2$avgCount <- as.numeric(inf_cann_mov_3_2$avgCount)
inf_cann_mov_3_2$variance <- as.numeric(inf_cann_mov_3_2$variance)
inf_cann_mov_3_2$stdev <- as.numeric(inf_cann_mov_3_2$stdev)
inf_cann_mov_3_2$type <- as.factor(inf_cann_mov_3_2$type)
inf_cann_mov_3_2$n <- as.numeric(inf_cann_mov_3_2$n)
inf_cann_mov_3_2$inf_cannibalism <- as.numeric(inf_cann_mov_3_2$inf_cannibalism)


inf_cann_mov_3_2 %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, inf_cannibalism), colour=inf_cannibalism, linetype=type))+
  geom_line()+#aes(alpha=0.9, size=(n/10)))+
  scale_x_continuous(breaks = seq(0, 15000, 1000))+
  scale_y_continuous(breaks = seq(0, 700, 50))+
  scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 50)+
  theme_bw()

# generating other plots

inf_cann_mov_3_3 <- inf_cann_mov_3 %>% 
  group_by(inf_cannibalism, type, Time) %>% 
  summarise(avgCount = mean(Count))

inf_cann_mov_3_3$inf_cannibalism <- as.numeric(inf_cann_mov_3_3$inf_cannibalism)
inf_cann_mov_3_3$type <- as.factor(inf_cann_mov_3_3$type)
inf_cann_mov_3_3$Time <- as.numeric(inf_cann_mov_3_3$Time)
inf_cann_mov_3_3$avgCount <- as.numeric(inf_cann_mov_3_3$avgCount)

inf_cann_mov_3_4 <- tidyr::spread(data = inf_cann_mov_3_3,key = type,value = avgCount)
head(inf_cann_mov_3_4)
inf_cann_mov_3_4$inf_perc <- (inf_cann_mov_3_4$infecteds/(inf_cann_mov_3_4$infecteds + inf_cann_mov_3_4$uninfecteds))*100
inf_cann_mov_3_4$total_pop <- inf_cann_mov_3_4$infecteds + inf_cann_mov_3_4$uninfecteds

inf_cann_mov_3_5 <- inf_cann_mov_3_4 %>% 
  filter(Time >= 5000)

inf_cann_mov_3_6 <- inf_cann_mov_3_5 %>% 
  group_by(inf_cannibalism) %>% 
  summarise(mean_inf_perc = mean(inf_perc))

## plot infection against cannibalism rate
inf_cann_mov_3_6 %>% 
  ggplot(aes(x=inf_cannibalism,y=mean_inf_perc))+
  geom_line()+
  theme_bw()

## now

inf_cann_mov_3_7 <- inf_cann_mov_3_5 %>% 
  group_by(inf_cannibalism) %>% 
  summarise(mean_total_pop = mean(total_pop))

inf_cann_mov_3_7 %>% 
  ggplot(aes(x=inf_cannibalism,y=mean_total_pop))+
  geom_line()+
  theme_bw()


## try plotting both together
par(mar=c(5,3,4,3))
plot(inf_cann_mov_3_7$inf_cannibalism,inf_cann_mov_3_7$mean_total_pop,type="l",xlab="infected cannibalism rate",
     ylab=NA, main="movement = 9")
mtext(side=2,line=2,"mean total population size")
par(new=T)
plot(inf_cann_mov_3_7$inf_cannibalism,inf_cann_mov_3_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue")
axis(side=4)
mtext(side=4,line=2,"mean infection rate")
legend("topleft", col=c("black","blue"),
       legend=c("pop size","inf rate"),lty = rep(1,2),cex=0.75)





## lots of feedbacks going on here, density and cannibalism and infection all interact. It's possible that movement and infection are tied together. Simplify things a little bit, make pathogen minimally pathogenic. Get rid of infection

# also decouple density and infection, make it a steady rate of infections, this eliminates these things from the overall dynamic. This will make infection-induced cannibalism much easier to see.

# if horizontal transmission is an important mode for Geocoris, a pathogen that operates this way is not going to achieve high prevalence bc it eradicates its own host. It seems likely based on our field observations that there is some reservoir.

# essentially, run a super stripped down version and add things in from there