library(dplyr)
library(tidyr)
library(magrittr)
library(zoo)
source("GitHub/Cannibalism_ABM_Project/CannNetLogoToR_function.R")
#### Movement = 1 ####


inf_cann_mov_1 <- CannNetLogoToR("Documents/Cann_ABM_Outputs copy/Cann_Model_stripped_down_inf_cann_mov_1-spreadsheet.csv",c(1:11, 13:28), "inf_cannibalism")

# inf_cann_mov_1_2 <- inf_cann_mov_1 %>% 
#   filter(grepl("[0]$", Time)) %>% 
#   group_by(inf_cannibalism, type, Time) %>% 
#   summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n())
# 
# 
# inf_cann_mov_1_2$Time <- as.numeric(inf_cann_mov_1_2$Time)
# inf_cann_mov_1_2$avgCount <- as.numeric(inf_cann_mov_1_2$avgCount)
# inf_cann_mov_1_2$variance <- as.numeric(inf_cann_mov_1_2$variance)
# inf_cann_mov_1_2$stdev <- as.numeric(inf_cann_mov_1_2$stdev)
# inf_cann_mov_1_2$type <- as.factor(inf_cann_mov_1_2$type)
# inf_cann_mov_1_2$n <- as.numeric(inf_cann_mov_1_2$n)
# inf_cann_mov_1_2$inf_cannibalism <- as.numeric(inf_cann_mov_1_2$inf_cannibalism)
# 
# 
# inf_cann_mov_1_2 %>% 
#   ggplot(aes(x=Time, y=avgCount, group=interaction(type, inf_cannibalism), colour=inf_cannibalism, linetype=type))+
#   geom_line()+#aes(alpha=0.9, size=(n/10)))+
#   scale_x_continuous(breaks = seq(0, 15000, 1000))+
#   scale_y_continuous(breaks = seq(0, 700, 50))+
#   scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 50)+
#   theme_bw()

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
# 
# ## plot infection against cannibalism rate
# inf_cann_mov_1_6 %>% 
#   ggplot(aes(x=inf_cannibalism,y=mean_inf_perc))+
#   geom_line()+
#   theme_bw()
# 
# ## now

inf_cann_mov_1_7 <- inf_cann_mov_1_5 %>%
  group_by(inf_cannibalism) %>%
  summarise(mean_total_pop = mean(total_pop))

# inf_cann_mov_1_7 %>% 
#   ggplot(aes(x=inf_cannibalism,y=mean_total_pop))+
#   geom_line()+
#   theme_bw()


## try plotting both together
par(mar=c(5,3,4,3))
plot(inf_cann_mov_1_7$inf_cannibalism,inf_cann_mov_1_7$mean_total_pop,type="l",xlab="infected cannibalism rate", ylab=NA, main="movement = 1",ylim=c(0,1200))
mtext(side=2,line=2,"mean total population size")
par(new=T)
plot(inf_cann_mov_1_7$inf_cannibalism,inf_cann_mov_1_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue",ylim=c(0,100))
axis(side=4)
mtext(side=4,line=2,"mean infection rate")
legend("topright", col=c("black","blue"),
       legend=c("pop size","inf rate"),lty = rep(1,2),cex=0.75)


#### Movement = 2 ####



inf_cann_mov_2 <- CannNetLogoToR("Documents/Cann_ABM_Outputs copy/Cann_Model_stripped_down inf_cann_mov_2-spreadsheet.csv",c(1:11, 13:28), "inf_cannibalism")
# 
# inf_cann_mov_2_2 <- inf_cann_mov_2 %>% 
#   filter(grepl("[0]$", Time)) %>% 
#   group_by(inf_cannibalism, type, Time) %>% 
#   summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n())
# 
# 
# inf_cann_mov_2_2$Time <- as.numeric(inf_cann_mov_2_2$Time)
# inf_cann_mov_2_2$avgCount <- as.numeric(inf_cann_mov_2_2$avgCount)
# inf_cann_mov_2_2$variance <- as.numeric(inf_cann_mov_2_2$variance)
# inf_cann_mov_2_2$stdev <- as.numeric(inf_cann_mov_2_2$stdev)
# inf_cann_mov_2_2$type <- as.factor(inf_cann_mov_2_2$type)
# inf_cann_mov_2_2$n <- as.numeric(inf_cann_mov_2_2$n)
# inf_cann_mov_2_2$inf_cannibalism <- as.numeric(inf_cann_mov_2_2$inf_cannibalism)
# 
# 
# inf_cann_mov_2_2 %>% 
#   ggplot(aes(x=Time, y=avgCount, group=interaction(type, inf_cannibalism), colour=inf_cannibalism, linetype=type))+
#   geom_line()+#aes(alpha=0.9, size=(n/10)))+
#   scale_x_continuous(breaks = seq(0, 15000, 1000))+
#   scale_y_continuous(breaks = seq(0, 700, 50))+
#   scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 50)+
#   theme_bw()

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
# 
# ## plot infection against cannibalism rate
# inf_cann_mov_2_6 %>% 
#   ggplot(aes(x=inf_cannibalism,y=mean_inf_perc))+
#   geom_line()+
#   theme_bw()
# 
## now

inf_cann_mov_2_7 <- inf_cann_mov_2_5 %>%
  group_by(inf_cannibalism) %>%
  summarise(mean_total_pop = mean(total_pop))

# inf_cann_mov_2_7 %>% 
#   ggplot(aes(x=inf_cannibalism,y=mean_total_pop))+
#   geom_line()+
#   theme_bw()
# 

## try plotting both together
par(mar=c(5,3,4,3))
plot(inf_cann_mov_2_7$inf_cannibalism,inf_cann_mov_2_7$mean_total_pop,type="l",xlab="infected cannibalism rate", ylab=NA, main="movement = 2",ylim=c(0,1200))
mtext(side=2,line=2,"mean total population size")
par(new=T)
plot(inf_cann_mov_2_7$inf_cannibalism,inf_cann_mov_2_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue",ylim=c(0,100))
axis(side=4)
mtext(side=4,line=2,"mean infection rate")
legend("topright", col=c("black","blue"),
       legend=c("pop size","inf rate"),lty = rep(1,2),cex=0.75)


#### Movement = 3 ####


inf_cann_mov_3 <- CannNetLogoToR("Documents/Cann_ABM_Outputs copy/Cann_Model_stripped_down inf_cann_mov_3-spreadsheet.csv",c(1:11, 13:28), "inf_cannibalism")

# inf_cann_mov_3_2 <- inf_cann_mov_3 %>% 
#   filter(grepl("[0]$", Time)) %>% 
#   group_by(inf_cannibalism, type, Time) %>% 
#   summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n())
# 
# 
# inf_cann_mov_3_2$Time <- as.numeric(inf_cann_mov_3_2$Time)
# inf_cann_mov_3_2$avgCount <- as.numeric(inf_cann_mov_3_2$avgCount)
# inf_cann_mov_3_2$variance <- as.numeric(inf_cann_mov_3_2$variance)
# inf_cann_mov_3_2$stdev <- as.numeric(inf_cann_mov_3_2$stdev)
# inf_cann_mov_3_2$type <- as.factor(inf_cann_mov_3_2$type)
# inf_cann_mov_3_2$n <- as.numeric(inf_cann_mov_3_2$n)
# inf_cann_mov_3_2$inf_cannibalism <- as.numeric(inf_cann_mov_3_2$inf_cannibalism)
# 
# 
# inf_cann_mov_3_2 %>% 
#   ggplot(aes(x=Time, y=avgCount, group=interaction(type, inf_cannibalism), colour=inf_cannibalism, linetype=type))+
#   geom_line()+#aes(alpha=0.9, size=(n/10)))+
#   scale_x_continuous(breaks = seq(0, 15000, 1000))+
#   scale_y_continuous(breaks = seq(0, 700, 50))+
#   scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 50)+
#   theme_bw()

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
# 
# ## plot infection against cannibalism rate
# inf_cann_mov_3_6 %>% 
#   ggplot(aes(x=inf_cannibalism,y=mean_inf_perc))+
#   geom_line()+
#   theme_bw()
# 
## now

inf_cann_mov_3_7 <- inf_cann_mov_3_5 %>%
  group_by(inf_cannibalism) %>%
  summarise(mean_total_pop = mean(total_pop))

# inf_cann_mov_3_7 %>% 
#   ggplot(aes(x=inf_cannibalism,y=mean_total_pop))+
#   geom_line()+
#   theme_bw()


## try plotting both together
par(mar=c(5,3,4,3))
plot(inf_cann_mov_3_7$inf_cannibalism,inf_cann_mov_3_7$mean_total_pop,type="l",xlab="infected cannibalism rate", ylab=NA, main="movement = 3",ylim=c(0,1200))
mtext(side=2,line=2,"mean total population size")
par(new=T)
plot(inf_cann_mov_3_7$inf_cannibalism,inf_cann_mov_3_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue",ylim=c(0,100))
axis(side=4)
mtext(side=4,line=2,"mean infection rate")
legend("topright", col=c("black","blue"),
       legend=c("pop size","inf rate"),lty = rep(1,2),cex=0.75)


#### Movement = 4 ####


inf_cann_mov_4 <- CannNetLogoToR("Documents/Cann_ABM_Outputs copy/Cann_Model_stripped_down inf_cann_mov_4-spreadsheet.csv",c(1:11, 13:28), "inf_cannibalism")

# inf_cann_mov_4_2 <- inf_cann_mov_4 %>% 
#   filter(grepl("[0]$", Time)) %>% 
#   group_by(inf_cannibalism, type, Time) %>% 
#   summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n())
# 
# 
# inf_cann_mov_4_2$Time <- as.numeric(inf_cann_mov_4_2$Time)
# inf_cann_mov_4_2$avgCount <- as.numeric(inf_cann_mov_4_2$avgCount)
# inf_cann_mov_4_2$variance <- as.numeric(inf_cann_mov_4_2$variance)
# inf_cann_mov_4_2$stdev <- as.numeric(inf_cann_mov_4_2$stdev)
# inf_cann_mov_4_2$type <- as.factor(inf_cann_mov_4_2$type)
# inf_cann_mov_4_2$n <- as.numeric(inf_cann_mov_4_2$n)
# inf_cann_mov_4_2$inf_cannibalism <- as.numeric(inf_cann_mov_4_2$inf_cannibalism)
# 
# 
# inf_cann_mov_4_2 %>% 
#   ggplot(aes(x=Time, y=avgCount, group=interaction(type, inf_cannibalism), colour=inf_cannibalism, linetype=type))+
#   geom_line()+#aes(alpha=0.9, size=(n/10)))+
#   scale_x_continuous(breaks = seq(0, 15000, 1000))+
#   scale_y_continuous(breaks = seq(0, 700, 50))+
#   scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 50)+
#   theme_bw()

# generating other plots

inf_cann_mov_4_3 <- inf_cann_mov_4 %>% 
  group_by(inf_cannibalism, type, Time) %>% 
  summarise(avgCount = mean(Count))

inf_cann_mov_4_3$inf_cannibalism <- as.numeric(inf_cann_mov_4_3$inf_cannibalism)
inf_cann_mov_4_3$type <- as.factor(inf_cann_mov_4_3$type)
inf_cann_mov_4_3$Time <- as.numeric(inf_cann_mov_4_3$Time)
inf_cann_mov_4_3$avgCount <- as.numeric(inf_cann_mov_4_3$avgCount)

inf_cann_mov_4_4 <- tidyr::spread(data = inf_cann_mov_4_3,key = type,value = avgCount)
head(inf_cann_mov_4_4)
inf_cann_mov_4_4$inf_perc <- (inf_cann_mov_4_4$infecteds/(inf_cann_mov_4_4$infecteds + inf_cann_mov_4_4$uninfecteds))*100
inf_cann_mov_4_4$total_pop <- inf_cann_mov_4_4$infecteds + inf_cann_mov_4_4$uninfecteds

inf_cann_mov_4_5 <- inf_cann_mov_4_4 %>% 
  filter(Time >= 5000)

inf_cann_mov_4_6 <- inf_cann_mov_4_5 %>% 
  group_by(inf_cannibalism) %>% 
  summarise(mean_inf_perc = mean(inf_perc))
# 
# ## plot infection against cannibalism rate
# inf_cann_mov_4_6 %>% 
#   ggplot(aes(x=inf_cannibalism,y=mean_inf_perc))+
#   geom_line()+
#   theme_bw()
# 
## now

inf_cann_mov_4_7 <- inf_cann_mov_4_5 %>%
  group_by(inf_cannibalism) %>%
  summarise(mean_total_pop = mean(total_pop))

# inf_cann_mov_4_7 %>% 
#   ggplot(aes(x=inf_cannibalism,y=mean_total_pop))+
#   geom_line()+
#   theme_bw()


## try plotting both together
par(mar=c(5,3,4,3))
plot(inf_cann_mov_4_7$inf_cannibalism,inf_cann_mov_4_7$mean_total_pop,type="l",xlab="infected cannibalism rate", ylab=NA, main="movement = 4",ylim=c(0,1200))
mtext(side=2,line=2,"mean total population size")
par(new=T)
plot(inf_cann_mov_4_7$inf_cannibalism,inf_cann_mov_4_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue",ylim=c(0,100))
axis(side=4)
mtext(side=4,line=2,"mean infection rate")
legend("topright", col=c("black","blue"),
       legend=c("pop size","inf rate"),lty = rep(1,2),cex=0.75)




#### plot all the plots next to each other ####

dev.off()


pdf(file="/Users/MJ/GitHub/Cannibalism_ABM_Project/inf_cann_mov_all_trans_085")
par(mfrow=c(2,2))


plot(inf_cann_mov_1_7$inf_cannibalism,inf_cann_mov_1_7$mean_total_pop,type="l",xlab=NA, ylab=NA, main="movement = 1",ylim=c(0,1200))
par(new=T)
plot(inf_cann_mov_1_7$inf_cannibalism,inf_cann_mov_1_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue",ylim=c(0,100))
axis(side=4)

plot(inf_cann_mov_2_7$inf_cannibalism,inf_cann_mov_2_7$mean_total_pop,type="l",xlab=NA, ylab=NA, main="movement = 2",ylim=c(0,1200))
par(new=T)
plot(inf_cann_mov_2_7$inf_cannibalism,inf_cann_mov_2_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue",ylim=c(0,100))
axis(side=4)

plot(inf_cann_mov_3_7$inf_cannibalism,inf_cann_mov_3_7$mean_total_pop,type="l",xlab=NA, ylab=NA, main="movement = 3",ylim=c(0,1200))
par(new=T)
plot(inf_cann_mov_3_7$inf_cannibalism,inf_cann_mov_3_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue",ylim=c(0,100))
axis(side=4)

plot(inf_cann_mov_4_7$inf_cannibalism,inf_cann_mov_4_7$mean_total_pop,type="l",xlab=NA, ylab=NA, main="movement = 4",ylim=c(0,1200))
par(new=T)
plot(inf_cann_mov_4_7$inf_cannibalism,inf_cann_mov_4_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue",ylim=c(0,100))
axis(side=4)
dev.off()

#### Movement = 1 Transmission = 0.65 ####

inf_cann_mov_1_t_65 <- CannNetLogoToR("Documents/Cann_ABM_Outputs copy/Cann_Model_stripped_down inf_cann_mov_1_transmission_065-spreadsheet.csv",c(1:11, 13:28), "inf_cannibalism")

inf_cann_1_t_65 <- inf_cann_mov_1_t_65

inf_cann_1_t_65_3 <- inf_cann_1_t_65 %>% 
  group_by(inf_cannibalism, type, Time) %>% 
  summarise(avgCount = mean(Count))

inf_cann_1_t_65_3$inf_cannibalism <- as.numeric(inf_cann_1_t_65_3$inf_cannibalism)
inf_cann_1_t_65_3$type <- as.factor(inf_cann_1_t_65_3$type)
inf_cann_1_t_65_3$Time <- as.numeric(inf_cann_1_t_65_3$Time)
inf_cann_1_t_65_3$avgCount <- as.numeric(inf_cann_1_t_65_3$avgCount)

inf_cann_1_t_65_4 <- tidyr::spread(data = inf_cann_1_t_65_3,key = type,value = avgCount)
head(inf_cann_1_t_65_4)
inf_cann_1_t_65_4$inf_perc <- (inf_cann_1_t_65_4$infecteds/(inf_cann_1_t_65_4$infecteds + inf_cann_1_t_65_4$uninfecteds))*100
inf_cann_1_t_65_4$total_pop <- inf_cann_1_t_65_4$infecteds + inf_cann_1_t_65_4$uninfecteds

inf_cann_1_t_65_5 <- inf_cann_1_t_65_4 %>% 
  filter(Time >= 5000)

inf_cann_1_t_65_6 <- inf_cann_1_t_65_5 %>% 
  group_by(inf_cannibalism) %>% 
  summarise(mean_inf_perc = mean(inf_perc))
# 
# ## plot infection against cannibalism rate
# inf_cann_1_t_65_6 %>% 
#   ggplot(aes(x=inf_cannibalism,y=mean_inf_perc))+
#   geom_line()+
#   theme_bw()
# 
## now

inf_cann_1_t_65_7 <- inf_cann_1_t_65_5 %>%
  group_by(inf_cannibalism) %>%
  summarise(mean_total_pop = mean(total_pop))

#### Movement = 1 Transmission = 0.45 ####

inf_cann_mov_1_t_45 <- CannNetLogoToR("Documents/Cann_ABM_Outputs copy/Cann_Model_stripped_down inf_cann_mov_1_transmission_045-spreadsheet.csv",c(1:11, 13:28), "inf_cannibalism")
inf_cann_1_t_45 <- inf_cann_mov_1_t_45

inf_cann_1_t_45_3 <- inf_cann_1_t_45 %>% 
  group_by(inf_cannibalism, type, Time) %>% 
  summarise(avgCount = mean(Count))

inf_cann_1_t_45_3$inf_cannibalism <- as.numeric(inf_cann_1_t_45_3$inf_cannibalism)
inf_cann_1_t_45_3$type <- as.factor(inf_cann_1_t_45_3$type)
inf_cann_1_t_45_3$Time <- as.numeric(inf_cann_1_t_45_3$Time)
inf_cann_1_t_45_3$avgCount <- as.numeric(inf_cann_1_t_45_3$avgCount)

inf_cann_1_t_45_4 <- tidyr::spread(data = inf_cann_1_t_45_3,key = type,value = avgCount)
head(inf_cann_1_t_45_4)
inf_cann_1_t_45_4$inf_perc <- (inf_cann_1_t_45_4$infecteds/(inf_cann_1_t_45_4$infecteds + inf_cann_1_t_45_4$uninfecteds))*100
inf_cann_1_t_45_4$total_pop <- inf_cann_1_t_45_4$infecteds + inf_cann_1_t_45_4$uninfecteds

inf_cann_1_t_45_5 <- inf_cann_1_t_45_4 %>% 
  filter(Time >= 5000)

inf_cann_1_t_45_6 <- inf_cann_1_t_45_5 %>% 
  group_by(inf_cannibalism) %>% 
  summarise(mean_inf_perc = mean(inf_perc))
# 
# ## plot infection against cannibalism rate
# inf_cann_1_t_45_6 %>% 
#   ggplot(aes(x=inf_cannibalism,y=mean_inf_perc))+
#   geom_line()+
#   theme_bw()
# 
## now

inf_cann_1_t_45_7 <- inf_cann_1_t_45_5 %>%
  group_by(inf_cannibalism) %>%
  summarise(mean_total_pop = mean(total_pop))

#### movement = 1, transmission = 0.25 ####

inf_cann_1_t_25 <- CannNetLogoToR("Documents/Cann_ABM_Outputs copy/Cann_Model_stripped_down inf_cann_mov_1_transmission_025-spreadsheet.csv",c(1:11, 13:28), "inf_cannibalism")


inf_cann_1_t_25_3 <- inf_cann_1_t_25 %>% 
  group_by(inf_cannibalism, type, Time) %>% 
  summarise(avgCount = mean(Count))

inf_cann_1_t_25_3$inf_cannibalism <- as.numeric(inf_cann_1_t_25_3$inf_cannibalism)
inf_cann_1_t_25_3$type <- as.factor(inf_cann_1_t_25_3$type)
inf_cann_1_t_25_3$Time <- as.numeric(inf_cann_1_t_25_3$Time)
inf_cann_1_t_25_3$avgCount <- as.numeric(inf_cann_1_t_25_3$avgCount)

inf_cann_1_t_25_4 <- tidyr::spread(data = inf_cann_1_t_25_3,key = type,value = avgCount)
head(inf_cann_1_t_25_4)
inf_cann_1_t_25_4$inf_perc <- (inf_cann_1_t_25_4$infecteds/(inf_cann_1_t_25_4$infecteds + inf_cann_1_t_25_4$uninfecteds))*100
inf_cann_1_t_25_4$total_pop <- inf_cann_1_t_25_4$infecteds + inf_cann_1_t_25_4$uninfecteds

inf_cann_1_t_25_5 <- inf_cann_1_t_25_4 %>% 
  filter(Time >= 5000)

inf_cann_1_t_25_6 <- inf_cann_1_t_25_5 %>% 
  group_by(inf_cannibalism) %>% 
  summarise(mean_inf_perc = mean(inf_perc))
# 
# ## plot infection against cannibalism rate
# inf_cann_1_t_25_6 %>% 
#   ggplot(aes(x=inf_cannibalism,y=mean_inf_perc))+
#   geom_line()+
#   theme_bw()
# 
## now

inf_cann_1_t_25_7 <- inf_cann_1_t_25_5 %>%
  group_by(inf_cannibalism) %>%
  summarise(mean_total_pop = mean(total_pop))



#### plot 4 different transmission values ####

pdf(file="/Users/MJ/GitHub/Cannibalism_ABM_Project/inf_cann_mov_1_all_trans")

par(mfrow=c(2,2))


plot(inf_cann_mov_1_7$inf_cannibalism,inf_cann_mov_1_7$mean_total_pop,type="l",xlab=NA, ylab=NA, main="m = 1, t = 0.85",ylim=c(0,1200))
par(new=T)
plot(inf_cann_mov_1_7$inf_cannibalism,inf_cann_mov_1_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue",ylim=c(0,100))
axis(side=4)

plot(inf_cann_1_t_65_7$inf_cannibalism,inf_cann_1_t_65_7$mean_total_pop,type="l",xlab=NA, ylab=NA, main="m = 1, t = 0.65",ylim=c(0,1200))
par(new=T)
plot(inf_cann_1_t_65_7$inf_cannibalism,inf_cann_1_t_65_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue",ylim=c(0,100))
axis(side=4)

plot(inf_cann_1_t_45_7$inf_cannibalism,inf_cann_1_t_45_7$mean_total_pop,type="l",xlab=NA, ylab=NA, main="m = 1, t = 0.45",ylim=c(0,1200))
par(new=T)
plot(inf_cann_1_t_45_7$inf_cannibalism,inf_cann_1_t_45_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue",ylim=c(0,100))
axis(side=4)

plot(inf_cann_1_t_25_7$inf_cannibalism,inf_cann_1_t_25_7$mean_total_pop,type="l",xlab=NA, ylab=NA, main="m = 1, t = 0.25",ylim=c(0,1200))
par(new=T)
plot(inf_cann_1_t_25_7$inf_cannibalism,inf_cann_1_t_25_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue",ylim=c(0,100))
axis(side=4)


dev.off()


#### plot 4 transmission levels on same plot ####

pdf(file="/Users/MJ/GitHub/Cannibalism_ABM_Project/inf_cann_mov_1_plot")

plot(inf_cann_mov_1_7$inf_cannibalism,inf_cann_mov_1_7$mean_total_pop,type="l",xlab=NA, ylab=NA, main="Movement = 1",ylim=c(0,1200))
lines(inf_cann_1_t_65_7$inf_cannibalism,inf_cann_1_t_65_7$mean_total_pop,col="red")
lines(inf_cann_1_t_45_7$inf_cannibalism,inf_cann_1_t_45_7$mean_total_pop,col="blue")
lines(inf_cann_1_t_25_7$inf_cannibalism,inf_cann_1_t_25_7$mean_total_pop,col="green")
par(new=T)
plot(inf_cann_mov_1_7$inf_cannibalism,inf_cann_mov_1_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,lty=2,ylim=c(0,100))
axis(side=4)
legend("topright", col=c("black","red","blue","green"),
       legend=c("t=0.85","t=0.65","t=0.45","t=0.25"),lty = rep(1,4),cex=0.75)
lines(inf_cann_1_t_65_7$inf_cannibalism,inf_cann_1_t_65_6$mean_inf_perc,col="red",lty=2)
lines(inf_cann_1_t_45_7$inf_cannibalism,inf_cann_1_t_45_6$mean_inf_perc,col="blue",lty=2)
lines(inf_cann_1_t_25_7$inf_cannibalism,inf_cann_1_t_25_6$mean_inf_perc,col="green",lty=2)

dev.off()

#### same thing, but png ####

png(file="/Users/MJ/GitHub/Cannibalism_ABM_Project/inf_cann_mov_1_plot.png",width = 6, height = 6, units = 'in', res = 300)
par(mar=c(5,3,4,3))
plot(inf_cann_mov_1_7$inf_cannibalism,inf_cann_mov_1_7$mean_total_pop,type="l",xlab=NA, ylab=NA, main="Movement = 1",ylim=c(0,1200))
lines(inf_cann_1_t_65_7$inf_cannibalism,inf_cann_1_t_65_7$mean_total_pop,col="red")
lines(inf_cann_1_t_45_7$inf_cannibalism,inf_cann_1_t_45_7$mean_total_pop,col="blue")
lines(inf_cann_1_t_25_7$inf_cannibalism,inf_cann_1_t_25_7$mean_total_pop,col="green")
mtext(side=2,line=2,"mean total population size")
par(new=T)
plot(inf_cann_mov_1_7$inf_cannibalism,inf_cann_mov_1_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,lty=2,ylim=c(0,100))
axis(side=4)
legend("topright", col=c("black","red","blue","green"),
       legend=c("t=0.85","t=0.65","t=0.45","t=0.25"),lty = rep(1,4),cex=0.75)
lines(inf_cann_1_t_65_7$inf_cannibalism,inf_cann_1_t_65_6$mean_inf_perc,col="red",lty=2)
lines(inf_cann_1_t_45_7$inf_cannibalism,inf_cann_1_t_45_6$mean_inf_perc,col="blue",lty=2)
lines(inf_cann_1_t_25_7$inf_cannibalism,inf_cann_1_t_25_6$mean_inf_perc,col="green",lty=2)
mtext(side=4,line=2,"mean infection rate")
dev.off()

#### now 4 movement levels ####
png(file="/Users/MJ/Documents/Grad_School/Dissertation_Proposal/inf_cann_trans_085_plot.png",width = 6, height = 6, units = 'in', res = 300)
par(mar=c(5,3,4,3))
plot(inf_cann_mov_1_7$inf_cannibalism,inf_cann_mov_1_7$mean_total_pop,type="l",xlab="Infected Cannibalism Rate", ylab=NA, main="Transmission = 0.85",ylim=c(0,1200))
lines(inf_cann_mov_2_7$inf_cannibalism,inf_cann_mov_2_7$mean_total_pop,col="red")
lines(inf_cann_mov_3_7$inf_cannibalism,inf_cann_mov_3_7$mean_total_pop,col="blue")
lines(inf_cann_mov_4_7$inf_cannibalism,inf_cann_mov_4_7$mean_total_pop,col="green")
mtext(side=2,line=2,"Mean Total Population Size")
par(new=T)
plot(inf_cann_mov_1_7$inf_cannibalism,inf_cann_mov_1_6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,lty=2,ylim=c(0,100))
axis(side=4)
legend("topright", col=c("black","red","blue","green"),
       legend=c("mov=1","mov=2","mov=3","mov=4"),lty = rep(1,4),cex=0.75)
lines(inf_cann_mov_2_7$inf_cannibalism,inf_cann_mov_2_6$mean_inf_perc,col="red",lty=2)
lines(inf_cann_mov_3_7$inf_cannibalism,inf_cann_mov_3_6$mean_inf_perc,col="blue",lty=2)
lines(inf_cann_mov_4_7$inf_cannibalism,inf_cann_mov_4_6$mean_inf_perc,col="green",lty=2)
mtext(side=4,line=2,"Mean Infection Percentage")
dev.off()
