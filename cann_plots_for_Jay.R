inf_cann <- CannNetLogoToR("GitHub/Cann_ABM_Outputs/inf-cann-level.csv",c(1:7, 9:27), "inf_cannibalism")



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
     ylab=NA, main="mean for last 5000 time steps, 20 runs")
mtext(side=2,line=2,"mean total population size")
par(new=T)
plot(inf_cann7$inf_cannibalism,inf_cann6$mean_inf_perc,type="l",axes=F,xlab=NA,ylab=NA,col="blue")
axis(side=4)
mtext(side=4,line=2,"mean infection rate")
legend("topright", col=c("black","blue"),
       legend=c("pop size","inf rate"),lty = rep(1,2),cex=0.75)
