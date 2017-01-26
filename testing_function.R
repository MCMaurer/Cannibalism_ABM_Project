setwd("/Users/MJ")
data <- CannNetLogoToR("Github/Cann_ABM_Outputs/Cannibalism_sensitivity_infected-death-modifier-spreadsheet.csv", 
                       c(1, 3:27), "Inf_Death_Modifier")


data2 <- CannNetLogoToR("Github/Cann_ABM_Outputs/Cannibalism_sensitivity_inf-fecund-modifier-spreadsheet.csv", 
                        c(1:13, 15:27), "Inf_fecund_modifier")

View(data)

View(data2)

library(ggplot2)
library(dplyr)
library(tidyr)
library(wesanderson)
library(colorspace)
library(plotly)

## this code works really well!

datasmall_avg <- data %>% 
  filter(grepl("[0]$", Time)) %>% 
  group_by(Inf_Death_Modifier, type, Time) %>% 
  summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n()) 

?dplyr::n

datasmall_avg$Time <- as.numeric(datasmall_avg$Time)
datasmall_avg$avgCount <- as.numeric(datasmall_avg$avgCount)
datasmall_avg$variance <- as.numeric(datasmall_avg$variance)
datasmall_avg$stdev <- as.numeric(datasmall_avg$stdev)
datasmall_avg$type <- as.factor(datasmall_avg$type)
datasmall_avg$n <- as.numeric(datasmall_avg$n)

datasmall_avg$coeffVar <- datasmall_avg$stdev / datasmall_avg$avgCount

View(datasmall_avg)

datasmall_avg %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, Inf_Death_Modifier), colour=Inf_Death_Modifier))+
  geom_line(aes(alpha=0.2))+#, size=1/(2*n)))+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_manual(values = colorspace::diverge_hcl(n=21))+
  theme_bw()

## now trying it with fecundity

data2small_avg <- data2 %>% 
  filter(grepl("[0]$", Time)) %>% 
  group_by(Inf_fecund_modifier, type, Time) %>% 
  summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n()) 


data2small_avg$Time <- as.numeric(data2small_avg$Time)
data2small_avg$avgCount <- as.numeric(data2small_avg$avgCount)
data2small_avg$variance <- as.numeric(data2small_avg$variance)
data2small_avg$stdev <- as.numeric(data2small_avg$stdev)
data2small_avg$type <- as.factor(data2small_avg$type)
data2small_avg$n <- as.numeric(data2small_avg$n)

View(datasmall_avg)

data2small_avg %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, Inf_fecund_modifier), colour=Inf_fecund_modifier))+
  geom_line(aes(alpha=0.2))+#, size=1/n))+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_manual(values = colorspace::diverge_hcl(n=21))+
  theme_bw()

## death in bigger steps

data3 <- CannNetLogoToR("/Users/MJ/GitHub/Cann_ABM_Outputs/current_exp_output2016_11_13_19:00:01.csv", 
                        c(1, 3:27), "Inf_death_modifier")

data3small_avg <- data3 %>% 
  filter(grepl("[0]$", Time)) %>% 
  group_by(Inf_death_modifier, type, Time) %>% 
  summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n()) 


data3small_avg$Time <- as.numeric(data3small_avg$Time)
data3small_avg$avgCount <- as.numeric(data3small_avg$avgCount)
data3small_avg$variance <- as.numeric(data3small_avg$variance)
data3small_avg$stdev <- as.numeric(data3small_avg$stdev)
data3small_avg$type <- as.factor(data3small_avg$type)
data3small_avg$n <- as.numeric(data3small_avg$n)
data3small_avg$Inf_death_modifier <- as.numeric(data3small_avg$Inf_death_modifier)

data3small_avg %>% 
  #filter(Time <=3000) %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, Inf_death_modifier), colour=Inf_death_modifier, linetype=type))+
  geom_line()+#aes(alpha=0.9, size=(n/10)))+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0.5) + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.line = element_line(size = 0.2, 
        linetype = "solid"), panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"), 
    axis.title = element_text(family = "mono"), 
    axis.text = element_text(family = "mono"), 
    legend.text = element_text(family = "mono"), 
    legend.title = element_text(family = "mono"), 
    panel.background = element_rect(fill = NA), 
    legend.key = element_rect(fill = NA), 
    legend.background = element_rect(fill = NA)) +labs(y = "# of Individuals", colour = "Viral Mortality Increase")+
    theme(legend.title = element_text(size = 7)) + theme(legend.text = element_text(size = 6))
  
   theme_few()
?theme_few

## maturation time now

data4 <- CannNetLogoToR("/Users/MJ/GitHub/Cann_ABM_Outputs/maturation_time_2016_11_11.csv", 
                        c(1:6, 8:27), "maturation_time")

data4small_avg <- data4 %>% 
  filter(grepl("[0]$", Time)) %>% 
  group_by(maturation_time, type, Time) %>% 
  summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n()) 


data4small_avg$Time <- as.numeric(data4small_avg$Time)
data4small_avg$avgCount <- as.numeric(data4small_avg$avgCount)
data4small_avg$variance <- as.numeric(data4small_avg$variance)
data4small_avg$stdev <- as.numeric(data4small_avg$stdev)
data4small_avg$type <- as.factor(data4small_avg$type)
data4small_avg$n <- as.numeric(data4small_avg$n)
data4small_avg$maturation_time <- as.numeric(data4small_avg$maturation_time)


library(plotly)
inplot <- data4small_avg %>% 
  #filter(Time <= 4000) %>% 
  #filter(maturation_time <= 200) %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, maturation_time), colour=maturation_time, linetype=type))+
  geom_line()+#aes(alpha=0.9, size=(n/10)))+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 250) + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.line = element_line(size = 0.4, 
        linetype = "solid"), panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"), 
    axis.title = element_text(family = "mono"), 
    axis.text = element_text(family = "mono"), 
    plot.title = element_text(family = "mono"), 
    legend.text = element_text(family = "mono"), 
    legend.title = element_text(family = "mono"), 
    panel.background = element_rect(fill = NA), 
    plot.background = element_rect(colour = NA), 
    legend.key = element_rect(fill = NA), 
    legend.background = element_rect(fill = NA)) +labs(y = "# of Individuals", colour = "Maturation Time")
inplot
ggplotly(inplot)

?scale_color_gradient2

## inf-cann-level


data5small_avg <- data5 %>% 
  filter(grepl("[0]$", Time)) %>% 
  group_by(Inf_cann_level, type, Time) %>% 
  summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n())



data5small_avg$Time <- as.numeric(data5small_avg$Time)
data5small_avg$avgCount <- as.numeric(data5small_avg$avgCount)
data5small_avg$variance <- as.numeric(data5small_avg$variance)
data5small_avg$stdev <- as.numeric(data5small_avg$stdev)
data5small_avg$type <- as.factor(data5small_avg$type)
data5small_avg$n <- as.numeric(data5small_avg$n)
data5small_avg$Inf_cann_level <- as.numeric(data5small_avg$Inf_cann_level)

data5small_avg %>% 
  #filter(Time <=3000) %>% 
  #filter(Inf_cann_level <= 20) %>% 
  filter(Inf_cann_level == 0 | Inf_cann_level == 5 | Inf_cann_level == 10
         | Inf_cann_level == 25 | Inf_cann_level == 50 | Inf_cann_level == 75 | Inf_cann_level == 100) %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, Inf_cann_level), colour=Inf_cann_level, linetype=type))+
  geom_line()+#aes(alpha=0.9, size=(n/10)))+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 50) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.line = element_line(size = 0.2, 
                                 linetype = "solid"), panel.grid.major = element_line(linetype = "blank"), 
        panel.grid.minor = element_line(linetype = "blank"), 
        axis.title = element_text(family = "mono"), 
        axis.text = element_text(family = "mono"), 
        legend.text = element_text(family = "mono"), 
        legend.title = element_text(family = "mono"), 
        panel.background = element_rect(fill = NA), 
        legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(y = "# of Individuals", colour = "Inf Cann Level")+
  theme(legend.title = element_text(size = 7)) + theme(legend.text = element_text(size = 6))

## between cann values of 5 and 10, there is a qualitative switch. At 5, there are more infecteds than
## uninfecteds, but at 10, it is reversed, and quite significantly. At 5, infecteds are stable at about
## 180, uninf at 100, but at 10, inf are at about 120 and uninf at 180.


## gonna try to animate some stuff

devtools::install_github("dgrtwo/gganimate")

p <- data5small_avg %>% 
  #filter(Time <=3000) %>% 
  #filter(Inf_cann_level <= 20) %>% 
  #filter(Inf_cann_level >= 5) %>% 
  ggplot(aes(x=Time, y=avgCount, colour=Inf_cann_level, linetype=type, frame=Inf_cann_level))+
  geom_point(aes(cumulative=TRUE, shape=type), cex=0.2)+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 50)+
  theme_few()

gg_animate(p)

library(ggthemes)
setwd("/Users/MJ/")
library(gganimate)
pa <- gg_animate(p)
gg_animate_save(pa, filename = "~/GitHub/Cannibalism_ABM_Project/new_cann_level.gif", saver = "gif")
?gg_animate_save


df <- data.frame(x = sample(100, replace = TRUE), y = runif(100))
p <- ggplot(df, aes(x, y)) + geom_point(aes(frame = x, cumulative = TRUE))
gg_animate(p, interval = 1)

## overall fecundity

data6 <- CannNetLogoToR("/Users/MJ/GitHub/Cann_ABM_Outputs/fecundity.csv", 
                        c(1:18, 20:27), "fecundity")



data6small_avg <- data6 %>% 
  filter(grepl("[0]$", Time)) %>% 
  group_by(fecundity, type, Time) %>% 
  summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count))

write.csv(data6small_avg, file = "/Users/MJ/GitHub/Cannibalism_ABM_Project/fecundity_small_tidy.csv")

data6small_avg$Time <- as.numeric(data6small_avg$Time)
data6small_avg$avgCount <- as.numeric(data6small_avg$avgCount)
data6small_avg$variance <- as.numeric(data6small_avg$variance)
data6small_avg$stdev <- as.numeric(data6small_avg$stdev)
data6small_avg$type <- as.factor(data6small_avg$type)
data6small_avg$n <- as.numeric(data6small_avg$n)
data6small_avg$fecundity <- as.numeric(data6small_avg$fecundity)

fecplot<-data6small_avg %>% 
  #filter(Time <=2000) %>% 
  filter(grepl("[.25]$", fecundity)) %>% 
  #filter(fecundity <= 7.5) %>% 
  #filter(fecundity >= 2.5) %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, fecundity), colour=fecundity, linetype=type))+
  geom_line()+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 10) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.line = element_line(size = 0.2, 
                                 linetype = "solid"), panel.grid.major = element_line(linetype = "blank"), 
        panel.grid.minor = element_line(linetype = "blank"), 
        axis.title = element_text(family = "mono"), 
        axis.text = element_text(family = "mono"), 
        legend.text = element_text(family = "mono"), 
        legend.title = element_text(family = "mono"), 
        panel.background = element_rect(fill = NA), 
        legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(y = "# of Individuals", colour = "fecundity")+
  theme(legend.title = element_text(size = 7)) + theme(legend.text = element_text(size = 6))

ggplotly(fecplot)

# death_odds

data7 <- CannNetLogoToR("/Users/MJ/GitHub/Cann_ABM_Outputs/death_odds.csv", 
                        c(1:11, 13:27), "death_odds")

data7small_avg <- data7 %>% 
  filter(grepl("[0]$", Time)) %>% 
  group_by(death_odds, type, Time) %>% 
  summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n())


data7small_avg$Time <- as.numeric(data7small_avg$Time)
data7small_avg$avgCount <- as.numeric(data7small_avg$avgCount)
data7small_avg$variance <- as.numeric(data7small_avg$variance)
data7small_avg$stdev <- as.numeric(data7small_avg$stdev)
data7small_avg$type <- as.factor(data7small_avg$type)
data7small_avg$n <- as.numeric(data7small_avg$n)
data7small_avg$death_odds <- as.numeric(data7small_avg$death_odds)

data7small_avg %>% 
  #filter(Time <=2000) %>% 
  filter(death_odds <= 1) %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, death_odds), colour=death_odds, linetype=type))+
  geom_line()+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 0.5) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.line = element_line(size = 0.2, 
                                 linetype = "solid"), panel.grid.major = element_line(linetype = "blank"), 
        panel.grid.minor = element_line(linetype = "blank"), 
        axis.title = element_text(family = "mono"), 
        axis.text = element_text(family = "mono"), 
        legend.text = element_text(family = "mono"), 
        legend.title = element_text(family = "mono"), 
        panel.background = element_rect(fill = NA), 
        legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(y = "# of Individuals", colour = "death probability")+
  theme(legend.title = element_text(size = 7)) + theme(legend.text = element_text(size = 6))


#infectious_level

data8 <- CannNetLogoToR("/Users/MJ/GitHub/Cann_ABM_Outputs/infectious_level.csv", 
c(1:8, 10:27), "infectious_level")

data8small_avg <- data8 %>% 
  filter(grepl("[0]$", Time)) %>% 
  group_by(infectious_level, type, Time) %>% 
  summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n())


data8small_avg$Time <- as.numeric(data8small_avg$Time)
data8small_avg$avgCount <- as.numeric(data8small_avg$avgCount)
data8small_avg$variance <- as.numeric(data8small_avg$variance)
data8small_avg$stdev <- as.numeric(data8small_avg$stdev)
data8small_avg$type <- as.factor(data8small_avg$type)
data8small_avg$n <- as.numeric(data8small_avg$n)
data8small_avg$infectious_level <- as.numeric(data8small_avg$infectious_level)

data8small_avg %>% 
  #filter(Time <=2000) %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, infectious_level), colour=infectious_level, linetype=type))+
  geom_line()+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 50) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.line = element_line(size = 0.2, 
                                 linetype = "solid"), panel.grid.major = element_line(linetype = "blank"), 
        panel.grid.minor = element_line(linetype = "blank"), 
        axis.title = element_text(family = "mono"), 
        axis.text = element_text(family = "mono"), 
        legend.text = element_text(family = "mono"), 
        legend.title = element_text(family = "mono"), 
        panel.background = element_rect(fill = NA), 
        legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA)) +labs(y = "# of Individuals", colour = "infectious_level")+
  theme(legend.title = element_text(size = 7)) + theme(legend.text = element_text(size = 6))


# movement_odds

data9 <- CannNetLogoToR("/Users/MJ/GitHub/Cann_ABM_Outputs/movementodds.csv", 
                        c(1:4, 6:27), "movement_odds")

data9small_avg <- data9 %>% 
  filter(grepl("[0]$", Time)) %>% 
  group_by(movement_odds, type, Time) %>% 
  summarise(avgCount = mean(Count), variance=var(Count), stdev=sd(Count), n=n())


data9small_avg$Time <- as.numeric(data9small_avg$Time)
data9small_avg$avgCount <- as.numeric(data9small_avg$avgCount)
data9small_avg$variance <- as.numeric(data9small_avg$variance)
data9small_avg$stdev <- as.numeric(data9small_avg$stdev)
data9small_avg$type <- as.factor(data9small_avg$type)
data9small_avg$n <- as.numeric(data9small_avg$n)
data9small_avg$movement_odds <- as.numeric(data9small_avg$movement_odds)

data9small_avg %>% 
  #filter(Time <=2000) %>% 
  filter(grepl("[.5]$", movement_odds)) %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, movement_odds), colour=movement_odds, linetype=type))+
  geom_line()+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 10) + 
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1),
        axis.line = element_line(size = 0.2,
                                 linetype = "solid"), panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_text(family = "mono"),
        axis.text = element_text(family = "mono"),
        legend.text = element_text(family = "mono"),
        legend.title = element_text(family = "mono"),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA)) +labs(y = "# of Individuals", colour = "movement odds")+
  theme(legend.title = element_text(size = 7)) + theme(legend.text = element_text(size = 6))

