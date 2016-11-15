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

data3small_avg %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, Inf_death_modifier), colour=Inf_death_modifier))+
  geom_line()+#aes(alpha=0.9, size=(n/10)))+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_manual(values = colorspace::diverge_hcl(n=21))+
  theme_bw()


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

data4small_avg %>% 
  filter(Time <= 3000) %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, maturation_time), colour=maturation_time, linetype=type))+
  geom_line()+#aes(alpha=0.9, size=(n/10)))+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 300)+
  theme_bw()

?scale_color_gradient2
