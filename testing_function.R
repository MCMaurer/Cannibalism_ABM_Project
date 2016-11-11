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
  geom_point(aes(alpha=0.2))+#, size=1/(2*n)))+
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
  geom_point(aes(alpha=0.2))+#, size=1/n))+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_manual(values = colorspace::diverge_hcl(n=21))+
  theme_bw()