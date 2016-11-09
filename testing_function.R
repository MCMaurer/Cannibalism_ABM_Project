setwd("/Users/MJ")
data <- CannNetLogoToR("Github/Cannibalism_ABM_Project/Cannibalism_sensitivity_infected-death-modifier-spreadsheet.csv", 
                       c(1, 3:27), "Inf_Death_Modifier")


data2 <- CannNetLogoToR("Github/Cannibalism_ABM_Project/Cannibalism_sensitivity_inf-fecund-modifier-spreadsheet.csv", 
                        c(1:13, 15:27), "Inf_fecund_modifier")

View(data)

View(data2)

write.csv(data, "Documents/Cannibalism_ABM_Project/InfDeathOutput.csv")


View(data %>% filter(Inf_Death_Modifier == -27))

library(ggplot2)
library(dplyr)
library(tidyr)
library(wesanderson)



data_small <- data %>%
  filter(grepl("[0]$", Time))


datasmall <- data_small[1:838838,]

datasmall <- datasmall %>% 
  filter(grepl("[0]$", Run))

View(datasmall)

View(data_small)

datasmall$Time <- as.numeric(datasmall$Time)
datasmall$Count <- as.numeric(datasmall$Count)
datasmall$type <- as.factor(datasmall$type)

datasmall %>% 
  #filter(Time <= 3000) %>% 
  group_by(Run, type, Inf_Death_Modifier) %>% 
  ggplot(aes(x=Time, y=Count, group=interaction(Run, type, Inf_Death_Modifier), alpha=0.2, colour=Inf_Death_Modifier, linetype=type))+
  geom_line()+
  scale_x_continuous(breaks = seq(0, 10000, 1000))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_color_manual(values = wes_palette("Zissou", 21, type = "continuous"))+
  theme_bw()

## do something to average counts at each time point for each run in a given paramater value, then plot

## this could give a much cleaner plot



datasmall_avg <- data %>% 
  filter(grepl("[0]$", Time)) %>% 
  group_by(Inf_Death_Modifier, type, Time) %>% 
  summarise(avgCount = mean(Count)) 

datasmall_avg$Time <- as.numeric(datasmall_avg$Time)
datasmall_avg$avgCount <- as.numeric(datasmall_avg$avgCount)
datasmall_avg$type <- as.factor(datasmall_avg$type)

View(datasmall_avg)

datasmall_avg %>% 
  ggplot(aes(x=Time, y=avgCount, group=interaction(type, Inf_Death_Modifier), colour=Inf_Death_Modifier, linetype=type))+
  geom_line()+
  #scale_x_continuous(breaks = seq(0, 10000, 1000))+
  #scale_y_continuous(breaks = seq(0, 600, 50))+
  #scale_color_manual(values = wes_palette("Zissou", 21, type = "continuous"))+
  theme_bw()