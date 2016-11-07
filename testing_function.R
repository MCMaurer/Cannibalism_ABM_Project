setwd("/Users/MJ")
data <- CannNetLogoToR("Github/Cannibalism_ABM_Project/Cannibalism_sensitivity_infected-death-modifier-spreadsheet.csv", 
                       c(1, 3:27), "Inf_Death_Modifier")

View(data[1:50,])


data2 <- CannNetLogoToR("Github/Cannibalism_ABM_Project/Cannibalism_sensitivity_inf-fecund-modifier-spreadsheet.csv", 
                        c(1:13, 15:27), "Inf_Fecund_modifier")



View(data[1:50,])

library(ggplot2)
library(dplyr)
library(tidyr)



data_small <- data %>%
  filter(grepl("[0]$", Time))

data_small %>% 
  group_by(Run, Time, type, Inf_Death_Modifier) %>% 
  ggplot(aes(x=Time, y=Count, group=Run))+
  geom_line(aes(colour=Inf_Death_Modifier))

  
View(head(data_small))
  
?seq

?geom_line
