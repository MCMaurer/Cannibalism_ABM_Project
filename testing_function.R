data <- CannNetLogoToR("Documents/Cannibalism_ABM_Project/Cannibalism_sensitivity_infected-death-modifier-spreadsheet.csv", 
                       c(1, 3:27))

View(data[1:50,])


data2 <- CannNetLogoToR("Github/Cannibalism_ABM_Project/Cannibalism_sensitivity_inf-fecund-modifier-spreadsheet.csv", 
                        c(1:13, 15:27))

View(data2[1:50,])
