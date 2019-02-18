setwd("/Users/wandi/Desktop")
full <- read.csv("transposed-data.csv")
full$Price.SMP[1:4] = mean(full$Price.SMP[-1:-4])
full$PriceIndexPercentageChange.SMP[1:4] = 0
full$QuantitySold.SMP[1:4] = 0
model1 = VAR(full[,c(-1,-2,-9,-10,-11,-12,-13,-14)], p = 2, type = "both")
model2 = VAR(full[-1:-52,c(-1,-2,-9,-10,-11)], p = 2, type = "both")
model3 = VAR(full[-1:-69,c(-1,-2)], p = 2, type = "both")