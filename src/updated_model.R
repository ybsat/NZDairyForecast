setwd("/Users/wandi/Desktop")
full <- read.csv("fullfull.csv")
full$Price.SMP[1:4] = mean(full$Price.SMP[-1:-4])
full$PriceIndexPercentageChange.SMP[1:4] = 0
full$QuantitySold.SMP[1:4] = 0
model1 = VAR(full[,c(-1,-2,-9,-10,-11,-12,-13,-14)], p = 2, type = "both")
model2 = VAR(full[-1:-52,c(-1,-2,-9,-10,-11)], p = 2, type = "both")
model3 = VAR(full[-1:-69,c(-1,-2)], p = 2, type = "both")

### Model from March 8
## lag 2 model for only prices
library(dplyr)
full <- read.csv("fullfull.csv")
full <- full[,-1:-2]
results <- full[,c(1,2,4,6,8,10)]
results = mutate(results, AMF_lag1 = lag(AMF_Price), AMF_lag2 = lag(AMF_lag1),
                 BMP_lag1 = lag(BMP_Price),  BMP_lag2 = lag(BMP_lag1),
                 BUT_lag1 = lag(BUT_Price),  BUT_lag2 = lag(BUT_lag1),
                 SMP_lag1 = lag(SMP_Price),  SMP_lag2 = lag(SMP_lag1),
                 WMP_lag1 = lag(WMP_Price),  WMP_lag2 = lag(WMP_lag1))
results <- results[-1:-2,]
products = c("AMF","BMP","BUT","SMP","WMP")
model_priceAMF= lm(log(AMF_Price) ~ log(AMF_lag1) + log(AMF_lag2), results)
model_priceBMP= lm(log(BMP_Price) ~ log(BMP_lag1) + log(BMP_lag2), results)
model_priceBUT= lm(log(BUT_Price) ~ log(BUT_lag1) + log(BUT_lag2), results)
model_priceSMP= lm(log(SMP_Price) ~ log(SMP_lag1) + log(SMP_lag2), results)
model_priceWMP= lm(log(WMP_Price) ~ log(WMP_lag1) + log(WMP_lag2), results)

results = mutate(results, AMF_lag3 = lag(AMF_lag2))
tt = results[-1,]
model_AMF3lags= lm(log(AMF_Price) ~ log(AMF_lag1) + log(AMF_lag2)
                   + log(AMF_lag3), tt)


## Model with lag2 prices of all products and other variables
results <- full[,c(1,2,4,6,8,10)]
results = mutate(results, AMF_lag1 = lag(AMF_Price), AMF_lag2 = lag(AMF_lag1),
                 BMP_lag1 = lag(BMP_Price),  BMP_lag2 = lag(BMP_lag1),
                 BUT_lag1 = lag(BUT_Price),  BUT_lag2 = lag(BUT_lag1),
                 SMP_lag1 = lag(SMP_Price),  SMP_lag2 = lag(SMP_lag1),
                 WMP_lag1 = lag(WMP_Price),  WMP_lag2 = lag(WMP_lag1))

results = mutate(results, Bidders_lag1 = lag(full$Participating.Bidders),
                 Bidders_lag2 = lag(Bidders_lag1),
                 Rounds_lag1 = lag(full$Rounds), Rounds_lag2 = lag(Rounds_lag1),
                 WP_lag1 = lag(full$World.Price..US...and.SDRs), WP_lag2 = lag(WP_lag1))


results = results[-1:-2,]
model_allAMF= lm(log(AMF_Price) ~ log(AMF_lag1) + log(AMF_lag2) + Bidders_lag1
                 + Bidders_lag2 + Rounds_lag1 + Rounds_lag2 + WP_lag1
                 +WP_lag2, results)
model_allBMP= lm(log(BMP_Price) ~ log(BMP_lag1) + log(BMP_lag2) + Bidders_lag1
                   + Bidders_lag2 + Rounds_lag1 + Rounds_lag2 + WP_lag1
                   +WP_lag2, results)
model_allBUT= lm(log(BUT_Price) ~ log(BUT_lag1) + log(BUT_lag2) + Bidders_lag1
                   + Bidders_lag2 + Rounds_lag1 + Rounds_lag2 + WP_lag1
                   +WP_lag2, results)
model_allSMP= lm(log(SMP_Price) ~ log(SMP_lag1) + log(SMP_lag2) + Bidders_lag1
                   + Bidders_lag2 + Rounds_lag1 + Rounds_lag2 + WP_lag1
                   +WP_lag2, results)
model_allWMP= lm(log(WMP_Price) ~ log(WMP_lag1) + log(WMP_lag2) + Bidders_lag1
                   + Bidders_lag2 + Rounds_lag1 + Rounds_lag2 + WP_lag1
                   +WP_lag2, results)

## Step Function for lag 2 all
WMP <- results[,-1:-5]
SMP <- results[,c(-1,-2,-3,-4,-6)]
AMF <- results[,c(-1,-3,-4,-5,-6)]
BMP <- results[,c(-1,-2,-4,-5,-6)]
BUT <- results[,c(-1,-2,-3,-5,-6)]
lm0 <- lm(WMP_Price~1, WMP)
lmall <- lm(WMP_Price~.,WMP)
step(lm0, scope=list(lower=formula(lm0), 
                     upper = formula(lmall)), 
     direction="both", trace=1)

lm0 <- lm(SMP_Price~1, SMP)
lmall <- lm(SMP_Price~.,SMP)
step(lm0, scope=list(lower=formula(lm0), 
                     upper = formula(lmall)), 
     direction="both", trace=1)

lm0 <- lm(BUT_Price~1, BUT)
lmall <- lm(BUT_Price~.,BUT)
step(lm0, scope=list(lower=formula(lm0), 
                     upper = formula(lmall)), 
     direction="both", trace=1)

lm0 <- lm(BMP_Price~1, BMP)
lmall <- lm(BMP_Price~.,BMP)
step(lm0, scope=list(lower=formula(lm0), 
                     upper = formula(lmall)), 
     direction="both", trace=1)

lm0 <- lm(AMF_Price~1, AMF)
lmall <- lm(AMF_Price~.,AMF)
step(lm0, scope=list(lower=formula(lm0), 
                     upper = formula(lmall)), 
     direction="both", trace=1)

#### 3/15
library(lubridate)
results$mon <- month(as.Date(results$Date, format = "%Y-%m-%d"))
demeaned = results[,1:6]
#demean each products price (mean = the average price of the month
# of past years)
for (ii in 2:6) {
  for (jj in 1:205) {
    mn = mean(results[which(results$mon == results$mon[jj]),ii])
    demeaned[jj, ii] = demeaned[jj, ii] - mn
  }
}
# did not use the part above

## need to go back to run line 36 (re-define results)
results = mutate(results, bidders = full$Participating.Bidders,
                 rounds = full$Rounds, WP = full$World.Price..US...and.SDRs)
results = results[-1:-2,]

SMPWMP <- results[,c(5,6,7)]
BUTAMF <- results[,c(2,4)]
onlyBMP <- results[,c(3,9)]

library(MASS)
library(zoo)
library(sandwich)
library(lmtest)
library(vars)
modelSW = VAR(SMPWMP, p = 2, type = "both")
summary(modelSW)
modelBA = VAR(BUTAMF, p = 2, type = "both")
summary(modelBA)
modelBMP = VAR(onlyBMP, p = 2, type = "both")
summary(modelBMP)
## tried to add seasonality but it's not significant
