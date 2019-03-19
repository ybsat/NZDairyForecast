### data cleaning
wmp <- joined[which(joined$ProductGroupCode == "WMP"), -1]
wmp <- wmp[,c(-5,-15,-4,-6,-10,-16)]
names(wmp)[1] <- "Price"
wmp <- wmp[,c(-2,-8)]
names(wmp)[6] <- "AuctionPrice"
names(wmp)[15] <- "12MonthAllProdSold"
names(wmp)[5] <- "12MonthWMPSold"

wmp$EventDate <- as.POSIXlt(as.Date(
  wmp$EventDate,format = "%m/%d/%y"))
for (ii in 1:nrow(wmp)) {
if (wmp$Price[ii] > 0)
  {
  wmp$ProductSold[ii] = TRUE
}}
wmp <- wmp[which(wmp$ProductSold == TRUE),]
wmp <- wmp[,c(-3,-4)]
wmp$EventDuration <- as.character(wmp$EventDuration)
wmp$EventDuration <- strptime(wmp$EventDuration, 
                                format = "%H:%M")
wmp$EventDuration <- wmp$EventDuration$hour*60 + wmp$EventDuration$min
wmp$EventMonth <- wmp$EventDate$mo+1
wmp <- wmp[,-5]
wmp <- na.omit(wmp)

### Analysis
lm0 <- lm(Price ~1,wmp)
lmall <- lm(Price~.,wmp)
step(lm0, scope=list(lower=formula(lm0), 
                     upper = formula(lmall)), 
     direction="both", trace=1)
modelop <- lm(Price ~ AveragePrice_Event + QualifiedBidders + 
     EventMonth + ProductGroup12MSalesSplit + TotalRounds, wmp)
summary(modelop)
pairs(~Price+AveragePrice_Event + QualifiedBidders + 
        EventMonth + ProductGroup12MSalesSplit + TotalRounds, wmp)