hist <- read.csv("HistoricalData.csv")
hist$Date <- as.POSIXlt(as.Date(hist$Date.of.Event, format = "%d %B %y"))
hist <- hist[,-2]
names(hist)[2] <- "Price"
hist <- hist[,c(-7,-8,-9)]
joined <- read.csv("joineddata.csv")
names(joined)
joined <- joined[,c(-1,-6,-7,-11,-16,-17)]
names(joined)[1] <- "Price"
names(joined)[4] <- "Product.Type"
names(hist)[3] <- "PriceIndexPercentageChange"
names(hist)[7] <- "QuantitySold"
joined$Date <- as.POSIXlt(as.Date(joined$EventDate,format = "%m/%d/%y"))
joined <- joined[,-10]
joined$Duration <- strptime(joined$EventDuration, format = "%H:%M")
joined$Duration <- joined$Duration$hour*60 + joined$Duration$min
joined <- joined[,-11]

joinednew <- joined[,c(4,1,2,5,18,12,3,6,7,8,9,10,11,13,14,15,16,17,19)]
names(hist)[7] <- "ProductSold"
hist <- hist[,c(1,2,3,7,8)]

toadd = names(joinednew[,c(6:19)])
for (i in 1:length(toadd)) {
  n = ''
  hist = cbind(hist,n)
  names(hist)[ncol(hist)] <- toadd[i]
}
hist <- hist[,-7:-25]
merged = rbind(joinednew,hist)