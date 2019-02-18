library("dplyr")
library("ggplot2")
library("zoo")
library("vars")
library("astsa")
library("xts")

### Processing of GDT Historic Data
# Loading
hist <- read.csv("/Users/wandi/Desktop/HistoricalProductData.csv")
hist$Date <- as.Date(as.character(hist$Date.of.Event),format = "%d-%b-%y")
hist <- hist[,c(1,3,4,11,12)]
hist <- hist[order(hist$Product.Type,hist$Date),]

names(hist)[3] <- "PriceIndexPercentageChange"
names(hist)[2] <- "Price"
names(hist)[4]  <- "QuantitySold"
hist$Price = as.integer(as.character(hist$Price))
hist$PriceIndexPercentageChange = as.double(as.character(hist$PriceIndexPercentageChange))
hist$QuantitySold = as.integer(as.character(hist$QuantitySold))

#Filtering out early start dates
products <- unique(hist$Product.Type)
for (prd in products){
  strt = min(which(is.na(hist$Price) & (hist$Product.Type == prd)))
  fin = max(which(is.na(hist$Price) & (hist$Product.Type == prd)))
  if (!(strt == Inf) & !(fin == -Inf)) {
    hist = hist[-c(strt:fin),]
  }
}
hist = hist[hist$Date < as.Date('2014-01-07'),]
### Processing of recent auctions data
recent <- read.csv("/Users/wandi/Desktop/joineddata.csv")
recent <- recent[,c(-1,-6,-7,-11,-16,-17)]
names(recent)[1] <- "Price"
names(recent)[4] <- "Product.Type"
recent$Date <- as.Date(recent$EventDate,format = "%m/%d/%y")
recent = recent[order(recent$Product.Type,recent$Date),]

recent$Product.Type = as.character(recent$Product.Type)
recent$Product.Type[recent$Product.Type == "Butter"] = "BUT"
recent = recent[recent$Product.Type %in% products,]
recent$Product.Type = as.factor(recent$Product.Type)
recent$Price[is.na(recent$Price)] = recent$AveragePublishedPrice.1[is.na(recent$Price)]

## Generation of series data
recent_series = recent[,c(4,1,2,16,20)]
series = rbind(recent_series,hist)
series = series[order(series$Product.Type,series$Date),]
series$PriceIndexPercentageChange[2:length(series$PriceIndexPercentageChange)] = 
  (series$Price[2:length(series$Price)] - series$Price[1:length(series$Price)-1])/series$Price[1:length(series$Price)-1]



##Transpose starts
AMF <- series[which(series$Product.Type=="AMF"),2:5]
names(AMF)[1:3] <- c("Price.AMF", "PriceIndexPercentageChange.AMF","QuantitySold.AMF")
BMP <- series[which(series$Product.Type=="BMP"),2:5]
names(BMP)[1:3] <- c("Price.BMP", "PriceIndexPercentageChange.BMP","QuantitySold.BMP")
BUT <- series[which(series$Product.Type=="BUT"),2:5]
names(BUT)[1:3] <- c("Price.BUT", "PriceIndexPercentageChange.BUT","QuantitySold.BUT")
SMP <- series[which(series$Product.Type=="SMP"),2:5]
names(SMP)[1:3] <- c("Price.SMP", "PriceIndexPercentageChange.SMP","QuantitySold.SMP")
WMP <- series[which(series$Product.Type=="AMF"),2:5]
names(WMP)[1:3] <- c("Price.WMP", "PriceIndexPercentageChange.WMP","QuantitySold.WMP")

testmerged <- merge(WMP,SMP,by = "Date", all = TRUE)
testmerged <- merge(testmerged, BUT, by = "Date", all = TRUE)
testmerged <- merge(testmerged, BMP, by = "Date", all = TRUE)
testmerged <- merge(testmerged, AMF, by = "Date", all = TRUE)
####### transpose ends

# Adding World Price Indices
index = read.csv("./data/price_indeces.csv")
index = index[,c(-3)]
index$Date = as.yearmon(index$Date,"%b %y")
index$ind_change = 0
index$ind_change[2:length(index$Date)] = (index$World.Price..US...and.SDRs[2:length(index$Date)] - 
                                            index$World.Price..US...and.SDRs[1:length(index$Date)-1]) / (index$World.Price..US...and.SDRs[1:length(index$Date)-1])
series$yearmon = as.yearmon(series$Date)

series = left_join(series,index, by = c("yearmon" = "Date"))
series = series[,-c(6)]
series = series[order(series$Date),]
lastindex = series$World.Price..US...and.SDRs[min(which(is.na(series$World.Price..US...and.SDRs))) - 1]
series$World.Price..US...and.SDRs[is.na(series$World.Price..US...and.SDRs)] = lastindex
series$ind_change[is.na(series$ind_change)] = 0
series = series[order(series$Product.Type,series$Date),]


# Plot the series for all product groups
qplot(series$Date,series$Price, color = series$Product.Type, xlab = "Date", ylab = "Price", geom = c("point","line"))


## TS Visualization
series_sub = series[series$Product.Type == "WMP",]

tsData = ts(series_sub$Price, start = c(2009,11,3), frequency = 26)
components.ts = decompose(tsData)
plot(components.ts)

## Vector Series

testfit = VAR(series[series$Product.Type == 'AMF',c(2,3,6,7)],p=2,type = 'both') # shown to Zhaoran
#summary(testfit)

#vecotrized
series_vec




# making stationary
series_sub$diff = 0
series_sub$diff[2:length(series_sub$Price)] = series_sub$Price[2:length(series_sub$Price)] - series_sub$Price[1:length(series_sub$Price)-1]





#joined$Duration <- strptime(joined$EventDuration, format = "%H:%M")
#joined$Duration <- joined$Duration$hour*60 + joined$Duration$min
#joined <- joined[,-c(10,11)]
#joined <- joined[,c(4,1,2,18,12,3,6,7,8,9,10,11,13,14,15,16,17,19)]



#toadd = names(joined[,c(6:18)])
#for (i in 1:length(toadd)) {
#  n = ''
#  hist = cbind(hist,n)
#  names(hist)[ncol(hist)] <- toadd[i]
#}
#merged = rbind(joined,hist)