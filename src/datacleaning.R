library("dplyr")
library("ggplot2")
library("zoo")
library("vars")
library("astsa")
library("xts")
library("reshape2")
library("tseries")

### Processing of GDT Historic Product Data
# Loading
hist <- read.csv("../data/HistoricalProductData.csv")
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

### Processing of Recent Auction Product Data
recent <- read.csv("../data/joineddata.csv")
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



# Converting to wide format
series = series[order(series$Date),]
eventdates = unique(series$Date)

wide = data.frame("Date" = eventdates)
for (prd in products) {
  sub = series[series$Product.Type == prd,c(5,2,4)]
  names(sub)[2] <- paste(as.character(prd),"_Price",sep='')
  names(sub)[3] = paste(as.character(prd),"_QtySold",sep='')
  wide = left_join(wide,sub)
} 

wide = wide[order(wide$Date),]
wide = wide[wide$Date >= as.Date("2010-03-02"),]

# importing historical event summary
hist_event <- read.csv("../data/HistoricEventSummary.csv")
hist_event$Date <- as.Date(as.character(hist_event$Date),format = "%d-%b-%y")
hist_event$Duration <- as.character(hist_event$Duration)
hist_event$Duration <-  as.integer(substr(hist_event$Duration,1,regexpr(':',hist_event$Duration)[1]-1)) * 60 + 
                    as.integer(substr(hist_event$Duration,regexpr(':',hist_event$Duration)[1]+1,10)) 
hist_event = hist_event[hist_event$Date < as.Date('2014-01-07'),]

# processing recent event summary
recent_event = read.csv("../data/events.csv")
recent_event$Date = as.Date(recent_event$EventDate,format = "%m/%d/%y")
recent_event = recent_event[,c(17,12,11,16,14,5)]
names(recent_event) <- names(hist_event)
recent_event$Duration = as.character(recent_event$Duration)
recent_event$Duration <-  as.integer(substr(recent_event$Duration,1,regexpr(':',recent_event$Duration)[1]-1)) * 60 + 
  as.integer(substr(recent_event$Duration,regexpr(':',recent_event$Duration)[1]+1,10)) 

# combining event summaries and joinining to wide data
events = rbind(hist_event,recent_event)
events = events[order(events$Date),]

wide = left_join(wide, events)


# Adding World Price Indices
index = read.csv("../data/price_indeces.csv")
index = index[,c(-3)]
index$Date = as.yearmon(index$Date,"%b %y")
index$ind_change = 0
index$ind_change[2:length(index$Date)] = (index$World.Price..US...and.SDRs[2:length(index$Date)] - 
    index$World.Price..US...and.SDRs[1:length(index$Date)-1]) / (index$World.Price..US...and.SDRs[1:length(index$Date)-1])

wide$yearmon = as.yearmon(wide$Date)


wide = left_join(wide,index, by = c("yearmon" = "Date"))
wide = wide[,-c(17)]


lastindex = wide$World.Price..US...and.SDRs[min(which(is.na(wide$World.Price..US...and.SDRs))) - 1]
wide$World.Price..US...and.SDRs[is.na(wide$World.Price..US...and.SDRs)] = lastindex
wide$ind_change[is.na(wide$ind_change)] = 0

wide = wide[,c(-18)]

write.csv(wide,"../data/fullseries.csv",row.names=FALSE)

