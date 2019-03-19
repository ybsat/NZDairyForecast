library("dplyr")
library("ggplot2")
library("zoo")
library("vars")
library("astsa")
library("xts")
library("reshape2")
library("tseries")

# Importing Data
series = read.csv('../data/fullseries.csv')
series$Date <- as.Date(as.character(series$Date))


# Plot the series for all product groups
qplot(series$Date,series$Price, color = series$Product.Type, xlab = "Date", ylab = "Price", geom = c("point","line"))

qplot(series$Date,series)

## TS Visualization

ts_WMP = ts(series$WMP_Price, start = c(2010,13), frequency = 24)

cmp_WMP = decompose(ts_WMP)
plot(cmp_WMP)

# plotting seasonality
qplot(series$Date,cmp_WMP$seasonal, geom=c('line'),xlab='Date',ylab='WMP Seasonal Variation') + ggtitle('Seasonal variation of WMP Price') 

#plotting randomness
qplot(series$Date,cmp_WMP$random, geom=c('line'),xlab='Date',ylab='WMP Random Component') + ggtitle('Random component of WMP Price') 

#plotting original - seasonal and original
qplot(series$Date,cmp_WMP$random, geom=c('line'),xlab='Date',ylab='WMP Random Component') + ggtitle('Random component of WMP Price') 

# plot of random and seasonal in same graph
plot(series$Date,cmp_WMP$random,type="l",col="red",xlab="Date",ylab="Variation",main='Seasonal (blue) and Random (red) Variation')
lines(series$Date,cmp_WMP$seasonal,col="blue")




# AMF
ts_AMF = ts(series$AMF_Price, start = c(2010,13), frequency = 24)

cmp_AMF = decompose(ts_AMF)
plot(cmp_AMF)

# plotting seasonality
qplot(series$Date,cmp_WMP$seasonal, geom=c('line'),xlab='Date',ylab='WMP Seasonal Variation') + ggtitle('Seasonal variation of WMP Price') 

#plotting randomness
qplot(series$Date,cmp_WMP$random, geom=c('line'),xlab='Date',ylab='WMP Random Component') + ggtitle('Random component of WMP Price') 

#plotting original - seasonal and original
qplot(series$Date,cmp_WMP$random, geom=c('line'),xlab='Date',ylab='WMP Random Component') + ggtitle('Random component of WMP Price') 

# plot of random and seasonal in same graph
plot(series$Date,cmp_WMP$random,type="l",col="red",xlab="Date",ylab="Variation",main='Seasonal (blue) and Random (red) Variation')
lines(series$Date,cmp_WMP$seasonal,col="blue")




agg = aggregate(WMP_Price ~ year,data = series, FUN = length)

