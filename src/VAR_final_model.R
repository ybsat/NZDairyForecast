library("dplyr")
library("ggplot2")
library("zoo")
library("vars")
library("astsa")
library("xts")
library("reshape2")
library("tseries")
library("devtools")
library("ggfortify") # import with install_github('sinhrks/ggfortify') after importing devtools
library("forecast")
library("GGally")

# Importing Data
series = read.csv('../data/fullseries.csv')
series$Date <- as.Date(as.character(series$Date))
series = series[series$Date >= as.Date("2013-03-05"),]
series$QtySold = series$WMP_QtySold
series$QtySold[series$Date < as.Date("2014-01-07")] =  series$WMP_QtySold[series$Date < as.Date("2014-01-07")] +
                                                      series$SMP_QtySold[series$Date < as.Date("2014-01-07")] +
                                                      series$AMF_QtySold[series$Date < as.Date("2014-01-07")] +
                                                      series$BMP_QtySold[series$Date < as.Date("2014-01-07")] +
                                                      series$BUT_QtySold[series$Date < as.Date("2014-01-07")]
series = series[,c(-3,-5,-7,-9,-11)]


products = c('AMF','BMP','BUT','SMP','WMP')
freq = 24
freq_days = 15
strt = c(2013,03)
nahead = 20

### Correcting World Price Index from Month to biweekly by averaging intermediate values
rows = nrow(series)
col = 12
#col = 17
for (i in 8:(rows-1)){
  prev_val = series[i-1,col] 
  val = series[i,col]
  next_val = series[i+1,col]
  if (val == prev_val){
    series[i,col] = (prev_val + next_val)/2
  }
}


series_org = series
# scatter plot
ggpairs(series[,c("WMP_Price","QtySold","Participating.Bidders","Winning.Bidders","Rounds","Date")],title = "Correlation plots between key variables",columnLabels = c("WMP Price","Qty Sold","Participation","Winners","Rounds","Date"))

####Removing World Price Index Seasonality
# Plotting World Price Index as is
qplot(series$Date,series$World.Price..US...and.SDRs,xlab = "Date",ylab = "Index Value",geom = c("point","line")) + ggtitle('Dairy Products Commodity Price Index')

#Plotting Time series of WMP before any change
qplot(series$Date,series$WMP_Price,xlab = "Date",ylab = "Price",geom = c("point","line")) + ggtitle('WMP Nominal Price')

ts_WMP_pre = ts(series$WMP_Price, start = strt, frequency = freq)
cmp_WMP_pre = decompose(ts_WMP_pre)
plot(cmp_WMP_pre)

ts_AMF_pre = ts(series$AMF_Price, start = strt, frequency = freq)
cmp_AMF_pre = decompose(ts_AMF_pre)
autoplot(cmp_AMF_pre,labels = c("Seasonal","Trend","Noise")) + ggtitle("AMF Price Time Series Decomposition")

adf.test(series$WMP_Price)


# Normalizing to Real Price
for (prd in products) {
  name = paste(prd,'_Price',sep='')
  series[name] =  series[name] / (series$World.Price..US...and.SDRs / 100)
}

qplot(series$Date,series$WMP_Price,xlab = "Date",ylab = "Price",geom = c("point","line")) + ggtitle('WMP Real Price Normalized by WPI')

# Looking at TS again
ts_WMP_real = ts(series$WMP_Price, start = strt, frequency = freq)
cmp_WMP_real = decompose(ts_WMP_real)
plot(cmp_WMP_real)

adf.test(series$WMP_Price)


# Removing remaining seasonality
seasonal_components = list()
for (i in 1:5) {
  name = paste(products[i],'_Price',sep='')
  temp_ts =  ts(series[name], start = strt, frequency = freq)
  seasonal_components[[i]] = decompose(temp_ts)$seasonal
  series[name] =  series[name] - seasonal_components[i]
}

qplot(series$Date,series$WMP_Price,xlab = "Date",ylab = "Price",geom = c("point","line")) + ggtitle('WMP Final Adjusted Price')

adf.test(series$WMP_Price)




# VAR Model 

VARselect(series[,c(-1,-12)], lag.max = 10, type = c("const", "trend", "both", "none"))
fit = VAR(series[,c(-1,-12)],p=10,type = 'both')
summary(fit)

### ARIMA for Commodity Price Iindex

cutat = which(series$Date == as.Date("2018-11-06"))
rowscut = nrow(series) - cutat

WPIts = ts(series$World.Price..US...and.SDRs[1:cutat],start = c(2013, 03),  deltat = 1/24)
WPIclean = tsclean(WPIts, replace.missing = TRUE, lambda = "auto")
WPI2 = auto.arima(WPIclean)
WPIpredict = forecast(WPI2, h = nahead + rowscut)
#WPIpredict = forecast(WPI2, h = nahead)


forecast_ci = data_frame(Date = adj_pred[1:20,1],lower = WPIpredict$lower[,2], fcst = WPIpredict$mean,upper = WPIpredict$upper[,2])
write.csv(forecast_ci,"/Users/Yahia/Desktop/WPIfuture.csv",row.names = FALSE)

forecasted_WPI = WPIpredict$mean[rowscut:end] / 100

### Predicting
# Seasonality adjusted
prd = predict(fit,n.ahead = nahead)
adj_pred = data.frame(Date = as.Date(character()),
                      Product = character(),
                      fcst = double(),
                      lower = double(),
                      upper = double(),
                      CI = double(),
                      stringsAsFactors = FALSE)

pos = nrow(series) %% freq + 1
to = pos + nahead - 1
for (i in 1:5){
  lastdate = as.Date(series[nrow(series),1])
  prod = products[i]
  seas = seasonal_components[[i]]
  seas = seas[pos:to]
  tmp = prd$fcst[[i]]
  for (j in 1:nahead){
    lastdate = lastdate + freq_days
    correction = seas[j]
    factor = forecasted_WPI[j]
    fcst = (tmp[j,1] + correction) * factor
    lower = (tmp[j,2] + correction) * factor
    upper = (tmp[j,3] + correction) * factor
    CI = upper - fcst
    tmpdf = data.frame(Date = lastdate, Product = prod, fcst = fcst, lower = lower, upper = upper, CI = CI, stringsAsFactors = FALSE)
    adj_pred = rbind(adj_pred,tmpdf)
  }
}

write.csv(series_org,"./data/analysis_series.csv",row.names=FALSE)
write.csv(adj_pred,"./data/predictions.csv",row.names=FALSE)



