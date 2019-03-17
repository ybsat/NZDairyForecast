library(tseries)
library(ggplot2)
library(forecast)
full = read.csv("fullseries.csv")
WPI = as.data.frame(full[,18])
names(WPI)[1] = "WPI"
WPIts = ts(WPI$WPI,start = c(2010, 03), end = c(2019, 01), deltat = 1/12)
WPImodel = arima(WPIts, order = c(1,0,2),
             seasonal = list(order=c(1,0,1)))
WPIpredict = forecast(WPImodel, h = 12)
