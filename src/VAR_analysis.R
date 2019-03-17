## Vector Series

testfit = VAR(series[series$Product.Type == 'AMF',c(2,3,6,7)],p=2,type = 'both') # shown to Zhaoran

testfit = VAR(wide[wide$Date >= as.Date("2013-02-19"),c(-1)],p=2,type = 'both')
testfit = VAR(wide[(wide$Date >= as.Date("2013-02-19")) & (wide$Date < as.Date("2018-10-02")),c(-1)],p=2,type = 'both')
s = predict(testfit)

#summary(testfit)



# making stationary
series_sub$diff = 0
series_sub$diff[2:length(series_sub$Price)] = series_sub$Price[2:length(series_sub$Price)] - series_sub$Price[1:length(series_sub$Price)-1]
