getwd() # get working directory
setwd("/Users/karinaveikos/Desktop/IE Senior Design/NZDairyForecast/data")

test = read.csv("price_indeces.csv", header=T)
head(test)

price_indeces = test

rm(test)

############################################ not important###############
as.Date("02JAN1970", "%d%b%Y")

as.Date("JAN1986", "%b%Y")

as.Date(paste("15", price_indeces$Date[1]), "%d %b%y")

x = c(rep(15,395))
matrix(c(rep(15,395)), nrow = 395)
x = matrix(c(rep(15,395)), nrow = 395)

price_indeces$Date = (paste("15", price_indeces$Date))

?cor

library(car)

scatterplotMatrix(price_indeces[-1])
#scatter plot of world prices and newe zealand dollars

# trivial regression on time.
length(price_indeces$Date)
# there are 395
time = c()
i = 0
for(date in price_indeces$Date){
  time = c(time, i)
  i = i + 1
}
length(time)
# Fit a linear regression of the money on time 
money = price_indeces$World.Price..US...and.SDRs
length(money)
fit = lm(money ~ time)
summary(fit)

#normalizing the data
log(price_indeces$World.Price..US...and.SDRs)

scatterplotMatrix(log(price_indeces[-1]))

test = read.csv("CleanEvents.csv", header = T)
head(test)

CleanEvents.csv = test
CleanEvents = test
rm(test)
rm(CleanEvents.csv)


as.Date(price_indeces$Date[1], "%d %b %y")
months(as.Date(price_indeces$Date[1], "%d %b %y"))

as.Date(CleanEvents$EventDate[1], "%d/%m/%y")
months(as.Date(CleanEvents$EventDate[1], "%d/%m/%y"))


# for ref
#year <- as.numeric(format(date,'%Y'))
merge (CleanEvents, price_indeces, by.x = c( months(as.Date(CleanEvents$EventDate, "%d/%m/%y")), year(as.Date(CleanEvents$EventDate, "%d/%m/%y"))), by.y = c( months(as.Date(price_indeces$Date, "%d %b %y")), year(as.Date(price_indeces$Date, "%d %b %y"))), all.x=T)


get_month_ce = function(date){
  return(months(as.Date(date, "%d/%m/%y")))
}

get_month_pe = function(date){
  return(months(as.Date(date, "%d %b %y")))
}
library(dplyr)
#left_join(test_data, kantrowitz, by = c("first_name" = "name"))
clean_ev2 = CleanEvents
clean_ev2$date2 = get_month_ce(clean_ev2$EventDate)
######################################## Now this is important ############

#Regression Time
test = read.csv("CleanEvents&PriceIndeces.csv", header = T)
head(test)
CleanEvents_PriceIndeces = test
rm(test)

# Regression 1 of auction price on world index

fit1 <- lm(AveragePublishedPrice ~ log(World.Price..US...and.SDRs), CleanEvents_PriceIndeces)
summary(fit1)
plot(log(CleanEvents_PriceIndeces$World.Price..US...and.SDRs), CleanEvents_PriceIndeces$AveragePublishedPrice)
abline(fit1)





