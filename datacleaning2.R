
load("./data/newdata.RData")

#preprocessing

#data$Date = as.Date(data$Date)
data$ProductSold[!is.na(data$Price)] = TRUE
data$ProductSold[is.na(data$Price)] = FALSE
data = data[order(data$Date),]
data$Product.Type = as.character(data$Product.Type)

products = c('WMP','SMP','AMF','BUT','BMP')
data = data[data$Product.Type %in% products,]

