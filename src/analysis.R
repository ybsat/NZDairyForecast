library('dplyr')


events = read.csv('events.csv')
products = read.csv('products.csv')

products = left_join(products, events, by = c("key" = "EventGUID"))

