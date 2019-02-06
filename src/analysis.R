library('dplyr')


events = read.csv('/data/events.csv')
products = read.csv('/data/products.csv')

products = left_join(products, events, by = c("key" = "EventGUID"))

