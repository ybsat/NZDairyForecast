library('dplyr')

# Joining events and products from python script_convert.py
events = read.csv('../data/events.csv')
products = read.csv('../data/products.csv')

products = left_join(products, events, by = c("key" = "EventGUID"))

products.to_csv('../data/joineddata.csv')
