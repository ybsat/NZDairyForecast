# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import json
import pandas as pd


with open("trading_data.json", "r") as read_file:
    data = json.load(read_file)
    
    
row = data[0]

temp = {k: [v] for k, v in row['EventSummary'].items()}
events = pd.DataFrame.from_dict(temp)
events = events.drop(events.index[0])

prdrows = row['ProductGroupResult']
subrow = prdrows[0]
temp = {k: [v] for k, v in subrow.items()}
prdgrps = pd.DataFrame.from_dict(temp)
prdgrps['key'] = ''
prdgrps = prdgrps.drop(prdgrps.index[0])
    
    
for row in data:
    key = row['key']
    temp = {k: [v] for k, v in row['EventSummary'].items()}
    event = pd.DataFrame.from_dict(temp)
    events = pd.concat([events, event], ignore_index=True)
    for prd in row['ProductGroupResult']:
        temp = {k: [v] for k, v in prd.items()}
        prdgrp = pd.DataFrame.from_dict(temp)
        prdgrp['key'] = key
        prdgrps = pd.concat([prdgrps,prdgrp], ignore_index=True)      
 
prdgrps.to_csv('products.csv')
events.to_csv('events.csv')       