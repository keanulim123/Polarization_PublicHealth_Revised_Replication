# Some sources used:
# https://kite.com/python/answers/how-to-convert-a-dictionary-to-a-numpy-array-in-python
# https://stackoverflow.com/questions/51186619/convert-list-of-dictionaries-to-dataframe-with-one-column-for-keys-and-one-for-v/51190541

import pandas as pd
import numpy as np
import ast
import csv
import gzip
import zipfile 
import io
import os
import glob
import re
#Get series of 2020 weeks
base_week = "2020-01-27"
times = pd.date_range(base_week, "2020-07-06", freq='W-MON') #Main time series used in estimation
times = times.append(times - pd.DateOffset(weeks=52))           #Add placebo data for main time series, lagged 52 weeks
times = times.append(pd.date_range("2020-01-20", pd.to_datetime(base_week) - pd.DateOffset(weeks=1), freq='W-MON')) #Fill in start of 2020
times = times.astype(str).tolist()

#Load commonly used files
poi_county   = pd.read_csv("output_local/placeid_county.csv", encoding = 'utf-8', dtype={'state_fips': str, 'county': str})
poi_precinct = pd.read_csv("output_local/placeid_precinct.csv", encoding = 'utf-8', dtype={'state_fips': str})
for t in times:
    print(t)
    if t<"2020-06-15":
        parts = ["external/Weekly Patterns/main-file/%s-weekly-patterns.csv.gz" % t]
    else:
        t_dt = pd.to_datetime(t) + pd.DateOffset(days=9) #Delivery date occurs 9 days after start of week
        parts = glob.glob("external/Weekly Patterns Continued/patterns/{delivery_dt}/*/*.csv.gz".format(delivery_dt = t_dt.strftime('%Y/%m/%d')))
        assert len(parts)==4
    temp = pd.DataFrame()
    for part in parts:
        temp = temp.append(pd.read_csv(part, escapechar = "\\", encoding = 'utf-8', compression='gzip'))

    # Get POI visits by day
    temp['visits_by_day'] = temp['visits_by_day'].apply(lambda x: ast.literal_eval(x))
    newvalues = np.dstack((
                    np.repeat(temp.safegraph_place_id.values, list(map(len, temp.visits_by_day))),
                    np.tile(range(1, len(temp.visits_by_day.values[0]) + 1), len(temp.visits_by_day)), 
                    np.concatenate(temp.visits_by_day.values)
    )) # https://stackoverflow.com/questions/53218931/how-to-unnest-explode-a-column-in-a-pandas-dataframe
    temp_daily = pd.DataFrame(data = newvalues[0], columns = ['safegraph_place_id', 'day', 'visits'])
    del temp, newvalues
    
    ### Clean for county
    # Restrict to POIs we mapped to counties.
    temp_county        = temp_daily.merge(poi_county, how = 'inner')

    temp_county['naics_2dig'] = temp_county.naics_code.astype(str).str[:2]
    temp_county['industry']   = "other_industries"
    temp_county.loc[np.logical_or(temp_county.naics_2dig=="44", temp_county.naics_2dig=="45"), 'industry'] = "retail_trade"
    temp_county.loc[temp_county.naics_2dig=="62", 'industry'] = "health_care"
    temp_county.loc[temp_county.naics_2dig=="71", 'industry'] = "entertainment"
    temp_county.loc[temp_county.naics_2dig=="72", 'industry'] = "accomod_and_food"
    
    # Group by county-day
    county_daily_poi_visits = temp_county.groupby(['county', 'day'])[['visits']].sum().reset_index()
    county_daily_poi_visits.to_csv("output_local/county_daily_%s.csv" % t, encoding = 'utf-8', index=False)
    del county_daily_poi_visits
    
    # Group by county-naics-day
    industry_daily_poi_visits =  temp_county.groupby(['county', 'industry', 'day'])[['visits']].sum().reset_index()
    industry_daily_poi_visits.to_csv("output_local/county_industry_daily_%s.csv" % t, encoding = 'utf-8', index=False)
    del industry_daily_poi_visits
    
    ### Clean for precinct POI
    # Restrict to POIs we mapped to precincts
    temp_daily = temp_daily.merge(poi_precinct, how = 'inner')
    # Group by precinct-day
    prec_daily_poi_visits = temp_daily.groupby(['state_fips', 'precinct_id', 'day'])[['visits']].sum().reset_index()
    prec_daily_poi_visits.to_csv("output_local/prec_daily_%s.csv" % t, encoding = 'utf-8', index=False)
    del temp_daily, prec_daily_poi_visits