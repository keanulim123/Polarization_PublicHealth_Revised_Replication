# Resources used
# https://gis.stackexchange.com/questions/113799/how-to-read-a-shapefile-in-python
# https://stackoverflow.com/questions/30405652/how-to-find-which-points-intersect-with-a-polygon-in-geopandas
# https://gis.stackexchange.com/questions/224496/creating-spatial-join-between-points-and-polygons-in-geopandas
# https://stackoverflow.com/questions/41898561/pandas-transform-a-dbf-table-into-a-dataframe
# https://gis.stackexchange.com/questions/306218/creating-missing-shx-file
# https://stackoverflow.com/questions/56961270/geopandas-spatial-join-attributeerror-nonetype-object-has-no-attribute-bou

from __future__ import division
import pandas as pd
import geopandas
import os
import us
import glob
import re
from simpledbf import Dbf5

## Load and Clean County Shapefile
county_shape = geopandas.read_file('input/county_shapefiles/gz_2010_us_050_00_20m.shp')
county_shape['county'] = county_shape['STATE'] + county_shape['COUNTY']
county_shape.rename(columns={'STATE': 'state_fips'}, inplace = True)

## CSR Conversion of county shapefile (QGIS says it is EPSG:4269 - NAD83 - degrees originally)
county_shape = county_shape.to_crs("EPSG:4326")

## Load safegraph POI Data 
df = pd.DataFrame()
parts = glob.glob("external/Core POI/core_poi-part*.csv.gz")
for part in parts:
	df = df.append(pd.read_csv(part, escapechar = "\\", encoding = 'utf-8', compression = 'gzip'))

## Convert safegraph to shapefile (From https://docs.safegraph.com/docs/places-manual, SafeGraph uses EPSG:4326.)
safe_shape     = geopandas.GeoDataFrame(df, geometry = geopandas.points_from_xy(df.longitude, df.latitude))
safe_shape.crs = "EPSG:4326"
del df

## Construct spatial join
if not county_shape.crs == safe_shape.crs: # Checks that crs are same
    exit()

county_cols = ['state_fips', 'county']
joined_data = geopandas.sjoin(safe_shape, county_shape[county_cols + ['geometry']], op = "within", how = "inner")  

## Save
safe_cols = ['safegraph_place_id', 'naics_code', 'latitude', 'longitude']
joined_data = joined_data[county_cols + safe_cols]
joined_data.to_csv("output_local/placeid_county.csv", encoding = 'utf-8', index = False)

## Save share matched
share_matched = float(joined_data.shape[0]) / safe_shape.shape[0]
with open("output/share_county_matched.txt", 'w') as f:
    f.write("<tab:share_county_matched>\n")
    f.write("{sh}".format(sh = share_matched))
