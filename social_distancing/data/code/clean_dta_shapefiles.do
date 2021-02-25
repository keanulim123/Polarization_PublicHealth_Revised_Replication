clear all
local scale_y = 1.2 //Initial ratio is off, stretch y coordinate values by this factor

**
** State Shapefiles
**
tempfile state_coord
shp2dta using "input/state_shapefiles/cb_2018_us_state_500k", ///
	database("output/state_db") coordinates(`state_coord') ///
	genid(state_id) replace
	
use `state_coord', clear
replace _Y = _Y*`scale_y'
save "output/state_coord", replace

**
** County Shapefiles
**
clear all
tempfile county_db county_coord

* Convert shapefiles to dta
shp2dta using "input/county_shapefiles/gz_2010_us_050_00_20m", ///
	database(`county_db') coordinates(`county_coord') genid(id) replace

use `county_coord', clear
replace _Y = _Y*`scale_y'
save "output/county_coord", replace

* Add on numeric county fips field (fips)
use `county_db', clear
gen fips = STATE + COUNTY
destring fips, replace
save "output/county_db", replace
