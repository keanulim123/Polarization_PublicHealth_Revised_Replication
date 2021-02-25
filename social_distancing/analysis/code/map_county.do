*ssc install gtools

/*******************************************************************************
* Construct County-Level Policy Dataset for All Counties
* (to deal with missing in county_day_data.csv
*******************************************************************************/
clear all
import delimited using "input/state_policies.csv", clear
tempfile state_policies
save `state_policies'

import delimited using "input/county_policies.csv", clear
ren county_fips county
tempfile county_policies
save `county_policies'

use STATE COUNTY using "input/county_db.dta", clear
destring STATE, gen(state_fips)
destring COUNTY, gen(county)
replace county = 1e3*state_fips + county
merge m:1 state_fips using `state_policies', nogen keep(1 3)
merge 1:1 county using `county_policies', nogen keep(1 3)
replace effective_date_sip_state = "NA" if effective_date_sip_state==""
replace effective_date_sip_county = "NA" if effective_date_sip_county==""
keep county effective_date_sip_state effective_date_sip_county 
tempfile counties
save `counties'

/*******************************************************************************
* Clean Data
*******************************************************************************/
import delimited using "input/county_day_data.csv", clear
merge m:1 county using `counties', nogen assert(2 3) update replace //Merges on policies for missing counties, asserts that existing policies match

ren date date_str
gen date = date(date_str, "YMD")
format date %td

local pre_week_start = td(27jan2020)
local post_week_start = td(6apr2020)
gen visits_pre  = visits if inrange(date, `pre_week_start',  `pre_week_start' +6)
gen visits_post = visits if inrange(date, `post_week_start', `post_week_start'+6)

summ date
local last_day = r(max)

replace deaths = . if date!=`last_day'
replace cases  = . if date!=`last_day'

replace effective_date_sip_county = "" if effective_date_sip_county=="NA"
replace effective_date_sip_state = "" if effective_date_sip_state=="NA"
ren effective_date_sip_county effective_date_sip_county_str
ren effective_date_sip_state effective_date_sip_state_str
gen effective_date_sip_county = date(effective_date_sip_county_str, "YMD")
gen effective_date_sip_state = date(effective_date_sip_state_str, "YMD")
gen sip_effective_date = min(effective_date_sip_county, effective_date_sip_state)
format effective_date_sip_county effective_date_sip_state sip_effective_date %tdDDmon

gen sip_county_preemption = effective_date_sip_state - sip_effective_date
replace sip_county_preemption = 99 if missing(effective_date_sip_state) & !missing(effective_date_sip_county)
replace sip_county_preemption = . if sip_county_preemption==0

gcollapse (firstnm) r_share deaths cases sip_effective_date sip_county_preemption ///
	(sum) visits_pre visits_post, by(county)

gen rep_pct = 100 * r_share

gen visits_pct_ch   = 100*(visits_post-visits_pre)/visits_pre
*_pctile visits_pre, percentiles(10)
*replace visits_pct_ch = . if visits_pre<=r(r1)

gen fips = county
merge 1:1 fips using "input/county_db", nogen assert(2 3) keepusing(id)

gen STATEFP = substr(string(county, "%05.0f"), 1, 2)
drop if inlist(STATEFP, "60", "66", "69", "72", "78")

/*******************************************************************************
* Produce maps
*******************************************************************************/
format rep_pct visits_pct_ch %12.0f

local nq = 5

local osize = "none"
local osizes = ""
forval i=1/`nq' {
	local osizes = "`osizes' `osize'"
}

tempname state_coord

local spmap_options = `"id(id) osize(`osizes') ndsize(`osize') polygon(data("`state_coord'.dta") osize(medium)) legend(size(4))"'

foreach restr in `"if !inlist(STATEFP,"02","15","72","78")"' {
	preserve
	use "input/state_coord", clear
	gen n = _n
	gen state_id = _ID
	merge m:1 state_id using "input/state_db", nogen assert(3) keepusing(STATEFP)
	drop if inlist(STATEFP, "60", "66", "69", "72", "78")
	if `"`restr'"'!="" keep `restr'
	save "`state_coord'.dta", replace
	restore
	
	local suff = cond(`"`restr'"'=="","_all", ///
		cond(`"`restr'"'==`"if STATEFP=="06""', "_california", ///
			"_continental"))
			
	foreach var of varlist sip_effective_date sip_county_preemption rep_pct visits_pct_ch deaths cases {
		if inlist("`var'", "cases", "deaths") {
			local fcolor = "Oranges"
			local breaks = cond("`var'"=="cases", "0 5 50 500 5000", "0 0.5 5 20 100")
			*_pctile `var', percentiles(80 95 99)
			*forval p=1/`=`nq'-2' {
			*	local breaks = "`breaks' `r(r`p')'"
			*}
			summ `var'
			local breaks = "`breaks' `r(max)'"
			local cl = "clmethod(custom) clbreaks(`breaks')"
		} 
		else {
			local fcolor = cond("`var'"=="sip_county_preemption", "Blues2", "BuYlRd")
			local cl = "clmethod(quantile) clnumber(`nq')"
		}
		
		local ndfcolor = cond("`var'"=="sip_effective_date", "red", "gs12")
		local ndlabel = cond("`var'"=="sip_effective_date", "ndlabel(No identified order)", ///
			cond("`var'"=="sip_county_preemption", "ndlabel(No state/local difference)",   ///
			""))
		local legorder = cond("`var'"=="sip_effective_date", "legorder(lohi)", "")

		spmap `var' using "input/county_coord" `restr', `spmap_options' ///
			 fcolor(`fcolor') `cl' ndfcolor(`ndfcolor') `legorder' `ndlabel'
		graph export "output/map_county`suff'_`var'.pdf", replace
	}
}

rm "`state_coord'.dta"


