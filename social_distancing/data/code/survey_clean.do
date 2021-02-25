********************************************************************************
********** Survey Data Cleaning **********
********************************************************************************

******* Get data for merge *******

***** Participant list from CloudResearch email *****
import delimited "input/cloudresearch_participants.csv", clear

gen aid = lower(ïaid)

tempfile participants
save `participants', replace

**************************************************


***** Zip codes *****
import delimited "input/zip_code_info.csv", clear


** drop duplicates
sort zipcode
quietly by zipcode: gen duplicate = cond(_N==1,0,_n)
drop if duplicate > 1

tempfile zips_temp
save `zips_temp', replace

**************************************************

***** County FIPS, cases, and deaths *****
import delimited "input/nyt_covid_us_counties.csv", clear


rename state state_name
rename county county_name
gen county = mod(fips,1000)
gen state = (fips - county)/1000


merge m:m state county using `zips_temp', nogen		// merge zipcode data

replace state = 36 if state_name == "New York"	 // NYC reported as city, not counties
replace county = 997 if state == 36 & (county_name == "New York City" | ///
	county == 5 | county == 47 | county == 61 | county == 81 | county == 85)
	

gen cases_temp = 0
replace cases_temp = cases if date == "2020-03-29"
replace cases_temp = cases_temp + cases if date == "2020-03-30"
replace cases_temp = cases_temp + cases if date == "2020-03-31"
replace cases_temp = cases_temp + cases if date == "2020-04-01"
replace cases_temp = cases_temp + cases if date == "2020-04-02"
replace cases_temp = cases_temp + cases if date == "2020-04-03"
replace cases_temp = cases_temp + cases if date == "2020-04-04"
bysort state county: egen cases_week = max(cases_temp)

replace cases_week = 0 if cases_week == .

gen log_cases_week = log(cases_week + 1)


gen deaths_temp = 0
replace deaths_temp = deaths if date == "2020-03-29"
replace deaths_temp = deaths_temp + deaths if date == "2020-03-30"
replace deaths_temp = deaths_temp + deaths if date == "2020-03-31"
replace deaths_temp = deaths_temp + deaths if date == "2020-04-01"
replace deaths_temp = deaths_temp + deaths if date == "2020-04-02"
replace deaths_temp = deaths_temp + deaths if date == "2020-04-03"
replace deaths_temp = deaths_temp + deaths if date == "2020-04-04"
bysort state county: egen deaths_week = max(deaths_temp)
replace deaths_week = 0 if deaths_week == .

gen log_deaths_week = log(deaths_week + 1)

replace log_cases_week = . if date != "2020-04-04"
replace log_deaths_week = . if date != "2020-04-04"

sort zipcode
quietly by zipcode: gen duplicate_zipcode = cond(_N==1,0,_n)
drop if duplicate_zipcode > 1

tempfile cases_temp
save `cases_temp', replace


**************************************************

******* Clean and merge survey data *******

import delimited "input/cloudresearch_4-7-20_raw.csv", clear


** fix one unmatched subject from below. Incorrect form of AID, but same demographics
replace aid = "5e88d5ba-1dd6-0a47-b079-5dbb000ed94d" if aid == "2KJKZj0h8B4YSBQqYSSwSH"

** merge cloudresearch list
merge m:m aid using `participants', keepusing(gender2) ///
nogen // many due to 2 subjects with same AID; these subjects have diff # children and edu

** drop if participants not in both lists: these were test responses
drop if gender2 == . | gender == ""  // in other words, drop if aid not in both files


***** Variables for weighting *****

gen female = (gender == "Female")
gen male = (gender == "Male")

replace age = 23 if abs(age - .23) < .001  // one miscoded response

** age categories used for weights
gen age_low = (age < 40)
gen age_mid = (age >= 40 & age < 60)
gen age_high = (age >= 60)

encode race, gen(race_dummy)
gen white = (race == "White (Not Hispanic or Latinx)")
gen black = (race == "Black or African American (Not Hispanic or Latinx)")
gen latinx = (race == "Hispanic or Latinx")
gen asian = (race == "Asian or Pacific Islander")
gen other_race = 1 - white - black - latinx - asian

** party_cont is the 7-point measure of party
gen party_cont = .
replace party_cont = 0 if party == "Democrat (Strongly Democratic)"
replace party_cont = 1/6 if party == "Democrat (Weakly Democratic)"
replace party_cont = 1/3 if party == "Independent (Lean toward the Democratic Party)"
replace party_cont = 1/2 if party == "Independent (Do not lean towards either party)" | party == "Other / prefer not to say"
replace party_cont = 2/3 if party == "Independent (Lean toward the Republican Party)"
replace party_cont = 5/6 if party == "Republican (Weakly Republican)"
replace party_cont = 1 if party == "Republican (Strongly Republican)"

** party ID used for weights
gen democrat = (party == "Democrat (Strongly Democratic)" | party == "Democrat (Weakly Democratic)")
gen republican = (party == "Republican (Strongly Republican)" | party == "Republican (Weakly Republican)")
gen independent = (party == "Independent (Lean toward the Democratic Party)" | ///
	party == "Independent (Do not lean towards either party)" | ///
	party == "Independent (Lean toward the Republican Party)")
		

***** Other variables *****

gen children = .
replace children = 0 if q_children == "0"
replace children = 1 if q_children == "1"
replace children = 2 if q_children == "2"
replace children = 3 if q_children == "3"
replace children = 4 if q_children == "4"
replace children = 5 if q_children == "5 or more"
drop q_children

** translate education answers to years
gen edu = .
replace edu = 11 if q_edu == "Less than a high school diploma"
replace edu = 12 if strpos(q_edu, "High school diploma or equivalent") != 0
replace edu = 14 if q_edu == "Some college but no degree"
replace edu = 14 if q_edu == "Associate's degree"
replace edu = 16 if q_edu == "Bachelor's degree"
replace edu = 18 if strpos(q_edu, "Graduate degree") != 0
drop q_edu


gen income = .
replace income = 0 if q_inc == "I did not earn income in 2019"
replace income = 5 if q_inc == "$1 to $9,999"
replace income = 15 if q_inc == "$10,000 to $19,999"
replace income = 25 if q_inc == "$20,000 to $29,999"
replace income = 35 if q_inc == "$30,000 to $39,999"
replace income = 45 if q_inc == "$40,000 to $49,999"
replace income = 55 if q_inc == "$50,000 to $59,999"
replace income = 62.5 if q_inc == "$60,000 to $74,999"
replace income = 87.5 if q_inc == "$75,000 to $99,999"
replace income = 112.5 if q_inc == "$100,000 to $124,999"
replace income = 137.5 if q_inc == "$125,000 to $149,999"
replace income = 200 if q_inc == "$150,000 or more"
gen log_inc = log(income + 1)
drop q_inc

replace q_zip = "43011" if q_zip == ",43011"  // one miscoded response
gen zipcode = real(q_zip)
merge m:m zipcode using `cases_temp', keepusing(state county zdensity cases_week deaths_week)	// merge zipcode data

drop q_zip 

** balance sample for representativeness
qui ebalance democrat republican age_low age_mid female white latinx black asian, ///
manualtargets(.3125 .3125 .481 .348 .508 .615 .176 .123 .053) // numbers from PAP

qui svyset responseid [pweight = _webal]

gen log_cases_week = log(cases_week + 1)
gen log_deaths_week = log(deaths_week + 1)
replace log_cases_week = 0 if log_cases_week == .
replace log_deaths_week = 0 if log_deaths_week == .

gen log_density = log(zdensity)

** for subjects with unmatched zip codes:

qui svy: mean log_density
qui ereturn list
mat b = e(b)
local mean_log_density = b[1,1]
replace log_density = `mean_log_density' if log_density == .  // log_density to mean
replace state = 99 if state == .

qui svy: mean log_cases_week
qui ereturn list
mat b = e(b)
local mean_log_cases_week = b[1,1]
replace log_cases_week = `mean_log_cases_week' if state == 99
svy: mean log_deaths_week
qui ereturn list
mat b = e(b)
local mean_log_deaths_week = b[1,1]
replace log_deaths_week = `mean_log_deaths_week' if state == 99

drop if age == .  // drop unmatched zips data

gen health = .
replace health = 1 if q_health == "Excellent"
replace health = .75 if q_health == "Very good"
replace health = .5 if q_health == "Good"
replace health = .25 if q_health == "Fair"
replace health = 0 if q_health == "Poor"
drop q_health

gen diabetes = (q_health_yn_1 == "Yes")
gen lung_disease = (q_health_yn_2 == "Yes")
gen heart_disease = (q_health_yn_3 == "Yes")
gen work_with_others = (q_health_yn_4 == "Yes")
gen smoke_life = (q_health_yn_5 == "Yes")
gen smoke_now = (q_health_yn_6 == "Yes")
drop q_health_yn_*


***** News sources *****

forval i=1/12 {
	gen trust_general_`i' = .
	replace trust_general_`i' = 0 if q_trust_general_`i' == "None at all" | q_trust_general_`i' == "Not familiar with this outlet"
	replace trust_general_`i' = 1/3 if q_trust_general_`i' == "Not very much"
	replace trust_general_`i' = 2/3 if q_trust_general_`i' == "A fair amount"
	replace trust_general_`i' = 1 if q_trust_general_`i' == "A great deal"
}	

forval i=1/12 {
	gen trust_covid_`i' = .
	replace trust_covid_`i' = 0 if q_trust_covid_`i' == "None at all" | q_trust_covid_`i' == "Not familiar with this outlet"
	replace trust_covid_`i' = 1/3 if q_trust_covid_`i' == "Not very much"
	replace trust_covid_`i' = 2/3 if q_trust_covid_`i' == "A fair amount"
	replace trust_covid_`i' = 1 if q_trust_covid_`i' == "A great deal"
}	

forval i=1/12 {
	gen consume_general_`i' = .
	replace consume_general_`i' = 0 if q_consume_general_`i' == "Never" | q_consume_general_`i' == "Not familiar with this outlet"
	replace consume_general_`i' = 1/3 if q_consume_general_`i' == "Rarely"
	replace consume_general_`i' = 2/3 if q_consume_general_`i' == "Sometimes"
	replace consume_general_`i' = 1 if q_consume_general_`i' == "Often"
}	

** This measure, frequency of watching news about covid, is used in analysis
forval i=1/12 {
	gen consume_covid_`i' = .
	replace consume_covid_`i' = 0 if q_consume_covid_`i' == "Never" | q_consume_covid_`i' == "Not familiar with this outlet"
	replace consume_covid_`i' = 1/3 if q_consume_covid_`i' == "Rarely"
	replace consume_covid_`i' = 2/3 if q_consume_covid_`i' == "Sometimes"
	replace consume_covid_`i' = 1 if q_consume_covid_`i' == "Often"
}	
drop q_trust_general* q_consume_general* q_trust_covid* q_consume_covid*

foreach i in general covid {
	foreach j in trust consume {
		rename `j'_`i'_1 `j'_`i'_network
		rename `j'_`i'_2 `j'_`i'_breitbart
		rename `j'_`i'_3 `j'_`i'_cnn
		rename `j'_`i'_4 `j'_`i'_facebook
		rename `j'_`i'_5 `j'_`i'_fox
		rename `j'_`i'_6 `j'_`i'_msnbc
		rename `j'_`i'_7 `j'_`i'_nyt
		rename `j'_`i'_8 `j'_`i'_wsj
		rename `j'_`i'_9 `j'_`i'_twitter
		rename `j'_`i'_10 `j'_`i'_wiki
		rename `j'_`i'_11 `j'_`i'_cdc
		rename `j'_`i'_12 `j'_`i'_who
	}
}

foreach j in consume trust {
	foreach i in general covid {
		gen `j'_partisan_news_`i' = ///
			(-1 * `j'_`i'_nyt - 2/3 * `j'_`i'_msnbc - 1/3 * `j'_`i'_cnn + ///
			0 * `j'_`i'_network + 1/3 * `j'_`i'_wsj + 2/3 * `j'_`i'_fox + 1 * `j'_`i'_breitbart) ///
			/ (`j'_`i'_nyt + `j'_`i'_msnbc + `j'_`i'_cnn + `j'_`i'_network + ///
			   `j'_`i'_wsj + `j'_`i'_fox + `j'_`i'_breitbart)
	}
}
							
gen social_dist = (social_dist_now + social_dist_twoweeks)/2

** covid_normal = P(COVID | normal routine), covid_dist = P(COVID | social distancing)
gen covid_normal = normalprob  // q wording slightly different if 0% social distancing
replace covid_normal = normalprob0 if normalprob0 != . 
rename distancingprob covid_dist
drop normalprob*

// gen correct_origin = (q_origin == "It came about naturally")
// gen correct_trump_test = (q_trumptest == "Yes, and he tested negative")
// gen correct_both = correct_origin * correct_trump_test
// drop q_origin q_trumptest

gen covid_or_economy = (q_economy-1)/6
drop q_economy

gen us_cases = .
replace us_cases = q_cases_uninc if incentivized == 0
replace us_cases = q_cases_inc if incentivized == 1
drop q_cases_uninc q_cases_inc
winsor2 us_cases, cuts(5 95)  // winsorized after merging incentivized/unincentivized

gen trump_approval = .
replace trump_approval = q_trump_uninc if incentivized == 0
replace trump_approval = q_trump_inc if incentivized == 1

svy: mean trump_approval if trump_approval >= 0 & trump_approval <= 100
qui ereturn list
mat b = e(b)
local mean_trump_approval = b[1,1]
replace trump_approval = `mean_trump_approval' if trump_approval < 0 | ///
							trump_approval > 100 // subjects <0 or >100 set to mean
drop q_trump_uninc q_trump_inc

gen trump_q_first = (quantitativepredictionsunincenti == "Q85|Q113|Q112") | ///
					(quantitativepredictionsincenti == "Q35|Q29|Q28") 
drop quantitativepredictionsincentivi quantitativepredictionsunincenti
rename wtpdistancing wtp

export delimited "output/cloudresearch_cleaned.csv", replace

