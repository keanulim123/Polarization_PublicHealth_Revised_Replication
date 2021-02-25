********************************************************************************
********** Survey Analysis **********
********************************************************************************

import delimited "input/cloudresearch_cleaned.csv", clear

** demographic distribution

foreach demographic in age_low age_mid age_high female male white latinx black asian democrat republican independent {
	egen share_`demographic' = mean(`demographic')
}
		
** balance sample for representativeness
qui ebalance democrat republican age_low age_mid female white latinx black asian, ///
manualtargets(.3125 .3125 .481 .348 .508 .615 .176 .123 .053) // numbers from PAP

qui svyset responseid [pweight = _webal]

drop race_dummy
encode race, gen(race_dummy)
gen log_us_cases_w = log(us_cases_w)

** z scores for variables
foreach outcome in us_cases_w log_us_cases_w trump_approval social_dist covid_dist covid_normal covid_or_economy {
	qui svy: mean `outcome'
	qui ereturn list
	mat b = e(b)
	qui estat sd
	qui return list
	mat sd = r(sd)
	scalar `outcome'_mean = b[1,1]
	scalar `outcome'_sd = sd[1,1]
	gen `outcome'_z = (`outcome' - `outcome'_mean) / `outcome'_sd
}

** correlation between beliefs and social distancing
qui xi: svy: reg covid_normal_z social_dist_z i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state 

qui ereturn list
scalar b_dist_and_efficacy = _b[social_dist_z]
scalar se_dist_and_efficacy = _se[social_dist_z]
scalar t_dist_and_efficacy = b_dist_and_efficacy / se_dist_and_efficacy
scalar p_dist_and_efficacy = 2 * ttail(e(df_r), abs(t_dist_and_efficacy))

qui xi: svy: reg us_cases_w_z social_dist_z i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state 

qui ereturn list
scalar b_dist_and_cases = _b[social_dist_z]
scalar se_dist_and_cases = _se[social_dist_z]
scalar t_dist_and_cases = b_dist_and_cases / se_dist_and_cases
scalar p_dist_and_cases = 2 * ttail(e(df_r), abs(t_dist_and_cases))


********** Coefplot: Main Outcomes **********

******* By partisanship *******

forval i=1/20 {
	gen party_cont`i' = party_cont  // duplicates so there are new coefs in coefplot
}

label var party_cont1 " "
label var party_cont2 `" "Predicted cases:" "All subjects" "'
label var party_cont10 " "
label var party_cont9 `" "Predicted cases:" "Incentivized subjects" "'
label var party_cont3 " "
label var party_cont4 `" "Self-reported" "social distancing" "'
label var party_cont5 " "
label var party_cont6 `" "Effectiveness" "of distancing" "'
label var party_cont7 " "
label var party_cont8 `" "Important to distance" "vs. help economy" "'

eststo clear

qui svy: reg us_cases_w_z party_cont1
eststo us_cases_nocontrols

qui xi: svy: reg us_cases_w_z party_cont2 trump_q_first i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
eststo us_cases_controls

qui ereturn list
mat coeffs_us_cases = e(b)
scalar b_cases_z = coeffs_us_cases[1,1]
scalar b_cases_rep = b_cases_z * us_cases_w_sd/2 + us_cases_w_mean
scalar b_cases_dem = -b_cases_z * us_cases_w_sd/2 + us_cases_w_mean

qui svy: reg us_cases_w_z party_cont9 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state ///
if incentivized == 1
eststo us_cases_incentives_c

qui svy: reg us_cases_w_z party_cont10 if incentivized == 1
eststo us_cases_incentives_nc

qui svy: reg social_dist_z party_cont3
eststo social_dist_nocontrols

qui xi: svy: reg social_dist_z party_cont4 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state 
eststo social_dist_controls

qui ereturn list
mat coeffs_social_dist = e(b)
scalar b_social_dist_z = coeffs_social_dist[1,1]
scalar b_social_dist_rep = b_social_dist_z * social_dist_sd/2 + social_dist_mean
scalar b_social_dist_dem = -b_social_dist_z * social_dist_sd/2 + social_dist_mean

qui svy: reg covid_normal_z party_cont5
eststo covid_normal_nocontrols

qui xi: svy: reg covid_normal_z party_cont6 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
eststo covid_normal_controls

qui ereturn list
mat coeffs_covid_normal = e(b)
scalar b_covid_normal_z = coeffs_covid_normal[1,1]
scalar b_covid_normal_rep = b_covid_normal_z * covid_normal_sd/2 + covid_normal_mean
scalar b_covid_normal_dem = -b_covid_normal_z * covid_normal_sd/2 + covid_normal_mean

qui svy: reg covid_or_economy_z party_cont7
eststo covid_or_economy_nocontrols

qui xi: svy: reg covid_or_economy_z party_cont8 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
eststo covid_or_economy_controls
qui ereturn list

mat coeffs_covid_or_economy = e(b)
scalar b_covid_or_economy_z = coeffs_covid_or_economy[1,1]
scalar b_covid_or_economy_rep = b_covid_or_economy_z * covid_or_economy_sd/2 + covid_or_economy_mean
scalar b_covid_or_economy_dem = -b_covid_or_economy_z * covid_or_economy_sd/2 + covid_or_economy_mean

qui xi: svy: reg covid_normal_z social_dist_z i.race_dummy age male children edu log_inc log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
mat coeffs_covid_normal_dist = e(b)
scalar b_covid_normal_dist_corr = coeffs_covid_normal_dist[1,1]

qui xi: svy: reg us_cases_w_z social_dist_z i.race_dummy age male children edu log_inc log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
mat coeffs_cases_dist = e(b)
scalar b_cases_dist_corr = coeffs_cases_dist[1,1]

qui xi: svy: reg covid_normal_z social_dist_z party_cont i.race_dummy age male children edu log_inc log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
mat coeffs_covid_normal_dist_party = e(b)
scalar b_covid_normal_dist_corr_party = coeffs_covid_normal_dist_party[1,1]

qui xi: svy: reg us_cases_w_z social_dist_z party_cont i.race_dummy age male children edu log_inc log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
mat coeffs_cases_dist_party = e(b)
scalar b_cases_dist_corr_party = coeffs_cases_dist_party[1,1]

_pctile wtp, p(50)
qui return list
scalar wtp_median = r(r1)

** Text file with values
// mat matrix_values = (b_covid_normal_dist_corr, b_cases_dist_corr, b_covid_normal_dist_corr_party, b_cases_dist_corr_party, wtp_median \ /// 
// 					social_dist_mean, social_dist_sd, b_social_dist_z, b_social_dist_rep, b_social_dist_dem \ ///
// 					covid_or_economy_mean, covid_or_economy_sd, b_covid_or_economy_z, b_covid_or_economy_rep, b_covid_or_economy_dem \ ///
// 					covid_normal_mean, covid_normal_sd, b_covid_normal_z, b_covid_normal_rep, b_covid_normal_dem \ ///
// 					us_cases_w_mean, us_cases_w_sd, b_cases_z, b_cases_rep, b_cases_dem)	
// mat2txt, matrix(matrix_values) saving(output/survey_estimates.txt) replace					
				
** Main figure
coefplot ///
(social_dist_nocontrols, keep(party_cont3) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(social_dist_controls, keep(party_cont4) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
(covid_or_economy_nocontrols, keep(party_cont7) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(covid_or_economy_controls, keep(party_cont8) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
(covid_normal_nocontrols, keep(party_cont5) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(covid_normal_controls, keep(party_cont6) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
(us_cases_nocontrols, keep(party_cont1) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(us_cases_controls, keep(party_cont2) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
(us_cases_incentives_nc, keep(party_cont10) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(us_cases_incentives_c, keep(party_cont9) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
, horizontal legend(order(2 "No controls" 4 "Controls") rows(1)) offset(0) drop(*_dummy *topic_id* _Icode* _cons) xline(0) ///
xscale(r(-0.6 0.6)) xlabel(-0.6 (0.2) 0.6) yscale(r(1 7)) yla(, notick) /// 
ytitle("") xtitle("Partisan difference") ///
graphregion(fcolor(white))

graph export "output/partisanship_coefplot_horizontal.pdf", replace


******* Unweighted *******

label var party_cont1 " "
label var party_cont2 `" "Predicted cases:" "All subjects" "'
label var party_cont10 " "
label var party_cont9 `" "Predicted cases:" "Incentivized subjects" "'
label var party_cont3 " "
label var party_cont4 `" "Self-reported" "social distancing" "'
label var party_cont5 " "
label var party_cont6 `" "Effectiveness" "of distancing" "'
label var party_cont7 " "
label var party_cont8 `" "Important to distance" "vs. help economy" "'

eststo clear

qui reg us_cases_w_z party_cont1, r
eststo us_cases_nocontrols

qui xi: reg us_cases_w_z party_cont2 trump_q_first i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state, r
eststo us_cases_controls

qui reg us_cases_w_z party_cont9 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state ///
if incentivized == 1, r
eststo us_cases_incentives_c

qui xi: reg us_cases_w_z party_cont10 if incentivized == 1, r
eststo us_cases_incentives_nc

qui reg social_dist_z party_cont3, r
eststo social_dist_nocontrols

qui xi: reg social_dist_z party_cont4 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state, r
eststo social_dist_controls

qui reg covid_normal_z party_cont5, r
eststo covid_normal_nocontrols

qui xi: reg covid_normal_z party_cont6 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state, r
eststo covid_normal_controls

qui reg covid_or_economy_z party_cont7, r
eststo covid_or_economy_nocontrols

qui xi: reg covid_or_economy_z party_cont8 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state, r
eststo covid_or_economy_controls


coefplot ///
(social_dist_nocontrols, keep(party_cont3) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(social_dist_controls, keep(party_cont4) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
(covid_or_economy_nocontrols, keep(party_cont7) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(covid_or_economy_controls, keep(party_cont8) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
(covid_normal_nocontrols, keep(party_cont5) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(covid_normal_controls, keep(party_cont6) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
(us_cases_nocontrols, keep(party_cont1) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(us_cases_controls, keep(party_cont2) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
(us_cases_incentives_nc, keep(party_cont10) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(us_cases_incentives_c, keep(party_cont9) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
, horizontal legend(order(2 "No controls" 4 "Controls") rows(1)) offset(0) drop(*_dummy *topic_id* _Icode* _cons) xline(0) ///
xscale(r(-0.6 0.6)) xlabel(-0.6 (0.2) 0.6) yscale(r(1 7)) yla(, notick) /// 
ytitle("") xtitle("Partisan difference") ///
graphregion(fcolor(white))

graph export "output/partisanship_coefplot_unweighted.pdf", replace


********** Coefplot: With and without incentives **********

gen trump_disapproval_z = -trump_approval_z

label var party_cont1 `" "Predicted" "U.S. cases" "'
label var party_cont2 " "
label var party_cont3 `" "Trump disapproval" "'
label var party_cont4 " "

eststo clear

qui svy: reg us_cases_w_z party_cont1 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state ///
if incentivized == 0
eststo us_cases_noincentives

qui svy: reg us_cases_w_z party_cont2 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state ///
if incentivized == 1
eststo us_cases_incentives

qui svy: reg trump_disapproval_z party_cont3 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state ///
if incentivized == 0
eststo trump_disapproval_noincent

qui svy: reg trump_disapproval_z party_cont4 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state ///
if incentivized == 1
eststo trump_disapproval_incent

coefplot ///
(us_cases_noincentives, keep(party_cont1) offset(.1) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(us_cases_incentives, keep(party_cont2) offset(.9) mcolor("140 140 50") ciopts(color("140 140 50"))) ///
(trump_disapproval_noincent, keep(party_cont3) offset(.1) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(trump_disapproval_incent, keep(party_cont4) offset(.9) mcolor("140 140 50") ciopts(color("140 140 50"))) ///
, horizontal legend(order(2 "Unincentivized" 4 "Incentivized" )) offset(0) drop(*_dummy *topic_id* _Icode* _cons) xline(0) ///
xscale(r(-1.2 0.6)) xlabel(-1.2 (0.2) 0.6) yscale(r(0 3)) yla(, notick) /// 
ytitle("") xtitle("Partisan difference") ///
graphregion(fcolor(white))

graph export "output/partisanship_incentives.pdf", replace


******* County FE *******

label var party_cont1 " "
label var party_cont2 `" "Predicted cases:" "All subjects" "'
label var party_cont10 " "
label var party_cont9 `" "Predicted cases:" "Incentivized subjects" "'
label var party_cont3 " "
label var party_cont4 `" "Self-reported" "social distancing" "'
label var party_cont5 " "
label var party_cont6 `" "Effectiveness" "of distancing" "'
label var party_cont7 " "
label var party_cont8 `" "Important to distance" "vs. help economy" "'

eststo clear

gen fips = state*1000 + county
duplicates tag fips, gen(duplicate_fips)
gen drop_county_fe = (duplicate_fips == 0 | fips == .)
egen share_drop_county_fe = mean(drop_county_fe)

preserve
drop if duplicate_fips == 0 | fips == .

** rebalance sample for representativeness
qui ebalance democrat republican age_low age_mid female white latinx black asian, ///
manualtargets(.3125 .3125 .481 .348 .508 .615 .176 .123 .053) // numbers from PAP

qui svyset responseid [pweight = _webal]

qui svy: reg us_cases_w_z party_cont1
eststo us_cases_nocontrols

qui xi: svy: reg us_cases_w_z party_cont2 trump_q_first i.race_dummy age male children edu log_inc ///
health diabetes lung_disease heart_disease smoke_life smoke_now i.fips
eststo us_cases_controls

qui svy: reg us_cases_w_z party_cont9 i.race_dummy age male children edu log_inc ///
health diabetes lung_disease heart_disease smoke_life smoke_now i.fips ///
if incentivized == 1
eststo us_cases_incentives_c

qui svy: reg us_cases_w_z party_cont10 if incentivized == 1
eststo us_cases_incentives_nc

qui svy: reg social_dist_z party_cont3
eststo social_dist_nocontrols

qui xi: svy: reg social_dist_z party_cont4 i.race_dummy age male children edu log_inc ///
health diabetes lung_disease heart_disease smoke_life smoke_now i.fips
eststo social_dist_controls

qui svy: reg covid_normal_z party_cont5
eststo covid_normal_nocontrols

qui xi: svy: reg covid_normal_z party_cont6 i.race_dummy age male children edu log_inc ///
health diabetes lung_disease heart_disease smoke_life smoke_now i.fips
eststo covid_normal_controls

qui svy: reg covid_or_economy_z party_cont7
eststo covid_or_economy_nocontrols

qui xi: svy: reg covid_or_economy_z party_cont8 i.race_dummy age male children edu log_inc ///
health diabetes lung_disease heart_disease smoke_life smoke_now i.fips
eststo covid_or_economy_controls

coefplot ///
(social_dist_nocontrols, keep(party_cont3) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(social_dist_controls, keep(party_cont4) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
(covid_or_economy_nocontrols, keep(party_cont7) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(covid_or_economy_controls, keep(party_cont8) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
(covid_normal_nocontrols, keep(party_cont5) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(covid_normal_controls, keep(party_cont6) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
(us_cases_nocontrols, keep(party_cont1) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(us_cases_controls, keep(party_cont2) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
(us_cases_incentives_nc, keep(party_cont10) offset(-.9) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(us_cases_incentives_c, keep(party_cont9) offset(-.1) mcolor("60 170 80") ciopts(color("60 170 80"))) ///
, horizontal legend(order(2 "No controls" 4 "Controls") rows(1)) offset(0) drop(*_dummy *topic_id* _Icode* _cons) xline(0) ///
xscale(r(-0.6 0.6)) xlabel(-0.6 (0.2) 0.6) yscale(r(1 7)) yla(, notick) /// 
ytitle("") xtitle("Partisan difference") ///
graphregion(fcolor(white))

graph export "output/partisanship_coefplot_countyfe.pdf", replace

restore


// ********** NOT RUN. Coefplot: Actions with controls for beliefs **********

// eststo clear

// label var party_cont1 `" "Demographic" "controls" "'
// label var party_cont2 `" "Perceived" "efficacy controls" "'
// label var party_cont3 `" "News controls" "'
// label var party_cont4 `" "Efficacy and" "news controls" "'

// qui xi: svy: reg social_dist_z party_cont1 i.race_dummy age male children edu log_inc ///
// log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
// eststo social_dist_controls

// qui xi: svy: reg social_dist_z party_cont2 covid_normal_z i.race_dummy age male children edu log_inc ///
// log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
// eststo social_dist_efficacy

// qui xi: svy: reg social_dist_z party_cont3 news_partisanship /// 
// i.race_dummy age male children edu log_inc ///
// log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
// eststo social_dist_news_consume

// qui xi: svy: reg social_dist_z party_cont4 covid_normal_z news_partisanship /// 
// i.race_dummy age male children edu log_inc ///
// log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
// eststo social_dist_news_trust

// coefplot ///
// (social_dist_controls, keep(party_cont1) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
// (social_dist_efficacy, keep(party_cont2) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
// (social_dist_news_consume, keep(party_cont3) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
// (social_dist_news_trust, keep(party_cont4) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
// , horizontal legend(off) offset(0) drop(*_dummy *topic_id* _Icode* _cons) xline(0) ///
// xscale(r(-0.6 0.6)) xlabel(-0.6 (0.2) 0.6) /// 
// ytitle("") xtitle("Partisan difference") ///
// graphregion(fcolor(white))

// graph export "output/distancing_coefplot_beliefs.pdf", replace


******* Media consumption *******

forval i=1/20 {
	gen consume_partisan_news_covid`i' = consume_partisan_news_covid  // duplicates so there are new coefs in coefplot
}


label var party_cont1 " "
label var party_cont2 `" "Predicted cases:" "All subjects" "'
label var party_cont9 " "
label var party_cont10 `" "Predicted cases:" "Incentivized subjects" "'
label var party_cont3 " "
label var party_cont4 `" "Self-reported" "social distancing" "'
label var party_cont5 " "
label var party_cont6 `" "Effectiveness" "of distancing" "'
label var party_cont7 " "
label var party_cont8 `" "Important to distance" "vs. help economy" "'

label var consume_partisan_news_covid2 " "
label var consume_partisan_news_covid4 " "
label var consume_partisan_news_covid6 " "
label var consume_partisan_news_covid8 " "
label var consume_partisan_news_covid10 " "

eststo clear

qui xi: svy: reg us_cases_w_z party_cont1 trump_q_first i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
eststo us_cases

qui xi: svy: reg us_cases_w_z party_cont2 consume_partisan_news_covid2 trump_q_first i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
eststo us_cases_news

qui xi: svy: reg us_cases_w_z party_cont9 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state ///
if incentivized == 1
eststo us_cases_incentives

qui xi: svy: reg us_cases_w_z party_cont10 consume_partisan_news_covid10 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state ///
if incentivized == 1
eststo us_cases_incentives_news

qui xi: svy: reg social_dist_z party_cont3 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state 
eststo social_dist

qui xi: svy: reg social_dist_z party_cont4 consume_partisan_news_covid4 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state 
eststo social_dist_news

qui xi: svy: reg covid_normal_z party_cont5 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
eststo covid_normal

qui xi: svy: reg covid_normal_z party_cont6 consume_partisan_news_covid6 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
eststo covid_normal_news

qui xi: svy: reg covid_or_economy_z party_cont7 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
eststo covid_or_economy

qui xi: svy: reg covid_or_economy_z party_cont8 consume_partisan_news_covid8 i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state
eststo covid_or_economy_news

coefplot ///
(social_dist, keep(party_cont3) offset(-.65) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(social_dist_news, keep(party_cont4) offset(0) mcolor("30 150 30") ciopts(color("30 150 30"))) ///
(social_dist_news, keep(consume_partisan_news_covid4) offset(.65) msymbol(Oh) mcolor("30 30 150") ciopts(color("30 30 150"))) ///
(covid_or_economy, keep(party_cont7) offset(-.65) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(covid_or_economy_news, keep(party_cont8) offset(0) mcolor("30 150 30") ciopts(color("30 150 30"))) ///
(covid_or_economy_news, keep(consume_partisan_news_covid8) offset(.65) msymbol(Oh) mcolor("30 30 150") ciopts(color("30 30 150"))) ///
(covid_normal, keep(party_cont5) offset(-.65) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(covid_normal_news, keep(party_cont6) offset(0) mcolor("30 150 30") ciopts(color("30 150 30"))) ///
(covid_normal_news, keep(consume_partisan_news_covid6) offset(.65) msymbol(Oh) mcolor("30 30 150") ciopts(color("30 30 150"))) ///
(us_cases, keep(party_cont1) offset(-.65) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(us_cases_news, keep(party_cont2) offset(0) mcolor("30 150 30") ciopts(color("30 150 30"))) ///
(us_cases_news, keep(consume_partisan_news_covid2) offset(.65) msymbol(Oh) mcolor("30 30 150") ciopts(color("30 30 150"))) ///
(us_cases_incentives, keep(party_cont9) offset(-.65) mcolor("0 0 0") ciopts(color("0 0 0"))) ///
(us_cases_incentives_news, keep(party_cont10) offset(0) mcolor("30 150 30") ciopts(color("30 150 30"))) ///
(us_cases_incentives_news, keep(consume_partisan_news_covid10) offset(.65) msymbol(Oh) mcolor("30 30 150") ciopts(color("30 30 150"))) ///
, horizontal legend(order(2 "Party effect," "no control for news" 4 "Party effect," "control for news" 6 "News effect," "control for party") rows(1)) offset(0) drop(*_dummy *topic_id* _Icode* _cons) xline(0) ///
xscale(r(-0.6 0.6)) xlabel(-0.6 (0.2) 0.6) yscale(r(1 7)) yla(, notick) /// 
ytitle("") xtitle("Partisan/news difference") ///
graphregion(fcolor(white))

graph export "output/partisanship_coefplot_news.pdf", replace


** Specifications testing differences between incentivized and unincentivized participant

qui xi: svy: reg us_cases_w_z 1.incentivized#c.party_cont incentivized party_cont ///
i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state

qui ereturn list
scalar b_incentives_level = _b[1.incentivized#c.party_cont]
scalar se_incentives_level = _se[1.incentivized#c.party_cont]
scalar t_incentives_level = b_incentives_level / se_incentives_level
scalar p_incentives_level = 2 * ttail(e(df_r), abs(t_incentives_level))

qui xi: svy: reg log_us_cases_w_z 1.incentivized#c.party_cont incentivized party_cont ///
i.race_dummy age male children edu log_inc ///
log_density log_cases_week log_deaths_week health diabetes lung_disease heart_disease smoke_life smoke_now i.state

qui ereturn list
scalar b_incentives_log = _b[1.incentivized#c.party_cont]
scalar se_incentives_log = _se[1.incentivized#c.party_cont]
scalar t_incentives_log = b_incentives_log / se_incentives_log
scalar p_incentives_log = 2 * ttail(e(df_r), abs(t_incentives_log))


******* Survey demographics ********
disp "Share 18-39:", share_age_low
disp "Share 40-59:", share_age_mid
disp "Share 60+", 1-share_age_low-share_age_mid
disp "Share Female:", share_female
disp "Share Male:", share_male
disp "Share Other/non-binary:", 1-share_female-share_male
disp "Share White non-Latinx:", share_white
disp "Share Latinx:", share_latinx
disp "Share Black non-Latinx:", share_black
disp "Share Asian:", share_asian
disp "Share Other race:", 1-share_white-share_latinx-share_black-share_asian
disp "Share Democrat:", share_democrat
disp "Share Republican:", share_republican
disp "Share Independent:", share_independent
disp "Share Other party:", 1-share_democrat-share_republican-share_independent


******* Numbers in text *******

mat table_stats = J(24, 1, .)

mat table_stats[1,1]  = b_dist_and_efficacy
mat table_stats[2,1]  = se_dist_and_efficacy
mat table_stats[3,1]  = p_dist_and_efficacy

mat table_stats[4,1]  = b_dist_and_cases
mat table_stats[5,1]  = se_dist_and_cases
mat table_stats[6,1]  = p_dist_and_cases

mat table_stats[7,1]  = social_dist_mean
mat table_stats[8,1]  = social_dist_sd
mat table_stats[9,1]  = b_social_dist_dem
mat table_stats[10,1]  = b_social_dist_rep
mat table_stats[11,1]  = b_social_dist_z

mat table_stats[12,1]  = b_covid_or_economy_z

mat table_stats[13,1]  = covid_normal_mean
mat table_stats[14,1]  = covid_normal_sd
mat table_stats[15,1]  = b_covid_normal_dem
mat table_stats[16,1]  = b_covid_normal_rep
mat table_stats[17,1]  = b_covid_normal_z

mat table_stats[18,1]  = us_cases_w_mean
mat table_stats[19,1]  = us_cases_w_sd
mat table_stats[20,1]  = b_cases_dem
mat table_stats[21,1]  = b_cases_rep
mat table_stats[22,1]  = b_cases_z

su share_drop_county_fe
mat table_stats[23,1]  = `r(mean)'

mat table_stats[24,1]  = wtp_median

matrix_to_txt, matrix(table_stats) saving("output/survey_stats.txt") replace title("<tab:survey_stats>")

******* Numbers in response to reviewer 1 *******

disp "Effect of incentives, levels: p-value", p_incentives_level
disp "Effect of incentives, logs: p-value", p_incentives_log
