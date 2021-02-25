program drop _all

program run_geo_time
	syntax , geo(string) time(string) reg_ns(string) [PLACebo ROBustness alt_outcomes(string) alt_reg_ns(string) SAVE_stats]
	
	local plac = cond("`placebo'"=="", "", "_plac")
	import delimited "input/`geo'_`time'`plac'_data.csv", clear
	
	local outcomes = "log_visits `alt_outcomes'"
	
	*Specify controls
	local health_controls   = "log_pop_density sh_age_65plus" + ///
		cond("`placebo'"=="", " log_cases log_deaths county_sip", "") 
	local econ_controls     = "sh_race_white sh_race_black sh_race_asian sh_educ_bachelors_plus sh_poverty sh_commute_public sh_enroll_undergrad sh_inc_gtreq100k sh_occ_man_bus_sci_art sh_occ_service sh_occ_sales_office sh_occ_nat_constr_maint"
	local weather_controls  = "tmin tmax precip"
	local combined_controls = "`health_controls' `econ_controls' `weather_controls'"
	
	if ("`robustness'"!="") {
		preserve
		import delimited "input/`geo'_`time'_plac_data.csv", clear
		destring visits, replace
		gen lag_log_visits = log(1+visits)
		keep `geo' time lag_log_visits
		tempfile placebo_data
		save `placebo_data'
		restore
		merge 1:1 `geo' time using `placebo_data', nogen assert(3)
		ds sh_*
		local share_vars = r(varlist)
		local saturated_controls: list combined_controls | share_vars
		local to_exclude = "sh_race_other sh_hispanic sh_degree_education sh_occ_production_transport sh_homelang_denom sh_commute_other sh_householdsize_6plus industry" 
		local saturated_controls: list saturated_controls - to_exclude
	}
	else {
		local saturated_controls = "`combined_controls'"
	}
	
	ren date date_str
	gen date = date(date_str, "YMD")
	format date %td
	if ("`geo'"=="county")         gen geo_id = county
	if ("`geo'"=="countyindustry") egen long geo_id = group(county industry)
	if ("`geo'"=="precinct")       egen long geo_id = group(precinct_id state_fips)
	tsset geo_id time
	
	local to_clean: list outcomes | saturated_controls
	preclean, to_clean(`to_clean')
	
	local all_vars = "`outcomes' `saturated_controls' r_share geo_id time county state_fips date"
	if ("`robustness'"!="")    		local all_vars = "`all_vars' pop lag_log_visits"
	destring `all_vars', ignore("NAa") replace
	if ("`geo'"=="countyindustry") 	local all_vars = "`all_vars' industry"
	keep `all_vars'
	
	sort time geo_id
	foreach v of varlist `saturated_controls' {
		local v2 = substr("`v'", 1, min(25, strlen("`v'")))
		by time: astile b_`v2' = `v', nq(10)
		local saturated_bins = "`saturated_bins' b_`v2'"
		foreach type in "health" "econ" "weather" {
			if (`: list v in `type'_controls') local `type'_bins = "``type'_bins' b_`v2'"
		}
		if ("`robustness'"=="") drop `v'
	}
	local combined_bins  = "`health_bins' `econ_bins' `weather_bins'"
		
	tsset
	local min_t = cond("`time'"=="day", -1, 0)
	if ("`robustness'"!="") {
		local ps = "10 25 50"
		foreach p of local ps {
			_pctile r_share if time>=`min_t', percentiles(`p' `=100-`p'')
			gen P_`p' = cond(r_share<r(r1), -1, ///
						cond(r_share>r(r2),  1, ///
						0)) if !missing(r_share)
		}
	}
	
	keep if time>=`min_t'
	
	if ("`time'"=="day") {
		gen weekend  = inlist(dow(date), 0, 6) if !missing(date) 
		assert  time < 999
		recode time (-1 = 999) //Stata does not allow negative factor variables, but we need t=-1 to normalize both weekdays and weekends separately
	
		run_main_regs, combined_bins(`combined_bins') ///
			reg_ns(`reg_ns') nm(`geo'_`time'`plac') geo_absorb(geo_id#weekend)
	} 
	else if ("`geo'"=="countyindustry") {
		local industries = `""retail_trade" "entertainment" "accomod_and_food" "health_care" "other_industries""'
		local restrictions = ""
		foreach industry of local industries {
			local restrictions = `"`restrictions' `"if industry=="`industry'""'"'
		}
		run_main_regs, combined_bins(`combined_bins') ///
			reg_ns(`reg_ns') nm(`geo'_`time'`plac') restrictions(`"`restrictions'"')
	}
	else {
		run_main_regs, combined_bins(`combined_bins') reg_ns(`reg_ns') nm(`geo'_`time'`plac')
		local base_count = e(N)
		if ("`save_stats'"=="save_stats") save_stats //Save regression values and other numbers cited in text
		
	}
	
	if ("`alt_outcomes'"!="") {
		run_main_regs, outcomes(`alt_outcomes') combined_bins(`combined_bins') ///
			reg_ns(`alt_reg_ns') nm(`geo'_`time'`plac'_alt)
	}
	
	if ("`robustness'"!="") {
		run_robustness_regs, combined_bins(`combined_bins') ///
			nm(`geo'_`time'`plac'_robustness) health_bins(`health_bins') econ_bins(`econ_bins') ///
			weather_bins(`weather_bins') saturated_bins(`saturated_bins') ///
			combined_controls(`combined_controls') base_count(`base_count')
	}
end

program define preclean
	syntax , to_clean(string)

	if regexm("`to_clean'", "devices_outside_home") {
		destring device_count completely_home_device_count, ignore("NAa") replace //Replaces R's NA or NaN values with missing
		gen devices_outside_home = device_count - completely_home_device_count
	}
	if regexm("`to_clean'", "home_alt") {
		destring device_count device_count completely_home_device_count, ignore("NAa") replace
		by geo_id: egen starting_device_count =  max(device_count * (time == 0))
		gen completely_home_alt = completely_home_device_count + (starting_device_count - device_count)
		replace completely_home_alt = max(0, completely_home_alt) if !missing(completely_home_alt)
		if (`: list posof "share_leave_home_alt" in to_clean') {
			gen share_leave_home_alt = 1 - completely_home_alt /starting_device_count
		}
	}
	if (`: list posof "share_leave_home" in to_clean') {
		destring completely_home_device_count device_count, ignore("NAa") replace
		gen share_leave_home           = 1 - completely_home_device_count / device_count
	}
	if (`: list posof "share_leave_home_candidate" in to_clean') {
		destring completely_home_device_count candidate_device_count, ignore("NAa") replace
		gen share_leave_home_candidate = 1 - completely_home_device_count / candidate_device_count
	}
	if regexm("`to_clean'", "destination_cbgs_not_self") {
		destring destination_cbgs_sum destination_cbgs_self, ignore("NAa") replace
		gen destination_cbgs_not_self = destination_cbgs_sum - destination_cbgs_self
	}
	if regexm("`to_clean'", "median_away_sg") {
		destring avg_median_non_home_dwell_time, ignore("NAa") replace
		ren avg_median_non_home_dwell_time median_away_sg
	}
	
	ds
	local all_vars = r(varlist)
	foreach var in `to_clean' {
		if (regexm("`var'", "(^log_)(.*$)") & (!`: list var in all_vars')) {
			local stub = regexs(2)
			destring `stub', ignore("NAa") replace
			gen `var' = log(1 + `stub')
		}
	}
end

program define run_main_regs
	syntax , combined_bins(varlist numeric) reg_ns(string) nm(string) ///
		[outcomes(varlist numeric min=1) RESTRictions(string) GEO_absorb(string)]
	
	*Specify default
	if ("`outcomes'"=="")       local outcomes = "log_visits" 	//Default outcome is log_visits
	if (`"`restrictions'"'=="") local restrictions = `""""' 	//Default is no restriction
	if ("`geo_absorb'"=="")		local geo_absorb = "geo_id"
	
	
	*Expand regression numlist
	numlist "`reg_ns'"
	local reg_ns = r(numlist)
	
	*Save list of coefficients to extract
	summ time if time!=999
	local max_t = r(max)
	forval i=1/`max_t' {
		local coef_names = "`coef_names' `i'.time#c.r_share"
	}
	
	*Save list of timeXbin interactions
	foreach var of varlist `combined_bins' {
		local timeXcombined_bins = "`timeXcombined_bins' time#`var'"
	}
	
	*Initialize regression table
	init_b_se_table, vars(`max_t')  extra_rows(2) ///
		regs(`= `: word count `outcomes'' * `: word count `reg_ns'' * `: word count `restrictions'' ')
		
	*Loop through and run regressions, adding stats to matrix
	local i = 1
	foreach y of varlist `outcomes' {
		foreach restriction of local restrictions {
			if (`: list posof "1" in reg_ns') {
				reghdfe `y' i(1/`max_t').time#c.r_share `restriction', absorb(`geo_absorb' time) vce(cluster state_fips)
				add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
			}	
			if (`: list posof "2" in reg_ns') {
				reghdfe `y' i(1/`max_t').time#c.r_share  `restriction', absorb(`geo_absorb' time#state_fips) vce(cluster state_fips)
				add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
			}
			if (`: list posof "3" in reg_ns') {
				reghdfe `y' i(1/`max_t').time#c.r_share `restriction', absorb(`geo_absorb' time#state_fips `timeXcombined_bins') vce(cluster state_fips)
				add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
			}
			if (`: list posof "4" in reg_ns') {
				reghdfe `y' i(1/`max_t').time#c.r_share `restriction', absorb(`geo_absorb' time#county `timeXcombined_bins') vce(cluster state_fips)
				add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")

			}
		}
	}
	
	*Save regression output as text file
	matrix_to_txt, matrix(table) saving("output/`nm'.txt") replace title("<tab:`nm'>")
end

program define save_stats, sortpreserve
	*Confirm that week of minimum visits is week 10, starting April 6th
	tempvar visits tot_visits
	gen `visits' = exp(log_visits)-1
	assert abs(log(`visits'+1) - log_visits)<1e-6 if !missing(`visits')
	bys time: egen `tot_visits' = total(`visits')
	summ `tot_visits'
	assert time==10 if `tot_visits'==r(min)
	assert td(27jan2020)+10*7==td(6apr2020)
	*Confirm that week with maximum partisan gap is week 15, starting May 11th
	summ time if e(sample)==1
	mata: asserteq(st_matrix("e(b)")[15], max(st_matrix("e(b)")[1..`r(max)']))
	assert td(27jan2020)+15*7==td(11may2020)
	*Get 10th and 90th percentile Republican vote share in sample
	_pctile r_share if e(sample)==1 & time==0, percentiles(10 90)
	local r10 = r(r1)
	local r90 = r(r2)
	local r90less10 = `r90'-`r10'
	summ `visits' if e(sample)==1 & time==0
	local visits0 = r(sum)
	foreach t in "10" "15" {
		local coef`t' = _b[`t'.time#c.r_share]
		local diffhilo`t' = `coef`t''*(`r90'-`r10')
		summ `visits' if e(sample)==1 & time==`t'
		local visits`t' = r(sum)
		local visitspct`t' = `visits`t''/`visits0'
		local diffhilopct`t' = (`diffhilo`t'')*`visitspct`t''/(1-`visitspct`t'')
		local droppct`t' = 1 - `visitspct`t''
	}

	mat table_stats = J(13, 1, .)
	mat table_stats[1,1]  = `coef10'
	mat table_stats[2,1]  = `coef15'
	mat table_stats[3,1]  = `diffhilo10'
	mat table_stats[4,1]  = `diffhilo15'
	mat table_stats[5,1]  = `r90'
	mat table_stats[6,1]  = `r10'
	mat table_stats[7,1]  = `r90less10'
	mat table_stats[8,1]  = `diffhilopct10'
	mat table_stats[9,1]  = `diffhilopct15'
	mat table_stats[10,1] = `visitspct10'
	mat table_stats[11,1] = `droppct10'
	mat table_stats[12,1] = `visitspct15'
	mat table_stats[13,1] = `droppct15'
	matrix_to_txt, matrix(table_stats) saving("output/magnitude_stats.txt") replace title("<tab:magnitude_stats>")

	
end

program define run_robustness_regs
	syntax , combined_bins(varlist numeric) nm(string) health_bins(varlist numeric) ///
		econ_bins(varlist numeric) weather_bins(varlist numeric) 				///
		saturated_bins(varlist numeric) combined_controls(varlist numeric) 		///
		base_count(integer) [outcomes(varlist numeric min=1)]
	
	*Specify default
	if ("`outcomes'"=="") local outcomes = "log_visits" //Default outcome is log_visits
	
	*Save list of coefficients to extract
	summ time
	local max_t = r(max)
	forval i=1/`max_t' {
		local coef_names = "`coef_names' `i'.time#c.r_share"
	}
	
	*Save list of timeXbin interactions
	foreach var of varlist `saturated_bins' {
		local timeXsaturated_bins = "`timeXsaturated_bins' time#`var'"
		if (`: list var in combined_bins') local timeXcombined_bins = "`timeXcombined_bins' time#`var'"
		foreach type in "health" "econ" "weather" {
			if (`: list var in `type'_bins') local timeX`type'_bins = "`timeX`type'_bins' time#`var'"
		}
	}
	
	*Initialize regression table
	init_b_se_table, vars(`max_t') extra_rows(2) regs(18)
	local i = 1
	*Run regressions, adding stats to matrix
	* Dropping Controls (1-3)
	reghdfe log_visits i(1/`max_t').time#c.r_share, absorb(geo_id time#state_fips `timeXecon_bins' `timeXweather_bins') vce(cluster state_fips)
	add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
	
	reghdfe log_visits i(1/`max_t').time#c.r_share, absorb(geo_id time#state_fips `timeXhealth_bins' `timeXweather_bins') vce(cluster state_fips)
	add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")

	reghdfe log_visits i(1/`max_t').time#c.r_share, absorb(geo_id time#state_fips `timeXhealth_bins' `timeXecon_bins') vce(cluster state_fips)
	add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")

	* Alternative controls (4-6)
	reghdfe log_visits i(1/`max_t').time#c.r_share, absorb(geo_id time `timeXcombined_bins') vce(cluster state_fips)
	add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
	
	reghdfe log_visits i(1/`max_t').time#c.r_share, absorb(geo_id time#state_fips c.(`combined_controls')##i.time) vce(cluster state_fips)
	add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
	
	reghdfe log_visits i(1/`max_t').time#c.r_share, absorb(geo_id time#state_fips `timeXsaturated_bins' c.lag_log_visits##i.time) vce(cluster state_fips)
	add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
	
	
	* Partisanship Indicators (7-9)
	foreach p in "50" "25" "10" {
		forval k=1/`max_t' {
			local coef_names`p' = "`coef_names`p'' `k'.time#c.P_`p'"
		}
		reghdfe log_visits i(1/`max_t').time#c.P_`p', absorb(geo_id time#state_fips `timeXcombined_bins') vce(cluster state_fips)
		add_b_se_table, iter(`i++') vars("`coef_names`p''") add_N("yes") add_N_clust("yes")
	}
	
	* Sample Restrictions (10-12)
	reghdfe log_visits i(1/`max_t').time#c.r_share if pop > 3000 & !missing(pop), absorb(geo_id time#state_fips `timeXcombined_bins') vce(cluster state_fips)
	add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
	local drop3000 = 1 - e(N)/`base_count'
	
	reghdfe log_visits i(1/`max_t').time#c.r_share if pop < 500000, absorb(geo_id time#state_fips `timeXcombined_bins') vce(cluster state_fips)
	add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
	local drop500000 = 1 - e(N)/`base_count'
	
	* Save sample restriction values
	mat table_drop = J(2, 1, .)
	mat table_drop[1,1]  = `drop3000'
	mat table_drop[2,1]  = `drop500000'
	matrix_to_txt, matrix(table_drop) saving("output/share_drop_pop_restrictions.txt") replace title("<tab:share_drop_pop_restrictions>")
	
	
	reghdfe log_visits i(1/`max_t').time#c.r_share if !inlist(state_fips, 6, 36, 53), absorb(geo_id time#state_fips `timeXcombined_bins') vce(cluster state_fips)
	add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
	
	* Sample Restrictions by Vote Shares (13-15)
	reghdfe log_visits i(1/`max_t').time#c.r_share if P_10==0, absorb(geo_id time#state_fips `timeXcombined_bins') vce(cluster state_fips)
	add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
	
	reghdfe log_visits i(1/`max_t').time#c.r_share if P_50==1, absorb(geo_id time#state_fips `timeXcombined_bins') vce(cluster state_fips)
	add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
	
	reghdfe log_visits i(1/`max_t').time#c.r_share if P_50==-1, absorb(geo_id time#state_fips `timeXcombined_bins') vce(cluster state_fips)
	add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
	
	* Weighting, SEs, Start Dates (16-18)
	reghdfe log_visits i(1/`max_t').time#c.r_share [aw=pop], absorb(geo_id time#state_fips `timeXcombined_bins') vce(cluster state_fips)
	add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
	
	reghdfe log_visits i(1/`max_t').time#c.r_share, absorb(geo_id time#state_fips `timeXcombined_bins') vce(cluster county)
	add_b_se_table, iter(`i++') vars("`coef_names'") add_N("yes") add_N_clust("yes")
	
	local coef_names_start2 = subinstr(`"`coef_names'"', "1.time#c.r_share", ".", 1)
	reghdfe log_visits i(2/`max_t').time#c.r_share, absorb(geo_id time#state_fips `timeXcombined_bins') vce(cluster state_fips)
	add_b_se_table, iter(`i++') vars("`coef_names_start2'") add_N("yes") add_N_clust("yes")
	assert `i'==19
	
	* Save regression output as text file
	matrix_to_txt, matrix(table) saving("output/`nm'.txt") replace title("<tab:`nm'>")
end

run_geo_time, geo("county") time("week") reg_ns(1/3) alt_reg_ns(1/3) robustness ///
	alt_outcomes(log_destination_cbgs_not_self log_devices_outside_home log_median_away_sg share_leave_home share_leave_home_alt share_leave_home_candidate log_device_count log_destination_cbgs_sum log_destination_cbgs_self) save_stats
	
run_geo_time, geo("county") time("week") reg_ns(1/3) placebo

run_geo_time, geo("county") time("day") reg_ns(3) 

run_geo_time, geo("countyindustry") time("week") reg_ns(3)

run_geo_time, geo("precinct") time("week") reg_ns(1/4) alt_reg_ns(4) ///
	alt_outcomes(log_destination_cbgs_not_self log_devices_outside_home)
	
run_geo_time, geo("precinct") time("week") reg_ns(1/4) placebo
