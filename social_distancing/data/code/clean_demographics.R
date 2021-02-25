CRAN_packages <- c("dplyr", "readr", "magrittr", "stringr", "purrr")
lapply(CRAN_packages, require, character.only = TRUE)

main <- function() {
  n_max <- Inf # Maximum number of observations to load (decrease for testing purposes)

  #Gather open census (ACS) data at cbg-level as summable variables (eg population, land area)
  df <- data.frame(census_block_group = character(), stringsAsFactors=FALSE) #Start with empty data frame
  df %<>% 
    add_age(          n_max=n_max) %>%
    add_race(         n_max=n_max) %>%
    add_hispanic(     n_max=n_max) %>%
    add_education(    n_max=n_max) %>%
    add_poverty(      n_max=n_max) %>%
    add_income(       n_max=n_max) %>%
    add_occupation(   n_max=n_max) %>%
    add_vehicle_home( n_max=n_max) %>%
    add_insurance(    n_max=n_max) %>%
    add_commute(      n_max=n_max) %>%
    add_enrollment(   n_max=n_max) %>%
    add_native(       n_max=n_max) %>%
    add_marriage(     n_max=n_max) %>%
    add_household(    n_max=n_max) %>%
    add_homelang(     n_max=n_max) %>%
    add_land_area(    n_max=n_max) %>%
    select(census_block_group, pop, everything())   #Order census block group and population as first variables
  
  #Save demographics by CBG
  df %>% calc_ratios(remove_denoms=TRUE) %>%                 #Calculate ratios (eg share white, population density, etc.)
    merge_vote_cbg() %>%                                     #Merge on voting
    write_csv("output_local/cbg_demographics.csv")           #Save demographics
  
  #Save demographics by precinct
  df %<>% mutate(county = str_sub(census_block_group, 1, 5)) #Add county fips (derived from CBG fips)
  df %>% aggregate_precinct(remove_denoms=TRUE) %>%          #Allocate CBG-level counts to precinct based on geographic overlap, assuming constant distribution, then calculate ratios and remove denominators
    merge_vote_precinct() %>%                                #Merge on voting
    write_csv("output_local/precinct_demographics.csv")      #Save demographics
  
  #Save demographics by county
  df %>% aggregate_county(remove_denoms=TRUE) %>%            #Sum CBG-level counts to county then calculate ratios and remove denominators
    merge_vote_county() %>%                                  #Merge on voting
    write_csv("output_local/county_demographics.csv")        #Save demographics
  
  return(NULL)
}

add_age <- function(df, n_max=Inf) {
  #Add counts needed to calculate share of population <18 and share >=65
  pop_65plus_vars <- c("B01001e20","B01001e21","B01001e22","B01001e23","B01001e24","B01001e25", # Males
                       "B01001e44","B01001e45","B01001e46","B01001e47","B01001e48","B01001e49") # Females
  pop_under18_vars  <- c("B01001e3" , "B01001e4" , "B01001e5" , "B01001e6",  #Males
                         "B01001e27", "B01001e28", "B01001e29", "B01001e30") #Females
  
  to_load        <- c("c", rep.int("d", 1+length(pop_65plus_vars) + length(pop_under18_vars))) #Variable types
  names(to_load) <- c("census_block_group", "B01001e1", pop_65plus_vars, pop_under18_vars)     #Variable names
  
  age <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_b01.csv",
                  n_max = n_max, col_types = do.call(cols_only, as.list(to_load))) %>% #Load data
    transmute(census_block_group = census_block_group,                                 
              pop_age_denom      = B01001e1,
              pop_age_65plus     = rowSums(.[pop_65plus_vars]),
              pop_age_under18    = rowSums(.[pop_under18_vars])) %>%                   #Calculate totals
    mutate(pop = pop_age_denom)                                                        #Add version of population that won't be removed when we drop denominators
  
  df %<>% merge(age, by="census_block_group", all = TRUE)                              #Merge new variables onto existing data frame
  return(df)
}

add_race <- function(df, n_max=Inf) {
  #Counts for shares of population by race
  race <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_b02.csv", n_max = n_max,
                   col_types = cols_only(census_block_group = "c",
                                         B02001e1           = "d",
                                         B02001e2           = "d",
                                         B02001e3           = "d",
                                         B02001e5           = "d")) %>%                        #Load data
    rename(pop_race_denom = B02001e1, 
           pop_race_white = B02001e2, 
           pop_race_black = B02001e3,
           pop_race_asian = B02001e5) %>%                                                      #Rename variables
    mutate(pop_race_other = pop_race_denom - pop_race_white - pop_race_black - pop_race_asian) #Add "other race" category 
  
  df %<>% merge(race, by="census_block_group", all = TRUE)                                     #Merge new variables onto existing data frame
  return(df)
}

add_hispanic <- function(df, n_max=Inf) {
  #Counts for hispanic share (separate from race)
  hispanic <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_b03.csv", n_max = n_max,
                       col_types = cols_only(census_block_group = "c",
                                             B03002e1           = "d",
                                             B03002e12          = "d")) %>% #Load data
    rename(pop_hispanic_denom = B03002e1, 
           pop_hispanic       = B03002e12)                                  #Rename variables
  
  df %<>% merge(hispanic, by = "census_block_group", all = TRUE)            #Merge new variables onto existing data frame
  return(df)
}

add_education <- function(df, n_max=Inf) {
  #Count for share with bachelors degree or higher, along with breakdown by degree field
  bachelors_plus_vars <- c("B15003e22", "B15003e23", "B15003e24", "B15003e25")
  degree_vars <- paste0("B15012e", 1:16)
  educ_load           <- c("c", rep.int("d", 1+ length(bachelors_plus_vars) + length(degree_vars)))
  names(educ_load)    <- c("census_block_group", "B15003e1", bachelors_plus_vars, degree_vars)
  
  education <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_b15.csv", n_max = n_max,
                        col_types = do.call(cols_only, as.list(educ_load))) %>%
    transmute(census_block_group             = census_block_group, 
              pop_educ_denom                 = B15003e1,
              pop_educ_bachelors_plus        = rowSums(.[bachelors_plus_vars]),       #Sum up educational attainment variables >=bachelors
              pop_degree_denom               = B15012e1,
              pop_degree_science_engineering = rowSums(.[paste0("B15012e", 2:9)]),    #Combine several science/engineering degree variables
              pop_degree_business            = B15012e10,
              pop_degree_education           = B15012e11,
              pop_degree_arts_humanities     = rowSums(.[paste0("B15012e", 12:16)]))  #Calculate new variables
  
  df %<>% merge(education, by = "census_block_group", all = TRUE)                     #Merge new variables onto existing data frame
  return(df)
}

add_poverty <- function(df, n_max=Inf) {
  #Count for porvery share
  poverty <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_c17.csv", n_max = n_max,
                      col_types = cols_only(census_block_group = "c",
                                            C17002e1           = "d",
                                            C17002e2           = "d",
                                            C17002e3           = "d")) %>%
    transmute(census_block_group = census_block_group, 
              pop_poverty        = C17002e2 + C17002e3,
              pop_poverty_denom  = C17002e1)
  
  df %<>% merge(poverty, by = "census_block_group", all = TRUE)
  return(df)
}

add_income <- function(df, n_max=Inf) {
  #Count for share of households with income <60k or >=100k
  income_lt60k_vars     <- paste0("B19001e", 2:11)
  income_gtreq100k_vars <- paste0("B19001e", 14:17)
  income_load        <- c("c", rep.int("d", 2 + length(income_lt60k_vars) + 
                                         length(income_gtreq100k_vars)))
  names(income_load) <- c("census_block_group", "B19001e1", "B19013e1", income_lt60k_vars,
                          income_gtreq100k_vars)
  
  income <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_b19.csv", n_max = n_max,
                     col_types = do.call(cols_only, as.list(income_load))) %>%
    transmute(census_block_group = census_block_group, 
              median_inc         = B19013e1,
              pop_inc_denom      = B19001e1, 
              pop_inc_lt60k      = rowSums(.[income_lt60k_vars]),
              pop_inc_gtreq100k  = rowSums(.[income_gtreq100k_vars])) 
  
  df %<>% merge(income, by = "census_block_group", all = TRUE)
  return(df)
}

add_occupation <- function(df, n_max=Inf) {
  #Counts for occupational shares
  pop_occ_vars <- c("C24010e1", #Total
                    "C24010e3", "C24010e19", "C24010e27", "C24010e30", "C24010e34", #Men
                    "C24010e39", "C24010e55", "C24010e63", "C24010e66", "C24010e70") #Women
  to_load <- c("c", rep.int("d", length(pop_occ_vars)))
  names(to_load) <- c("census_block_group", pop_occ_vars)
  occupation <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_c24.csv",
                         n_max = n_max, col_types = do.call(cols_only, as.list(to_load))) %>%
    transmute(census_block_group = census_block_group, pop_occ_denom = C24010e1,
              pop_occ_man_bus_sci_art      = C24010e3  + C24010e39,
              pop_occ_service              = C24010e19 + C24010e55,
              pop_occ_sales_office         = C24010e27 + C24010e63,
              pop_occ_nat_constr_maint     = C24010e30 + C24010e66,
              pop_occ_production_transport = C24010e34 + C24010e70)
  
  df %<>% merge(occupation, by="census_block_group", all = TRUE)
  return(df)
}

add_vehicle_home <- function(df, n_max=Inf) {
  #Counts for share of housing units with at lest one vehicle
  vehicle_home <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_b25.csv", 
                           n_max = n_max,
                           col_types = cols_only(census_block_group = "c",
                                                 B25044e1           = "d",
                                                 B25044e3           = "d",
                                                 B25044e10          = "d",
                                                 B25003e1           = "d",
                                                 B25003e3           = "d")) %>%
    transmute(census_block_group = census_block_group,
              pop_vehicle_denom  = B25044e1,
              pop_vehicle_gtreq1 = B25044e1 - B25044e3 - B25044e10,
              pop_home_denom     = B25003e1,
              pop_home_renter    = B25003e3)
  
  df %<>% merge(vehicle_home, by="census_block_group", all = TRUE)
  return(df)
}

add_insurance <- function(df, n_max=Inf) {
  #Counts for share with insurance
  insurance <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_b27.csv", 
                        n_max = n_max,
                        col_types = cols_only(census_block_group = "c",
                                              B27010e1           = "d",
                                              B27010e17          = "d",
                                              B27010e33          = "d",
                                              B27010e50          = "d",
                                              B27010e66          = "d")) %>%
    transmute(census_block_group    = census_block_group,
              pop_insurance_denom   = B27010e1,
              pop_insurance_insured = B27010e1 - B27010e17 - B27010e33 - B27010e50 - B27010e66)
  
  df %<>% merge(insurance, by="census_block_group", all = TRUE)
  return(df)
}

add_commute <- function(df, n_max=Inf) {
  #Counts for share by means of commuting to work
  commute <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_b08.csv", n_max = n_max,
                      col_types = cols_only(census_block_group = "c",
                                            B08301e1           = "d",
                                            B08301e2           = "d",
                                            B08301e10          = "d",
                                            B08301e16          = "d",
                                            B08301e17          = "d",
                                            B08301e18          = "d",
                                            B08301e19          = "d",
                                            B08301e20          = "d",
                                            B08301e21          = "d")) %>%
    transmute(census_block_group = census_block_group,
              pop_commute_denom  = B08301e1, 
              pop_commute_auto   = B08301e2, 
              pop_commute_public = B08301e10,
              pop_commute_taxi   = B08301e16,
              pop_commute_cycle  = B08301e17 + B08301e18,
              pop_commute_walk   = B08301e19,
              pop_commute_other  = B08301e20,
              pop_commute_none   = B08301e21)
  
  df %<>% merge(commute, by="census_block_group", all = TRUE)
  return(df)
}

add_enrollment <- function(df, n_max=Inf) {
  #Counts for share currently enrolled in college
  enroll <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_b14.csv", n_max = n_max,
                     col_types = cols_only(census_block_group = "c",
                                           B14007e17          = "d",
                                           B14007e18          = "d")) %>%
    rename(pop_enroll_undergrad = B14007e17,
           pop_enroll_grad_professional = B14007e18)
  
  df %<>% merge(enroll, by = "census_block_group", all = TRUE) %>%
    mutate(pop_enroll_denom = pop) #Assumes we have already created an overall population variable
  return(df)
}

add_native <- function(df, n_max=Inf) {
  #Counts for citizenship share
  native <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_b99.csv", n_max = n_max,
                     col_types = cols_only(census_block_group = "c",
                                           B99051e1           = "d",
                                           B99051e2          = "d")) %>%
    rename(pop_citizenship_denom   = B99051e1, 
           pop_citizenship_native  = B99051e2) 
  
  df %<>% merge(native, by = "census_block_group", all = TRUE)
  return(df)
}

add_marriage <- function(df, n_max=Inf) {
  #Counts for share married
  pop_married_vars <- c("B12001e4" , "B12001e5" , "B12001e6" , "B12001e7" , "B12001e8",  # Males
                        "B12001e13", "B12001e14", "B12001e15", "B12001e16", "B12001e17") # Females
  
  to_load        <- c("c", rep.int("d", 1+length(pop_married_vars)))
  names(to_load) <- c("census_block_group", "B12001e1", pop_married_vars)
  
  
  married <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_b12.csv",
                      n_max = n_max, col_types = do.call(cols_only, as.list(to_load))) %>%
    transmute(census_block_group = census_block_group, 
              pop_marstat_denom      = B12001e1,
              pop_marstat_married    = rowSums(.[pop_married_vars]))
  
  df %<>% merge(married, by="census_block_group", all = TRUE)
  return(df)
}

add_household <- function(df, n_max=Inf) {
  #Counts for shares by household size or type
  householdsize_vars     <- paste0("B11016e", 1:16)
  household_load        <- c("c", rep.int("d", 2 + length(householdsize_vars)))
  names(household_load) <- c("census_block_group", "B11006e1", "B11006e2", householdsize_vars)
  
  household <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_b11.csv",
                        n_max = n_max, col_types = do.call(cols_only, as.list(household_load))) %>%
    transmute(census_block_group             = census_block_group,
              pop_householdage_denom         = B11006e1,
              pop_householdage_has_age60plus = B11006e2,
              pop_householdsize_denom        = B11016e1,
              pop_householdsize_1            = B11016e10,
              pop_householdsize_2            = B11016e3 + B11016e11,
              pop_householdsize_3to5         = B11016e4 + B11016e5 + B11016e6 + B11016e12 +
                B11016e13 + B11016e14,
              pop_householdsize_6plus        = B11016e7 + B11016e8 + B11016e15 + B11016e16,
              pop_householdtype_denom        = B11016e1,
              pop_householdtype_family       = B11016e2) 
  
  df %<>% merge(household, by="census_block_group", all = TRUE)
  return(df)
}

add_homelang <- function(df, n_max=Inf) { 
  #Counts for share using only English at home
  homelang <- read_csv("external/Open Census/safegraph_open_census_data/data/cbg_c16.csv",
                       n_max = n_max,
                       col_types = cols_only(census_block_group = "c",
                                             B16004e1           = "d",
                                             B16004e3           = "d",
                                             B16004e25          = "d",
                                             B16004e47          = "d")) %>%
    transmute(census_block_group         = census_block_group,
              pop_homelang_denom         = B16004e1,
              pop_homelang_english_only  = B16004e3 + B16004e25 + B16004e47)
  
  df %<>% merge(homelang, by="census_block_group", all = TRUE)
  return(df)
}

add_land_area <- function(df, n_max=Inf) {
  #Land area
  cbg_meta <- read_csv("external/Open Census/safegraph_open_census_data/metadata/cbg_geographic_data.csv",
                       n_max = n_max,
                       col_types = cols_only(census_block_group = "c",
                                             amount_land        = "d")) %>%
    transmute(census_block_group=census_block_group, land_area_sq_km=amount_land * 1e-6)
  
  df %<>% merge(cbg_meta, by = "census_block_group", all=TRUE)
  return(df)
}

calc_ratios <- function(df, remove_denoms=TRUE) {
  #Calculate ratios from count variables 
  df_names <- df %>% names()
  denoms <- df %>% select(ends_with("_denom")) %>% names() #Grab variable names of denominators
  pattern <- "(^.*)(_denom$)"                              
  prefs <- map_chr(regmatches(denoms, regexec(pattern, denoms)), ~ .[2]) #Get prefixes for each denominator
  
  for (denom_n in seq_along(denoms)) {   #For each group of variables
    pref <- prefs[denom_n]               #Get the relevant prefix
    denom <- denoms[denom_n]             #Get the relevant denominator
    numerators <- df %>% select(starts_with(!! pref)) %>% select(-(!! denom)) %>% names() #Get the relevant numerator names
    shares <- numerators %>% str_replace("^pop_", "sh_") #Get names of shares to be created
    df[shares] <- df[numerators]/df[[denom]]             #Calculate shares as numerator/denominator
    if (remove_denoms) df %<>% select(-(!! denom))       #Remove denominator variables
  }
  if(all(c("pop", "land_area_sq_km") %in% df_names)) df %<>% mutate(pop_density = pop/land_area_sq_km) #Also add population density (doesn't match other patterns)
  return(df)
}

aggregate_precinct <- function(df, remove_denoms=TRUE) {
  #Allocate CBG-level summable variables to precincts based on geographic overlap, then calculate ratios
  sum_vars <- df %>% select(starts_with("pop"), land_area_sq_km) %>% names() #Get names of summable variables
  
  overlap <- read_csv("output_local/precinct_cbg_overlap.csv")   #Load precinct-CBG overla[]
  
  df_precinct <- df %>% merge(overlap, by="census_block_group")  #Merge precinct-CBG overlap onto CBG-level data frame
  df_precinct_county_quality <- df_precinct %>%                         #Get precinct-cbg match quality measures and county associated with best matching CBG
    mutate(intersect_gtr50 = (sh_cbg>0.5 & sh_precinct>0.5),            #Indicator that given intersection accounts for more than half of land mass for both precinct and CBG
           intersect_gtr75 = (sh_cbg>0.75 & sh_precinct>0.75),          #...more than 75%...
           intersect_gtr90 = (sh_cbg>0.9 & sh_precinct>0.9)) %>%        #...more than 90%...
    group_by(state_fips, precinct_id) %>% top_n(1, wt=sh_precinct) %>%  #Get stats associated with intersection containing largest portion of each cbg
    select(state_fips, precinct_id, county, starts_with("intersect_")) %>% ungroup() #Keep only specified variables
  
  #
  df_precinct[sum_vars] <- df_precinct[sum_vars]*
    matrix(rep(df_precinct$sh_cbg, times=length(sum_vars)), ncol=length(sum_vars))   #Take CBG-level summable variables and allocate to intersection by multiplying by share of CBG area in intersection (assumes constant geographic distribution)
  
  df_precinct %<>% group_by(state_fips, precinct_id) %>%                             
    summarize_at(sum_vars, sum) %>% ungroup() %>%                                    #Sum allocated intersection values by precinct
    calc_ratios(remove_denoms = remove_denoms) %>%                                   #Calculate ratios (eg share white) from summed variables
    merge(df_precinct_county_quality, by=c("state_fips", "precinct_id")) %>%         #Merge on intersection quality measures
    select(state_fips, precinct_id, county, everything())                            #Order some variables first
  
  return(df_precinct)
}

aggregate_county <- function(df, remove_denoms=TRUE) {
  #Sum variables to county-level, then calculate ratios
  sum_vars <- df %>% select(starts_with("pop"), land_area_sq_km) %>% names() #Get names of summable variables

  df_county <- df %>% group_by(county) %>% 
    summarize_at(sum_vars, sum) %>% ungroup() %>%                           #Sum summable variables by county
    calc_ratios(remove_denoms=remove_denoms) %>%                            #Calculate ratios (eg share white) from summed variables
    select(county, everything())                                            #Order some variables first
  return(df_county)
}

merge_vote_cbg <- function(df) {
  #Merge on republican vote share allocated to census block group (from precinct)
  cbg_voting <- read_csv("output_local/cbg_voting.csv") %>%                  #Load vote share data
    filter(!is.na(r_share))                                                  #Keep only areas with valid vote shares
  df %<>% merge(cbg_voting, by="census_block_group")                         #Merge vote share onto existing data frame
  return(df)
}

merge_vote_precinct <- function(df) {
  #Merge on precinct-level republican vote share
  precinct_voting <- read_csv("output_local/precinct_voting.csv") %>%        #Load vote share data
    filter(!is.na(r_share))                                                  #Keep only areas with valid vote shares
  df %<>% merge(precinct_voting, by=c("state_fips", "precinct_id"))          #Merge vote share onto existing data frame
  return(df)
}

merge_vote_county <- function(df) {
  #Merge on county-level republican vote share
  county_voting <- read_csv("input/countypres_2000-2016.csv")       %>%      #Load raw vote data
    filter(year==2016 & party=="republican")                        %>%      #Restrict to 2016 and republican candidate columns
    mutate(county = str_pad(FIPS, 5, side="left", pad="0"))         %>%      #Get county fips code as string of length 5
    filter(str_sub(county,1,2)!="02" & str_sub(county,-3)!="000")   %>%      #Drop AK as votes correspond to districts not counties, and invalid county codes (drops Kansas City MO)
    transmute(county = county, r_share = candidatevotes/totalvotes) %>%      #Calculate republican vote share
    filter(!is.na(r_share))                                                  #Keep only areas with valid vote shares
  df %<>% merge(county_voting, by="county")        #Merge vote share onto existing data frame
  return(df)
}

main()
