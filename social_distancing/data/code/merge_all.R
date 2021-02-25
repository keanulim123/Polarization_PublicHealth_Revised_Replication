CRAN_packages <- c("readr", "dplyr", "lubridate", "magrittr", "tidyr", "stringr")
lapply(CRAN_packages, require, character.only = TRUE)

# https://stackoverflow.com/questions/51295402/r-on-macos-error-vector-memory-exhausted-limit-reached
# Implement following:
#   `cd ~`
#   `touch .Renviron`
#   `open .Renviron`
# Then add the following: `R_MAX_VSIZE=100Gb`
Sys.setenv('R_MAX_VSIZE'=32000000000)

main <- function(){
  # Dates
  dates <- seq(ymd("2020-01-27"), ymd("2020-07-06"), "1 week") #Weekly dates starting with beginning of study
  dates_plac   <- dates - 7*52
  dates_plus   <- seq(ymd("2020-01-20"), max(dates), "1 week")
  
  # Clean dataset by countyXweek, including daily social distancing
  clean_data(out_filename = "county_week_data", input_data = "county_daily",
             dates = dates,  geo_vars = "county", time_var = "week",
             add_distancing = T)

  # Clean dataset by countyXweek, lagged one year (placebo)
  clean_data(out_filename = "county_week_plac_data", input_data = "county_daily",
             dates = dates_plac, geo_vars = "county", time_var = "week",
             add_distancing = F)

  # Clean dataset by countyXday
  clean_data(out_filename = "county_day_data", input_data = "county_daily",
             dates = dates_plus, geo_vars = "county", time_var = "day",
             add_distancing = F, relative_to = dates[1], min_time = -1)

  # Clean dataset by countyXday, lagged one year (placebo)
  clean_data(out_filename = "county_day_plac_data", input_data = "county_daily",
             dates = dates_plac, geo_vars = "county", time_var = "day",
             add_distancing = F)

  # Clean dataset by countyXindustryXweek
  clean_data(out_filename = "countyindustry_week_data", input_data = "county_industry_daily",
             dates = dates, geo_vars = c("county", "industry"), time_var = "week",
             add_distancing = F)
  
  # Clean dataset by precinctXweek, including social distancing
  clean_data(out_filename = "precinct_week_data", input_data = "prec_daily",
             dates = dates, geo_vars = c("state_fips", "precinct_id"),
             time_var = "week", add_distancing = T)
  
  # Clean dataset by precinctXweek, lagged one year (placebo)
  clean_data(out_filename = "precinct_week_plac_data", input_data = "prec_daily",
             dates = dates_plac, geo_vars = c("state_fips", "precinct_id"),
             time_var = "week", add_distancing = F)
}

clean_data <- function(out_filename, input_data, dates, geo_vars, time_var, 
                       add_distancing, relative_to = NULL, min_time = -Inf) {
  if (is.null(relative_to)) relative_to <- dates[1]
  year <- year(relative_to)
  geo <- ifelse("county" %in% geo_vars, "county", "precinct")
  geodate <- c(geo_vars, "date")
  
  data <- load_and_rbind(input_data, dates) %>%
    mutate(date = as.Date(date) + day - 1) %>%
    select(-day) %>%
    complete(nesting(!!! syms(geo_vars)), date, fill=list(visits = 0))
  
  if (add_distancing) {
    distancing <- read_csv(sprintf("output_local/daily_distancing_%s.csv", geo))
    distancing_summable <- distancing %>% 
      select(-all_of(geodate), -starts_with("avg_")) %>% names()
    fill0 <- as.list(rep.int(0, time=length(distancing_summable)))
    names(fill0) <- distancing_summable
    distancing %<>% complete(nesting(!!! syms(geo_vars)), date, fill=fill0)
    
    data %<>% merge(distancing, by=geodate, all = T)
    rm(distancing)
  } else {
    distancing_summable <- NULL
  }
  
  ## Add time-varying controls
  # Add Weather
  weather <- read_csv(sprintf("output_local/weather_%s%d.csv", geo, year))
  weather_vars <- weather %>% select(-any_of(geodate)) %>% names()
  data %<>% merge(weather, all.x = T)
  rm(weather)
  
  if (geo=="precinct") { #Need to first merge on county in order to include some other controls
    county <- read_csv("output_local/precinct_demographics.csv",
                       col_types = cols_only(state_fips = "c", precinct_id = "d",
                                             county = "c")) %>%
      distinct()
    data %<>% merge(county)
  }
  
  if (geo=="county") data %<>% mutate(state_fips = str_sub(county, 1, 2))
  
  if (year==2020) {
    # Add county-covid cases
    nyt_covid <- read_csv("input/nyt_covid_us_counties.csv") %>%
      select(date, county=fips, cases, deaths)
    data %<>% merge(nyt_covid, by=c("date", "county"), all.x = T) %>%
      replace_na(replace = list(cases = 0, deaths = 0))
    rm(nyt_covid)
    
    # Add Policies
    county_policies <- read_csv("output/county_policies.csv") %>%
      select(county=county_fips, effective_date_sip_county)
    data %<>% merge(county_policies, by = "county", all.x = TRUE)
    
    state_policies <- read_csv("output/state_policies.csv")
    data %<>% merge(state_policies, by = "state_fips", all.x = TRUE)
    
    data %<>% mutate(county_sip = 1*(date >= pmin(effective_date_sip_county, effective_date_sip_state,
                                                  max(data$date)+1, na.rm = T))) %>%
      select(-c(effective_date_sip_county, effective_date_sip_state))
    rm(county_policies, state_policies)
  }
  
  # Get relative date
  data %<>% mutate(day = as.numeric(date - as.Date(relative_to)))
  
  if (time_var == "week") {
    id_vars <- c(geo_vars, time_var)
    sum_vars <- c("visits", distancing_summable)
    avg_vars <- data %>% select(starts_with("avg_"),
                                any_of(c(weather_vars, "county_sip"))) %>%
      names()
    data %<>% mutate(week = floor(day / 7)) %>%
      select(-day) %<>%
      group_by(!!! syms(id_vars))
    
    data_summ <- data %>%
      summarize_at(vars(all_of(c(sum_vars, avg_vars))), sum) %>% ungroup() %>%
      mutate_at(vars(all_of(avg_vars)), .funs = list(~ ./7))
    
    first_vars <- c("date", "cases", "deaths", "state_fips")
    data %<>% select(!!! syms(id_vars), any_of(first_vars)) %>%
      slice_min(1, order_by = date, n = 1) %>% ungroup() %>%
      merge(data_summ) %>%
      select(!!! syms(geodate), week, everything())
  }
  
  data %<>% rename(time = {{time_var}}) %>%
    filter(time >= min_time)
  
  # Add Open Census County Controls and Vote Share
  demos <- read_csv(sprintf("output_local/%s_demographics.csv", geo))
  to_remove <- demos %>% select(starts_with("pop_")) %>% select(-pop_density) %>% names()
    demos %<>% select(-all_of(to_remove))
  
  data %<>% merge(demos)
  
  # Drop Alaska due to district-level voting issue
  data %<>% filter(state_fips != "02") #Already dropped Alaska when running add_county_controls()
  
  # Geographic exceptions in NYT data
  # Drop NYC boroughs (New York, Kings, Queens, Bronx, and Richmond --- these are collapsed into a single value for NYC in the NYT data)
  # Drop Kansas city counties (Cass, Clay, Jackson, and Platte --- cases from Kansas city are not being attributed to them)
  nyc  <- c("36061", "36047", "36081", "36005", "36085")
  kc   <- c("29037", "29047", "29095", "29165")
  data %<>% filter(!(county %in% c(nyc, kc)))
  
  # Save
  write_csv(data, sprintf("output/%s.csv", out_filename))
}

load_and_rbind <- function(prefix, dates) {
  data  <- NULL
  for (i in seq_along(dates)) {
    temp <- read_csv(sprintf("output_local/%s_%s.csv", prefix, dates[i])) %>%
      mutate(date = dates[i])
    
    data %<>% bind_rows(temp)
  }
  return(data)
}

# RUN
main()
