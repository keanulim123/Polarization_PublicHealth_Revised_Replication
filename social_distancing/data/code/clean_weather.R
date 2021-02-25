CRAN_packages <- c("readr", "dplyr", "magrittr", "purrr", "stringr", "lubridate")
lapply(CRAN_packages, require, character.only = TRUE)

main <- function() {
  keep_dates <- seq(ymd("2020/01/27"), ymd("2020/07/12"), "1 day")
  keep_dates_placebo <- keep_dates - 7*52
  keep_dates_other <- seq(ymd("2020/01/01"), ymd("2020/01/26"), "1 day")
  keep_dates <- c(keep_dates, keep_dates_placebo, keep_dates_other)
  
  year_suffs <- c("2019" = "_2019",
                  "2020" = "_2020-01-01_yesterday")
  overlap <- read_csv("output_local/precinct_cbg_overlap.csv")
  overlap_states <- unique(overlap$state_fips)
  
  for (j in seq_along(year_suffs)) {
    year_suff <- year_suffs[j]
    save_county(year_suff, keep_dates)
    weather_cbg <- save_cbg(year_suff, keep_dates)
    weather_cbg %<>% filter(str_sub(census_block_group, 1, 2) %in% overlap_states)
      
    save_precinct(year_suff, weather_cbg, overlap)
    rm(weather_cbg)
    gc()
  }
  return(NULL)
}

save_county <- function(year_suff, keep_dates) {
  to_load_county <-
    list(county = "c", date = col_date(format = ""), precip = "d", tmin = "d", tmax = "d")
  
  read_csv(sprintf("external/Weather/weather_county%s.csv.gz", year_suff),
           col_types = do.call(cols_only, to_load_county)) %>%
    mutate(county = str_pad(county, width = 5, side = "left", pad = "0")) %>%
    filter(date %in% keep_dates) %>%
    write_csv(sprintf("output_local/weather_county%s.csv", names(year_suff)))

  return(NULL)
}

save_cbg <- function(year_suff, keep_dates) {
  to_load_cbg <-   
    list(geoid = "c", date = col_date(format = ""), precip = "d", tmin = "d", tmax = "d")
  weather_cbg <- read_csv(sprintf("external/Weather/weather_cbg%s.csv.gz", year_suff),
                          col_types = do.call(cols_only, to_load_cbg)) %>%
    rename(census_block_group=geoid) %>%
    mutate(census_block_group = str_pad(census_block_group, width = 12, side = "left", pad = "0")) %>%
    filter(date %in% keep_dates)
  
  #weather_cbg %>% write_csv(sprintf("output_local/weather_cbg%s.csv", names(year_suff))) #CBG level dataset not currently used
  return(weather_cbg)
}

save_precinct <- function(year_suff, weather_cbg, overlap) {
  dates <- weather_cbg %>% use_series(date) %>% unique()
  append <- FALSE
  for (i in seq_along(dates)) {
    print(dates[i])
    weather_cbg %>% filter(date==dates[i]) %>% 
      merge(overlap, by="census_block_group") %>%
      group_by(state_fips, precinct_id, date) %>%
      summarize_at(vars(precip, tmin, tmax, precip), 
                   ~ weighted.mean(., sh_precinct, na.rm=TRUE)) %>%
      ungroup() %>%
      write_csv(sprintf("output_local/weather_precinct%s.csv", names(year_suff)),
                append=append)
    weather_cbg %<>% filter(date!=dates[i])
    append <- TRUE
  }
  return(NULL)
}

main()
