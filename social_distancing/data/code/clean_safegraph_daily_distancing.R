CRAN_packages <- c("dplyr", "readr", "magrittr", "stringr", "lubridate", "jsonlite", "tidyr", "purrr")
lapply(CRAN_packages, require, character.only = TRUE)

main <- function() {
  dates <- seq(ymd("2020/01/27"), ymd("2020/07/12"), "1 day")
  make_county   <- TRUE  #Produce county aggregates (in addition to CBG level)
  make_precinct <- TRUE  #Produce precinct aggregates (in addition to CBG level)
  
  df <- NULL
  append <- FALSE
  print("Starting to loop through CBG dates")
  for (i in seq_along(dates)) {
    print(dates[i])
    df_dt <- unzip_load(date=dates[i])
    if ("destination_cbgs" %in% names(df_dt)) df_dt %<>% flatten_destination_cbgs()
    df %<>% bind_rows(df_dt)
    rm(df_dt)
    append <- TRUE
  }

  df %>% write_csv("output_local/daily_distancing_cbg.csv")
  
  avg_vars <- df %>% select(starts_with("median_")) %>% names()
  sum_vars <- c("device_count", "completely_home_device_count",
                "candidate_device_count", "destination_cbgs_sum", "destination_cbgs_self") %>%
    intersect(names(df))
  
  df %<>% select(origin_census_block_group, date, all_of(c(avg_vars, sum_vars))) %>%
    group_by(origin_census_block_group) %>% filter(n()==length(dates)) %>% ungroup() #Balance panel prior to aggregation
  
  if (make_county) {
    df %>% aggregate_county(sum_vars=sum_vars, avg_vars=avg_vars) %>%
      write_csv("output_local/daily_distancing_county.csv") #Call common aggregation function between county and precinct
  }
  
  if (make_precinct) {
    df %>% aggregate_precinct(sum_vars=sum_vars, avg_vars=avg_vars) %>%
      write_csv("output_local/daily_distancing_precinct.csv")
  }
  return(NULL)
}

unzip_load <- function(date) {
  year <- toString(year(date))
  month <- str_pad(month(date), 2, pad="0")
  day <- str_pad(day(date), 2, pad="0")
  file <- sprintf("external/Daily Social Distancing/%s/%s/%s/%s-%s-%s-social-distancing.csv.gz",
                  year, month, day, year, month, day)
  df <- read_csv(file, col_types= #If we decide to stop loading variable, remove variable line below
                   cols_only(origin_census_block_group = col_character(),
                             device_count = col_double(),
                             completely_home_device_count = col_double(),
                             destination_cbgs = col_character(),
                             delivery_behavior_devices = col_double(),
                             median_non_home_dwell_time = col_double(),
                             candidate_device_count = col_double())) %>%
    mutate(date = ymd(sprintf("%s-%s-%s", year, month, day))) %>%
    filter(origin_census_block_group!="190570010001") #Remove sink: https://safegraphcovid19.slack.com/archives/C0109NPA543/p1585848524364400
  stopifnot(n_distinct(df$origin_census_block_group)==nrow(df))
  return(df)
}

flatten_destination_cbgs <- function(df) {
  df %<>% replace_na(list("destination_cbgs" = "{}")) %>%
    mutate(destination_cbgs = map(destination_cbgs, ~ fromJSON(.))) %>%
    mutate(destination_cbgs_sum = map_dbl(destination_cbgs, ~ sum(unlist(.))),
           destination_cbgs_self = map2_dbl(destination_cbgs, origin_census_block_group,
                                            ~  max(0,.x[[.y]]))) %>%
    select(-destination_cbgs)
  return(df)
}

aggregate_county <- function(df, sum_vars, avg_vars) {
  df %<>% mutate(county = str_sub(origin_census_block_group, 1, 5)) %>%
    aggregate_helper(group_vars = quos(county, date), sum_vars = sum_vars, avg_vars = avg_vars) 
  return(df)
}

aggregate_precinct <- function(df, sum_vars, avg_vars) {
  print("Starting precinct aggregation")
  overlap <- read_csv("output_local/precinct_cbg_overlap.csv")
  dates <- df %>% use_series(date) %>% unique() %>% sort()
  
  df_precinct <- NULL
  for (i in seq_along(dates)) {
    print(dates[i])
    df_dt <- df %>% filter(date==dates[i]) %>% 
      merge(overlap, by.x="origin_census_block_group", by.y="census_block_group") %>%
      mutate_at(vars(all_of(sum_vars)), .funs = list(~ .*sh_cbg)) %>%
      aggregate_helper(group_vars = quos(state_fips, precinct_id, date), 
                       sum_vars = sum_vars, avg_vars = avg_vars)
    
    df_precinct %<>% bind_rows(df_dt)
    df %<>% filter(date!=dates[i])
  }
  return(df_precinct)
}

aggregate_helper <- function(df, group_vars, sum_vars, avg_vars) {
  df %<>% mutate_at(avg_vars, .funs = list(~ .*device_count)) %>%
    group_by(!!! group_vars) %>%
    summarize_at(c(sum_vars, avg_vars), sum) %>%
    ungroup() %>%
    mutate_at(avg_vars, .funs = list(~ ./device_count)) %>%
    rename_at(avg_vars, .funs = list(~ paste0("avg_", .))) %>%
    select(!!! group_vars, everything())
  return(df)
}

main()