rm(list = ls())
CRAN_packages <- c("dplyr", "readr", "magrittr", "lubridate")
lapply(CRAN_packages, require, character.only = TRUE)

main <- function() {
  state_county <- clean_state_county()
  state_county %>% save_state()
  state_county %>% save_county()
  return(NULL)
}

clean_state_county <- function() {
  state_county <- read_csv("input/full_policy_data.csv",
                           col_types=cols_only(state_fips = "c", county_fips="c", effective_date_sip="c",
                                               geography="c")) %>%
    mutate(effective_date_sip = mdy(ifelse(is.na(effective_date_sip), NA,
                                           paste0(effective_date_sip,"/2020"))))
  return(state_county)
}

save_state <- function(state_county) {
  state <- state_county %>% filter(!is.na(state_fips) & is.na(county_fips))
  stopifnot(all(state$geography=="state"))
  state %<>% select(-c(county_fips, geography)) %>%
    rename(effective_date_sip_state=effective_date_sip)
  state %>% write_csv("output/state_policies.csv")
  return(NULL)
}

save_county <- function(state_county) {
  state_county %>%
    select(-geography) %>%
    filter(!is.na(county_fips)) %>%
    rename(effective_date_sip_county=effective_date_sip) %>%
    write_csv("output/county_policies.csv")
  return(NULL)
}

main()

  