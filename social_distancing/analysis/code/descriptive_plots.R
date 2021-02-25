CRAN_packages <- c("ggplot2", "lubridate", "readr", "dplyr", "magrittr", "tidyr")
lapply(CRAN_packages, require, character.only = TRUE)
source("../lib/write_gslab_table.R")

main <- function(){
  # Real
  data <- read_csv("input/county_day_data.csv",
                   col_types = cols_only(county = "c", time = "d", r_share = "d", visits = "d",
                                         cases = "d", deaths = "d", pop = "d"))
  r_share_cutoff <- data %>% use_series(r_share) %>% median()
  data %>% make_descriptive_plots(relative_to = "2020-01-27")
  rm(data)
  
  # Placebo
  data_plac <- read_csv("input/county_day_plac_data.csv",
                        col_types = cols_only(county = "c", time="d", r_share="d", visits="d"))
  r_share_cutoff_plac <- data_plac %>% use_series(r_share) %>% median()
  data_plac %>% make_descriptive_plots(relative_to = "2019-01-28", suffix = "_plac")
  
  # Save table for text.pdf
  write_gslab_table(as.matrix(c(r_share_cutoff, r_share_cutoff_plac)), "output/r_share_cutoff.txt", "<tab:r_share_cutoff>")
}

make_descriptive_plots <- function(data, relative_to, suffix = ""){
  data %<>% clean_descriptives(relative_to, suffix)
  
  # Plot POIs
  zp2 <- data %>% filter(Outcome == "Visits (millions)") %>% 
    ggplot(aes(x = date, y = y, col = Group))
  zp2 <- zp2 + geom_line() + scale_color_manual(values = c("blue3", "orangered1"))
  zp2 <- zp2 + theme_bw() + ylab("") + xlab("Week")
  zp2 <- zp2 + scale_x_date(breaks = unique(data$date), date_labels = "%b %d") # https://stackoverflow.com/questions/30018187/changing-tick-intervals-when-x-axis-values-are-dates
  
  zp2 <- zp2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text=element_text(size = 11), axis.text.x = element_text(angle = -60, hjust = 0)) # https://stackoverflow.com/questions/11748384/formatting-dates-on-x-axis-in-ggplot2
  ggsave(zp2, filename = sprintf("output/trends_overtime_poi%s.pdf", suffix), height = 4, width = 8)
  
  if (suffix!="_plac") {
    # Plot COVID-19
    zp2 <- data %>% filter(Outcome %in% c("Cases (per 100,000 people)", "Deaths (per 1,000,000 people)")) %>% 
      ggplot(aes(x = date, y = y, col = Group, lty = Outcome))
    zp2 <- zp2 + geom_line() + scale_color_manual(values = c("blue3", "orangered1"))
    zp2 <- zp2 + theme_bw() + ylab("") + xlab("Week")
    zp2 <- zp2 + scale_x_date(breaks = unique(data$date), date_labels = "%b %d") # https://stackoverflow.com/questions/30018187/changing-tick-intervals-when-x-axis-values-are-dates
    
    zp2 <- zp2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank(), axis.line = element_line(colour = "black"),
                       axis.text=element_text(size = 11), axis.text.x = element_text(angle = -60, hjust = 0)) # https://stackoverflow.com/questions/11748384/formatting-dates-on-x-axis-in-ggplot2
    ggsave(zp2, filename = sprintf("output/trends_overtime_covid%s.pdf", suffix), height = 4, width = 8)
    
  }
}


clean_descriptives <- function(data, relative_to, suffix){
  data %<>% mutate(group = ifelse(r_share > quantile(r_share, probs = c(.5)),
                                  "Republican counties", "Democratic counties"),
                   week = floor(time / 7)) %>%
    filter(week>=0)

  combined <- data %>%
    group_by(group, week) %>% summarize(visits = sum(visits) / 1000000) %>% ungroup() %>%
    arrange(group, week) %>% group_by(group) %>% mutate(visits = visits/visits[1]) %>% ungroup() %>%
    mutate(date = ymd(relative_to) + 7 * week)
    
  if (suffix!="_plac") {
    health <- data %>% group_by(county, week, group) %>% summarize_at(vars(cases, deaths, pop), .funs = max) %>%
      group_by(week, group) %>% summarize_at(vars(cases, deaths, pop), .funs = sum) %>% ungroup() %>%
      mutate(cases = 1e5 * cases / pop, deaths = 1e6 * deaths / pop)
    combined %<>% merge(health, by = c("week", "group"))
  }
  
  combined %<>% rename(Group = group) %>%
    pivot_longer(cols=any_of(c("visits", "cases", "deaths")), names_to = "Outcome", values_to = "y") %>%
    mutate(Outcome = ifelse(Outcome == "visits", "Visits (millions)",
                     ifelse(Outcome == "cases", "Cases (per 100,000 people)",
                     ifelse(Outcome == "deaths", "Deaths (per 1,000,000 people)", "ERROR"))))
  
  return(combined)
}


# RUN
main()
