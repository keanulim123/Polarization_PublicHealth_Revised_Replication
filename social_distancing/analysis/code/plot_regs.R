CRAN_packages <- c("ggplot2", "scales", "lubridate")
lapply(CRAN_packages, require, character.only = TRUE)

ylim_l_default <- -0.43
ylim_u_default <-  1.15

main <- function() {
  dates <- seq(ymd("2020-01-27"), ymd("2020-07-06"), "1 week")
  dates_plac <- dates - 7*52
  week_dates <- format(dates, format = "%b %d")
  week_dates_plac <- format(dates_plac, format = "%b %d")
  
  n_weeks <- length(week_dates) - 1
  n_days  <- length(week_dates) * 7

  ## Make Main County-Weekly POI-level Plots
  regs <- read.delim("output/county_week.txt", sep = "\t", skip = 1, header = F, as.is = T)
  for (i in 1:ncol(regs)) {
  make_plot(regs[, i], week_dates, filename = sprintf("output/county_week_%d.pdf", i),
            vars = n_weeks, XLAB = "Week")
  }

  ## Make Social Distancing Plots (County)
  regs <- read.delim("output/county_week_alt.txt", sep = "\t", skip = 1, header = F)
  outcomes <- c("log_destination_cbgs_not_self", "log_devices_outside_home", "log_median_away_sg",
    "share_leave_home", "share_leave_home_alt", "share_leave_home_candidate", "log_device_count", "log_destination_cbgs_sum",
     "log_destination_cbgs_self")
  colnames(regs) <- paste(rep(outcomes, each=3), rep(1:3, time = length(outcomes)), sep = "_")
  
  i <- 1
  for (v in outcomes) {
    for (n in 1:3) {
      col <- paste(v, n, sep="_")
      if (v %in% c("log_destination_cbgs_not_self", "log_devices_outside_home") & n==3) {
        L <- -.1
        U <- 0.4
      } else if (grepl("^share_", v)) {
        L <- -0.03
        U <- 0.15
      } else if (v=="log_median_away_sg" & n==3) {
        L <- -0.2
        U <- 0.9
      } else {
        L <- ylim_l_default
        U <- ylim_u_default
      }
      make_plot(regs[, i], week_dates, 
                filename = sprintf("output/county_week_alt_%s.pdf", col),
                vars = n_weeks, XLAB = "Week", ylim_l = L, ylim_u = U)
      i <- i + 1
    }
  }

  ## Make County-Weekly POI-level Plots [ROBUSTNESS]
  regs <- read.delim("output/county_week_robustness.txt", sep = "\t", skip = 1, header = F, as.is = T)
  
  for (i in 1:ncol(regs)) {
    if (i %in% 7:9) {
      ylim_l <- -.03
      ylim_u <- .085
    } else {
      ylim_l <- ylim_l_default
      ylim_u <- ylim_u_default
    }
    start <- if (i==18) 2 else 1
    make_plot(regs[, i],  week_dates, 
              filename = sprintf("output/county_week_robustness_%d.pdf", i),
              vars = n_weeks, XLAB = "Week", ylim_l = ylim_l, ylim_u = ylim_u, start=start)
  }

  ## Make County-Weekly POI-level Plots (PLACEBO)
  regs <- read.delim("output/county_week_plac.txt", sep = "\t", skip = 1, header = F, as.is = T)
  for (i in 1:ncol(regs)) {
    make_plot(regs[, i], week_dates_plac, filename = sprintf("output/county_week_plac_%d.pdf", i),
              vars = n_weeks, XLAB = "Week")
  }
  
  ## Make County-Daily POI-level Plots
  regs <- read.delim("output/county_day.txt", sep = "\t", skip = 1, header = F, as.is =T)
  regs <- rbind(matrix(NA, nrow=2, ncol=ncol(regs)), regs)
  day_dates <- min(dates) + c(0:n_days)-1
  make_plot(regs[, 1], day_dates, filename = "output/county_day.pdf", vars = n_days,
            XLAB = "Day", width=8, height=4)
  
  ## Make County-Weekly POI-level Plots by Industry
  regs <- read.delim("output/countyindustry_week.txt", sep = "\t", skip = 1, header = F, as.is = T)
  colnames(regs) <- c("retail_trade", "entertainment", "accomod_and_food", "health_care", "other_industries")
  for (i in 1:ncol(regs)) {
    make_plot(regs[, i], week_dates, 
              filename = sprintf("output/countyindustry_week_%s.pdf", colnames(regs)[i]),
                                 vars = n_weeks, XLAB = "Week")
  }

  ## Make POI Plots (Precinct)
  regs <- read.delim("output/precinct_week.txt", sep = "\t", skip = 1, header = F)
  for (i in 1:ncol(regs)) {
    make_plot(regs[, i], week_dates, filename = sprintf("output/precinct_week_%d.pdf", i),
              vars = n_weeks, XLAB = "Week")
  }

  ## Make Alternative SD Plots (Precinct)
  regs <- read.delim("output/precinct_week_alt.txt", sep = "\t", skip = 1, header = F)
  i <- 1
  for (v in c("log_destination_cbgs_not_self", "log_devices_outside_home")) {
    if (v %in% c("log_destination_cbgs_not_self", "log_devices_outside_home")) {
      L <- -.1
      U <- 0.4
    } else if (grepl("^share_", v)) {
      L <- -0.1
      U <- 0.15
    } else if (v=="log_median_away_sg") {
      L <- -0.2
      U <- 0.9
    }
    make_plot(regs[, i], week_dates, filename = sprintf("output/precinct_week_alt_%s_4.pdf", v), vars = n_weeks,
              XLAB = "Week", ylim_l = L, ylim_u = U)
    i <- i + 1
  }

  ## Make POI Plots (Precinct, Placebo)
  regs <- read.delim("output/precinct_week_plac.txt", sep = "\t", skip = 1, header = F)
  for (i in 1:ncol(regs)) {
    make_plot(regs[, i], week_dates_plac, 
              filename = sprintf("output/precinct_week_plac_%d.pdf", i),
              vars = n_weeks, XLAB = "Week")
  }
}

make_plot <- function(reg, dates, filename = "", vars = 7, number_zeros = 1, XLAB = "", events = c(), event_labs = c(),
                      ylim_l = ylim_l_default, ylim_u = ylim_u_default, weekend = F, start = 1,
                      width=6, height=4){
  reg   <- as.numeric(as.character(reg))
  if (vars > 50) reg[is.na(reg)] <- 0
  coefs <- reg[seq(1, vars * 2, by = 2)]
  ses   <- reg[seq(2, vars * 2 + 1, by = 2)]
  
  if (start == 2){
    coefs <- coefs[-1]
    ses   <- ses[-1]
    zeros <- matrix(c(c(NA, 0), c(NA, 0)), nrow = 2)
  } else {
    zeros <- matrix(rep(c(0, 0), number_zeros), nrow = number_zeros)
  }
    plot_data <- as.data.frame(rbind(zeros, cbind(coefs, ses)))
    plot_data$lower <- plot_data$coefs - 1.96 * plot_data$ses
    plot_data$upper <- plot_data$coefs + 1.96 * plot_data$ses
    
  if (vars > 50){
    plot_data$time <- factor(format(dates, "%b %d"), levels = format(dates, "%b %d"))
    plot_data$Weekend <- factor(as.numeric(weekdays(dates) %in% c("Sunday", "Saturday")))
  } else {
    plot_data$time <- factor(dates, levels = dates)  # https://sebastiansauer.github.io/ordering-bars/
  }
  
  # Plot
  zp1 <- ggplot(plot_data)
  zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2))
  
  if (length(events) > 0){
    for (i in 1:length(events)){
      e <- events[i]
      l <- event_labs[i]
      zp1 <- zp1 + geom_vline(xintercept = e, colour = "grey", linetype = "dashed") + # https://stackoverflow.com/questions/18091721/align-geom-text-to-a-geom-vline-in-ggplot2
        annotate(geom = "text", x = e, y = .5 + (i == 2) * .1, label = l, colour = "black") 
    }
  }
  
  if (vars > 50){
    zp1 <- zp1 + geom_errorbar(aes(ymin = lower, ymax = upper, x = time), width = .1, size = .6, position = position_dodge(width = 1/4), colour = "grey70")
    zp1 <- zp1 + geom_point(aes(x = time, y = coefs, col = Weekend), shape = 20, size = 2) +
      scale_color_manual(values = c("red", "black"))
  } else {
    zp1 <- zp1 + geom_errorbar(aes(ymin = lower, ymax = upper, x = time), width = .13, size = .8, position = position_dodge(width = 1/4), colour = "grey70")
    zp1 <- zp1 + geom_point(aes(x = time, y = coefs), colour = "red", shape = 20, size = 3.5)
  }
  zp1 <- zp1 + theme_bw() + xlab("") + ylab("Partisan Difference") + xlab(XLAB)
  zp1 <- zp1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text=element_text(size=11), axis.text.x=element_text(angle = -60, hjust = 0)) # https://stackoverflow.com/questions/11748384/formatting-dates-on-x-axis-in-ggplot2
  zp1 <- zp1 + ylim(ylim_l, ylim_u)
  if (vars > 50){
    idxs <- c(rep(F, 6), rep(c(T, rep(F, 9)), 25))[1:length(plot_data$time)]
    zp1 <- zp1 + scale_x_discrete(breaks = levels(plot_data$time)[idxs]) # https://stackoverflow.com/questions/31732597/how-to-not-show-all-labels-on-ggplot-axis
  }
  ggsave(zp1, filename = filename, width = width, height = height)
}

main()

