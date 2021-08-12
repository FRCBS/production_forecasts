# ---------------------------
##
## Script name: create_synthetic_demand.R
##
## Purpose of script: model an abnormal segment of a time series, noise it,
##                    and overwrite part of the real time series to extend
##                    the abnormal behavior
##
## Author: Esa Turkulainen, M.Sc.
##
## Date Created: 2021
## Date Modified: 2021-07-29
##
## License: GNU General Public License v3.0
## Email: esa.turkulainen@veripalvelu.fi
##
## ---------------------------
##
## Notes: This script is made public as
##        a part of a publication for
##        transparency and reproducibility
##        purposes.
##
## ---------------------------


# Load helper functions and required packages
source("~/production_forecasts/src/pffunctions.R")
library(data.table)
library(lubridate)
library(forecast)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load un-aggregated real data
FACSDIR <- "/home/esa/production_forecasts/data/FACS/"
DWDIR <- "/home/esa/production_forecasts/data/DWSALES/"
daily_FACS <- read_FACS_red(FACSDIR)
daily_DWSALES <- read_DW_red(DWDIR)

append_time_series <- function(x, y, sum.overlap = FALSE) {
  # Assumes first column is date
  # TODO: Check which first
  # TODO: sum.overlap = TRUE
  last_of_first <- x[nrow(x), 1]
  appendage <- y[(y[, 1] > last_of_first), ]
  result <- rbind(x, appendage)
  return(result)
}

daily <- append_time_series(daily_FACS, daily_DWSALES, sum.overlap = FALSE)

# Aggregate weekly
# Start aggregation from Monday
# To ensure we get subseries specifically between Mon-Sun
# And not some other 7 day interval

# Jan 1 2014 is a Wednesday, so we want Jan 6 2014
series <- daily[daily$date >= as.Date("2014-01-06"), ]

# Impute near zeros with last week's corresponding value
for (i in 1:nrow(series)) {
    ifelse(series$pcs[i] == 0, series$pcs[i] <- series$pcs[i - 7], series$pcs[i] <- series$pcs[i])
}


pcslist <- c(); weeklist <- c(); datelist <- c()  # Create storage lists
j <- 0; k <- 0  # j is for resetting week counter, k is for gathering dates
for (i in seq(to = (nrow(series) - 7), by = 7)) {
    if (j == 52) {j <- 0}
    j <- j + 1
    k <- k + 1
    pcslist[k] <- sum(series$pcs[i:(i + 6)])
    weeklist[k] <- j
    datelist[k] <- series$date[i]
}

weekly <- data.frame(week = weeklist, date = as.Date.numeric(datelist, origin = "1970-01-01"), pcs = pcslist)

# If value under 2000, add 1000 (anomaly fix)
for (i in 1:nrow(weekly)) {
    ifelse(weekly$pcs[i] < 2000, weekly$pcs[i] <- (weekly$pcs[i] + 1000), weekly$pcs[i] <- weekly$pcs[i])
}

# Open EPS device
postscript("weekly_real_FI.eps", width = 15, height = 4, horizontal = FALSE,
           onefile = FALSE, paper = "special", colormodel = "cmyk",
           family = "Helvetica")

# Plot
p <- ggplot(data = weekly, aes(x = date, y = pcs)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Date",
       y = "Units per week") +
  xlim(as.Date("2014-01-01"), as.Date("2021-06-01")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())
p
# Save
dev.off()

# We notice the emergence of a striking pattern around 2018 and 2019
# If we artificially continue it to the future, we may be able to
# demonstrate how the preferred method changes with this behavior change

# Extract the period of interest (2017-07-03 -- 2019-07-01)
start_date <- as.Date("2017-07-03")
end_date <- as.Date("2019-07-01")
slice <- weekly[weekly$date >= start_date & weekly$date <= end_date, ]
slice.ts <- ts(slice$pcs, start = c(year(start_date), month(start_date), day(start_date)),
               end = c(year(end_date), month(end_date), day(end_date)), frequency = 52)

# We can't include the generating model to the comparison!
# Fit STLF to the extract
fit <- stlf(slice.ts)

# Generate future using fit
sim.future <- forecast(fit, h = 104)$mean


# Add noise
set.seed(23062021)
noise <- rnorm(104) * median(slice$pcs) * 0.025
sim.future.noised <- sim.future + noise

# Add dates
sim.future.noised.df <- data.frame(date = seq.Date(from = as.Date("2019-07-08"), to = as.Date("2019-07-08") + weeks(103), by = "weeks"),
                                   pcs = sim.future.noised)

# Now replace the end of real with this generated data
weekly.sim <- rbind(weekly[weekly$date < as.Date("2019-07-08"), 2:3], sim.future.noised.df)

# Open EPS device
postscript("weekly_sim_FI.eps", width = 15, height = 4, horizontal = FALSE,
           onefile = FALSE, paper = "special", colormodel = "cmyk",
           family = "Helvetica")

# Check
p2 <- ggplot(data = weekly.sim, aes(x = date, y = pcs)) +
    geom_line() +
    theme_minimal() +
    labs(x = "Date",
         y = "Units per week") +
    geom_vline(xintercept = as.Date("2019-07-08"), linetype = "dotted", color = "grey50", size = 1)
p2
# Save
dev.off()

RUNAGAIN = FALSE
if (RUNAGAIN) {
  # Now, to determine if this change differentiates between methods, we run rolling forecasts
  # through this series and see what happens to the errors. 105 weeks is the minimum for these methods.
  rw <- 105

  # Prepare for saving
  snaive <- c()
  ma5 <- c()
  ma7 <- c()
  ma9 <- c()
  ma12 <- c()
  tbats <- c()
  stlf <- c()
  arimax <- c()
  dynreg <- c()
  nn <- c()
  ets <- c()
  stl <- c()

  for (i in 0:(nrow(weekly.sim) - rw - 1)) {
  train_start <- weekly.sim$date[1 + i]
  train <- subset(weekly.sim$pcs, weekly.sim$date >= train_start & weekly.sim$date <= train_start + weeks(rw - 1))

  test_date <- weekly.sim$date[rw + 1 + i]

  # Make train set into a ts object for forecast functions
  train_ts <- ts(train, start = c(year(train_start), month(train_start), day(train_start)), frequency = 52)

  # Create xregs for ARIMAX
  nrow <- length(train)
  onehotyear <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                     ncol = 11,
                     byrow = TRUE)

  month.m <- matrix(, nrow = nrow, ncol = 11)
  for (j in 1:nrow) {
      month.m[j,] <- onehotyear[month(start_date + weeks(j - 1)), ]
  }
  horizon.m <- matrix(, nrow = 1, ncol = 11)
  k <- 0
  for (j in nrow:nrow) {
      k <- k + 1
      horizon.m[k,] <- onehotyear[month(start_date + weeks(j)), ]
  }

  # Seasonal naive
  fit <- snaive(train_ts)
  snaive[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

  # MA5
  fit <- ma(train_ts, order = 5)
  ma5[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

  # MA7
  fit <- ma(train_ts, order = 7)
  ma7[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

  # MA9
  fit <- ma(train_ts, order = 9)
  ma9[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

  # M12
  fit <- ma(train_ts, order = 12)
  ma12[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

  # TBATS
  fit <- tbats(train_ts)
  tbats[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

  # STLF
  fit <- stlf(train_ts)
  stlf[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

  # ARIMAX
  fit <- auto.arima(train_ts, xreg = month.m)
  arimax[i + 1] <- as.numeric(forecast(fit, xreg = horizon.m, h = 1)$mean)

  # DYNREG
  fit1 <- tslm(train_ts ~ trend + season)
  fcast1 <- forecast(fit1, h = 1)

  fit2 <- auto.arima(fit1$residuals)
  fcast2 <- forecast(fit2, h = 1)

  y <- as.numeric(fcast1$mean)
  x <- as.numeric(fcast2$mean)
  dynreg[i + 1] <- (x + y)

  # NN
  fit <- nnetar(train_ts)
  nn[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

  # ETS
  fit <- ets(train_ts)
  ets[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

  # STL
  fit <- stl(train_ts, s.window = "periodic", t.window = 7)
  stl[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)
  }

  fcast_dates <- seq.Date(from = weekly.sim$date[rw + 1], to = weekly.sim$date[nrow(weekly.sim)], by = "weeks")
  single_histories_wo_avg <- data.frame(date = fcast_dates, snaive = snaive, ma5 = ma5, ma7 = ma7,
                                        ma9 = ma9, ma12 = ma12, ets = ets, stl = stl, tbats = tbats,
                                        nn = nn, arimax = arimax, dynreg = dynreg, stlf = stlf)

  avg <- apply(single_histories_wo_avg[, -1], 1, mean)
  single_histories <- cbind(single_histories_wo_avg, avg)

  write.csv(single_histories, "~/weekly_rw105_FI_final.csv", row.names = FALSE)
} else {
      single_histories <- read.csv("~/weekly_rw105_FI_final.csv")
      single_histories$date <- as.Date(single_histories$date)
}

# Get errors

# Ensure time window parity
ensure.window.parity <- function(x, y) {
  # Assumes date column is first
  if (x[1, 1] > y[1, 1]) start <- x[1, 1] else start <- y[1, 1]
  if (x[nrow(x), 1] < y[nrow(y), 1]) end <- x[nrow(x), 1] else end <- y[nrow(y), 1]
  x <- x[x[, 1] >= start & x[, 1] <= end, ]
  y <- y[y[, 1] >= start & y[, 1] <= end, ]

  return(list(first = x, second = y))
}

out <- ensure.window.parity(weekly.sim, single_histories)

real <- out$first
fcast <- out$second

apes <- 100 * abs(fcast[, -1] - real[, -1]) / real[, -1]; colnames(apes) <- c("SNAIVE", "MA-5", "MA-7", "MA-9", "MA-12", "ETS", "STL", "TBATS", "NNAR", "ARIMAX", "DYNREG", "STLF", "AVG")
# Plot
ggplot(data = real, aes(x = date, y = pcs)) +
    geom_line() +
    geom_line(data = fcast, aes(x = date, y = ma12), color = "red")

# Compare errors periodically
# Mean errors first half vs last half
first <- colMeans(apes[1:(nrow(apes)/2), ])
last <- colMeans(apes[(nrow(apes)/2 + 1):nrow(apes), ])
methods <- c(names(first), names(last))
subgroup <- c(rep("First half", 13), rep("Second half", 13))
values <- c(first, last)

dat <- data.frame(methods, subgroup, values)
# Errors grow: MA_5-12, ETS (chck this again), TBATS, ARIMAX
# Errors decrease: SNAIVE, STL, NN, DYNREG, STLF
postscript("half_diff.eps", width = 8, height = 4, horizontal = FALSE,
           onefile = FALSE, paper = "special", colormodel = "cmyk",
           family = "Helvetica")

ggplot(dat, aes(fill = subgroup, y = values, x = methods)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  labs(y = "MAPE (%)", fill = "Time window") +
  scale_x_discrete(limits = c("ETS", "MA-12", "MA-5", "MA-7", "MA-9", "ARIMAX", "TBATS",
                              "NNAR",  "AVG", "SNAIVE", "STL", "DYNREG", "STLF")) +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 0.9),
        legend.key.size = unit(0.1, "cm"), legend.title = element_text(size = 10)) +
  scale_fill_manual(values = c("grey10", "grey40"))

dev.off()


# What does method selection do here?

# Define function for creating the AutoN histories from single method histories
simulate_forecast <- function(real, singles, val){
  # Trim real
  realt <- subset(real, real$date >= singles$date[1] & real$date <= tail(singles$date, 1))
  # Errors
  errdf <- 100 * abs(realt$pcs - singles[, 2:13]) / realt$pcs # Selection 2:13 assume that 1st col is date, and no "comb"

  selected_method <- c()
  forecast <- c()
  date <- c()
  for(i in 1:(dim(errdf)[1] - val)){
    # Select method
    meanerr <- colMeans(errdf[i:(i + val - 1), 1:12])
    selected_method[i] <- names(errdf[, 1:12])[which.min(meanerr)]
    # Fetch forecast of selected method at i + val (- 1 + 1)
    forecast[i] <- singles[i + val, selected_method[i]]
    date[i] <- realt$date[i + val]
  }
  df <- data.frame(date = as.Date(date, origin = "1970-01-01"), method = selected_method, forecast = forecast)
  return(df)
}

val1 <- simulate_forecast(weekly.sim, single_histories, 1)
val3 <- simulate_forecast(weekly.sim, single_histories, 3)
val6 <- simulate_forecast(weekly.sim, single_histories, 6)
val12 <- simulate_forecast(weekly.sim, single_histories, 12)
val18 <- simulate_forecast(weekly.sim, single_histories, 18)
val24 <- simulate_forecast(weekly.sim, single_histories, 24)

# Create exp.avg using apes
alpha <- 0.5
alphavec <- c()
for(k in 1:13){
  alphavec[k] <- 0.5**k
}

exp.avg <- c()
for(i in 1:(dim(fcast)[1] - 1)){
  exp.avg[i] <- sum(alphavec * fcast[, 2:14][(i + 1), order(apes[i, 1:13])])
}

eavg.errors <- 100 * abs(exp.avg - real$pcs[-1]) / real$pcs[-1]

# Force parity to each error test
out1 <- ensure.window.parity(weekly.sim, val1[,c(1, 3)])
real1 <- out1$first
fcast1 <- out1$second
ape1 <- 100 * abs(fcast1[, -1] - real1[, -1]) / real1[, -1]

out3 <- ensure.window.parity(weekly.sim, val3[,c(1, 3)])
real3 <- out3$first
fcast3 <- out3$second
ape3 <- 100 * abs(fcast3[, -1] - real3[, -1]) / real3[, -1]

out6 <- ensure.window.parity(weekly.sim, val6[,c(1, 3)])
real6 <- out6$first
fcast6 <- out6$second
ape6 <- 100 * abs(fcast6[, -1] - real6[, -1]) / real6[, -1]

out12 <- ensure.window.parity(weekly.sim, val12[,c(1, 3)])
real12 <- out12$first
fcast12 <- out12$second
ape12 <- 100 * abs(fcast12[, -1] - real12[, -1]) / real12[, -1]

out18 <- ensure.window.parity(weekly.sim, val18[,c(1, 3)])
real18 <- out18$first
fcast18 <- out18$second
ape18 <- 100 * abs(fcast18[, -1] - real18[, -1]) / real18[, -1]

out24 <- ensure.window.parity(weekly.sim, val24[,c(1, 3)])
real24 <- out24$first
fcast24 <- out24$second
ape24 <- 100 * abs(fcast24[, -1] - real24[, -1]) / real24[, -1]

# Create weighted average
exp.avg <- c()
for(i in 1:(dim(fcast)[1] - 1)){
  exp.avg[i] <- sum(alphavec * fcast[, 2:14][(i + 1), order(apes[i, 1:13])])
}
eavg.errors <- 100 * abs(exp.avg - real$pcs[-1]) / real$pcs[-1]
