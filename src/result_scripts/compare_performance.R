## ---------------------------
## Script name: compare_performance.R
##
## Purpose of script: compare the performances of
##                    method selection heuristics and
##                    individual methods using both
##                    anomalous (synthetic) data and
##                    real demand data
##
## Author: Esa Turkulainen, M.Sc.
##
## Date Created: 2021-06-23
## Date Modified: 2021-07-29
##
## License: GNU General Public License v3.0
## Email: esa.turkulainen@veripalvelu.fi
##
## ---------------------------
## Notes: This script is made public as
##        a part of a publication for
##        transparency and reproducibility
##        purposes.
##
## ---------------------------

# Set working directory and paths
setwd("~/DIR/")
LOADPATH <- paste0(getwd(), "/data/")
SAVEPATH <- paste0(getwd(), "/results/")

# Load helper functions and required packages
source(paste0(getwd(), "src/helper_functions.R"))
library(ggplot2)
library(forecast)
library(lubridate)

# Load data
weekly <- read.csv(paste0(LOADPATH, "weekly_real.csv"), colClasses = c("Date", "numeric"))
synweekly <- read.csv(paste0(LOADPATH, "weekly_synthetic.csv"), colClasses = c("Date", "numeric"))

# Backtest forecast histories first on synthetic data
RUNSYNAGAIN = FALSE # TRUE -> Generate results; FALSE -> Load results
if (RUNSYNAGAIN) {
    # Set (rolling) training window size. 105 weeks is the minimum for these methods.
    rw <- 105

    # Prepare any methods for saving
    snaive <- c()
    ma5 <- c()
    ma7 <- c()
    ma9 <- c()
    ma12 <- c()
    ets <- c()
    stl <- c()
    tbats <- c()
    stlf <- c()
    arimax <- c()
    dynreg <- c()
    nn <- c()

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

    # Create a date vector for the forecasts
    fcast_dates <- seq.Date(from = weekly.sim$date[rw + 1], to = weekly.sim$date[nrow(weekly.sim)], by = "weeks")
    # Combine into a df
    forecasts_wo_avg <- data.frame(date = fcast_dates, snaive = snaive, ma5 = ma5, ma7 = ma7,
                                    ma9 = ma9, ma12 = ma12, ets = ets, stl = stl, tbats = tbats,
                                    nn = nn, arimax = arimax, dynreg = dynreg, stlf = stlf)

    # Create method average (AVG)
    avg <- apply(forecasts_wo_avg[, -1], 1, mean)
    # Add it to
    indv_forecasts_syn <- cbind(forecasts_wo_avg, avg)

    # Save
    write.csv(indv_forecasts_syn, paste0(SAVEPATH, "indv_method_forecasts_synthetic.csv"), row.names = FALSE)
    } else {
      indv_forecasts_syn <- read.csv(paste0(LOADPATH, "indv_method_forecasts_synthetic.csv"))
      indv_forecasts_syn$date <- as.Date(indv_forecasts_syn$date)
}

# Compute errors on synthetic data
# Ensure time window parity
out <- ensure.window.parity(synweekly, indv_forecasts_syn)
synseries <- out$first
fcast <- out$second

# APE = 100 * |F - A| / A
apes <- 100 * abs(fcast[, -1] - synseries[, -1]) / synseries[, -1]
colnames(apes) <- c("SNAIVE", "MA-5", "MA-7", "MA-9", "MA-12", "ETS", "STL", "TBATS", "NNAR", "ARIMAX", "DYNREG", "STLF", "AVG")

# Check overall errors
indv_syn_errors <- colMeans(apes)

# Compare errors periodically
# Mean errors first half vs last half
first <- colMeans(apes[1:(nrow(apes)/2), ])
last <- colMeans(apes[(nrow(apes)/2 + 1):nrow(apes), ])
methods <- c(names(first), names(last))
subgroup <- c(rep("First half", 13), rep("Second half", 13))
values <- c(first, last)

dat <- data.frame(methods, subgroup, values)

# Plot your result as a grouped bargraph PostScript
postscript(paste0(SAVEPATH, "half_diff.eps"), width = 8, height = 4, horizontal = FALSE,
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

# Now simulate forecasts for AutoN methods, exponentially weighted avg
# This is a bit laborious, but we run and save results for all desired val lengths
if (RUNSYNAGAIN) {

    val1 <- simulate_forecast(fcast, apes, 1)
    val3 <- simulate_forecast(fcast, apes, 3)
    val6 <- simulate_forecast(fcast, apes, 6)
    val12 <- simulate_forecast(fcast, apes, 12)
    val18 <- simulate_forecast(fcast, apes, 18)
    val24 <- simulate_forecast(fcast, apes, 24)

    write.csv(val1, paste0(SAVEPATH, "val1_fcasts_syn.csv"), row.names = FALSE)
    write.csv(val3, paste0(SAVEPATH, "val3_fcasts_syn.csv"), row.names = FALSE)
    write.csv(val6, paste0(SAVEPATH, "val6_fcasts_syn.csv"), row.names = FALSE)
    write.csv(val12, paste0(SAVEPATH, "val12_fcasts_syn.csv"), row.names = FALSE)
    write.csv(val18, paste0(SAVEPATH, "val18_fcasts_syn.csv"), row.names = FALSE)
    write.csv(val24, paste0(SAVEPATH, "val24_fcasts_syn.csv"), row.names = FALSE)

} else {

    val1 <- read.csv(paste0(LOADPATH, "val1_fcasts_syn.csv"), colClasses = c("Date", "character", "numeric"))
    val3 <- read.csv(paste0(LOADPATH, "val3_fcasts_syn.csv"), colClasses = c("Date", "character", "numeric"))
    val6 <- read.csv(paste0(LOADPATH, "val6_fcasts_syn.csv"), colClasses = c("Date", "character", "numeric"))
    val12 <- read.csv(paste0(LOADPATH, "val12_fcasts_syn.csv"), colClasses = c("Date", "character", "numeric"))
    val18 <- read.csv(paste0(LOADPATH, "val18_fcasts_syn.csv"), colClasses = c("Date", "character", "numeric"))
    val24 <- read.csv(paste0(LOADPATH, "val24_fcasts_syn.csv"), colClasses = c("Date", "character", "numeric"))

}

# Create W.AVG using apes
alpha <- 0.5
alphavec <- c()
for (k in 1:13) {
  alphavec[k] <- 0.5**k
}
w.avg <- c()
for (i in 1:(dim(fcast)[1] - 1)) {
  w.avg[i] <- sum(alphavec * fcast[, 2:14][(i + 1), order(apes[i, 1:13])])
}
wavg.errors <- 100 * abs(w.avg - synseries$pcs[-1]) / synseries$pcs[-1]

# Check errors here with mean
wavg.mape <- mean(wavg.errors)
syn.val1mape <- mean(val1$error)
# Et cetera...

## Now we have checked
## 1) How method performance behaves within the synthetic data
## 2) Does method selection help within the synthetic data
##
##
## Next, we will gauge the real world applicability of these
## by checking the performances with real data.
## We will do this in a slightly more rigorous manner:
## We will have to compute the method accuracies for each validation
## period separately, as the length of the validation period
## affects the available backtesting space we have.

# Backtest forecast histories on real data
RUNREALAGAIN = FALSE # TRUE -> Generate results; FALSE -> Load results
if (RUNREALAGAIN) {
    # Set (rolling) training window size. 105 weeks is the minimum for these methods.
    rw <- 105

    # Prepare any methods for saving
    snaive <- c()
    ma5 <- c()
    ma7 <- c()
    ma9 <- c()
    ma12 <- c()
    ets <- c()
    stl <- c()
    tbats <- c()
    stlf <- c()
    arimax <- c()
    dynreg <- c()
    nn <- c()

    for (i in 0:(nrow(weekly) - rw - 1)) {
    train_start <- weekly$date[1 + i]
    train <- subset(weekly$pcs, weekly$date >= train_start & weekly$date <= train_start + weeks(rw - 1))

    test_date <- weekly$date[rw + 1 + i]

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

    # Create a date vector for the forecasts
    fcast_dates <- seq.Date(from = weekly$date[rw + 1], to = weekly$date[nrow(weekly)], by = "weeks")
    # Combine into a df
    forecasts_wo_avg <- data.frame(date = fcast_dates, snaive = snaive, ma5 = ma5, ma7 = ma7,
                                    ma9 = ma9, ma12 = ma12, ets = ets, stl = stl, tbats = tbats,
                                    nn = nn, arimax = arimax, dynreg = dynreg, stlf = stlf)

    # Create method average (AVG)
    avg <- apply(forecasts_wo_avg[, -1], 1, mean)
    # Add it to
    indv_forecasts <- cbind(forecasts_wo_avg, avg)

    # Save
    write.csv(indv_forecasts, paste0(SAVEPATH, "indv_method_forecasts_real.csv"), row.names = FALSE)
    } else {
      indv_forecasts <- read.csv(paste0(LOADPATH, "indv_method_forecasts_real.csv"))
      indv_forecasts$date <- as.Date(indv_forecasts$date)
}

# Compute method errors
out <- ensure.window.parity(weekly[, -1], indv_forecasts)
real <- out$first
fcast <- out$second

apes <- 100 * abs(fcast[, -1] - real$pcs) / real$pcs

# Now simulate AutoN
if (RUNREALAGAIN) {

    val1 <- simulate_forecast(fcast, apes, 1)
    val3 <- simulate_forecast(fcast, apes, 3)
    val6 <- simulate_forecast(fcast, apes, 6)
    val12 <- simulate_forecast(fcast, apes, 12)
    val18 <- simulate_forecast(fcast, apes, 18)
    val24 <- simulate_forecast(fcast, apes, 24)

    write.csv(val1, paste0(SAVEPATH, "val1_fcasts_real.csv"), row.names = FALSE)
    write.csv(val3, paste0(SAVEPATH, "val3_fcasts_real.csv"), row.names = FALSE)
    write.csv(val6, paste0(SAVEPATH, "val6_fcasts_real.csv"), row.names = FALSE)
    write.csv(val12, paste0(SAVEPATH, "val12_fcasts_real.csv"), row.names = FALSE)
    write.csv(val18, paste0(SAVEPATH, "val18_fcasts_real.csv"), row.names = FALSE)
    write.csv(val24, paste0(SAVEPATH, "val24_fcasts_real.csv"), row.names = FALSE)

} else {

    val1 <- read.csv(paste0(LOADPATH, "val1_fcasts_real.csv"), colClasses = c("Date", "character", "numeric"))
    val3 <- read.csv(paste0(LOADPATH, "val3_fcasts_real.csv"), colClasses = c("Date", "character", "numeric"))
    val6 <- read.csv(paste0(LOADPATH, "val6_fcasts_real.csv"), colClasses = c("Date", "character", "numeric"))
    val12 <- read.csv(paste0(LOADPATH, "val12_fcasts_real.csv"), colClasses = c("Date", "character", "numeric"))
    val18 <- read.csv(paste0(LOADPATH, "val18_fcasts_real.csv"), colClasses = c("Date", "character", "numeric"))
    val24 <- read.csv(paste0(LOADPATH, "val24_fcasts_real.csv"), colClasses = c("Date", "character", "numeric"))

}

# Create W.AVG using apes
alpha <- 0.5
alphavec <- c()
for (k in 1:13) {
  alphavec[k] <- 0.5**k
}
w.avg <- c()
for (i in 1:(dim(fcast)[1] - 1)) {
  w.avg[i] <- sum(alphavec * fcast[, 2:14][(i + 1), order(apes[i, 1:13])])
}
wavg.errors <- 100 * abs(w.avg - real$pcs[-1]) / real$pcs[-1]

# Find theoretical minimum error for method selection
minimum <- c()
min.errors <- c()
optimal.scheme <- c()
for (i in 1:dim(fcast)[1]) {
    min.errors[i] <- min(apes[i, 1:13])
    min.method <- which.min(apes[i, 1:13])
    optimal.scheme[i] <- names(apes[, 1:13])[min.method]
    minimum[i] <- fcast[, 2:14][i, min.method]
}

# Because the length of the validation period decreases the number
# of forecasts we can generate, we need to evaluate all methods
# multiple times (for each validation width).
# The APEs are dropped from the beginning. Auto-N is always N forecasts
# shorter than the full APE set.

# Columns (cover all indv methods)
i1 <- val_errors_indv(apes, 1)
i3 <- val_errors_indv(apes, 3)
i6 <- val_errors_indv(apes, 6)
i12 <- val_errors_indv(apes, 12)
i18 <- val_errors_indv(apes, 18)
i24 <- val_errors_indv(apes, 24)

sm <- cbind(i1, i3, i6, i12, i18, i24); rownames(sm) <- c("min", names(apes), "w.avg")

# AutoN methods as rows to be rbound
val.levels <- c(1, 3, 6, 12, 18, 24)

a1 <- val_errors_auto(val1, 1, val.levels)
a3 <- val_errors_auto(val3, 3, val.levels)
a6 <- val_errors_auto(val6, 6, val.levels)
a12 <- val_errors_auto(val12, 12, val.levels)
a18 <- val_errors_auto(val18, 18, val.levels)
a24 <- val_errors_auto(val24, 24, val.levels)

fm <- rbind(sm, a1, a3, a6, a12, a18, a24)

# Save
if (RUNREALAGAIN) {
    write.csv(fm, paste0(SAVEPATH, "acc.csv"))
}


# Not provided: Heatmap visualization in the paper was created using the 'pheatmap' package.
