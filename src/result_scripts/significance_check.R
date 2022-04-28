# Test significance of accuracy results

# Set working directory and paths
setwd("~/prodfore_publ/")
LOADPATH <- paste0(getwd(), "/results/")
SAVEPATH <- paste0(getwd(), "/results/")

# Load helper functions and required packages
source(paste0(getwd(), "/src/helper_functions.R"))
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)

# Load data
weekly <- read.csv(paste0(LOADPATH, "weekly_real.csv"), colClasses = c("Date", "numeric"))
synthetic <- read.csv(paste0(LOADPATH, "weekly_synthetic.csv"), colClasses = c("Date", "numeric"))

# Load indv forecasts
forecasts <- read.csv(paste0(LOADPATH, "indv_method_forecasts_real.csv"))
forecasts$date <- as.Date(forecasts$date)

# Compute errors
out <- ensure.window.parity(weekly, forecasts)
real <- out$first
forecasts <- out$second

APEs <- abs(forecasts[, -1] - real[, -1]) / real[, -1]; APEs <- cbind(date = real$date, APEs)

# Load AutoN
f1 <- read.csv(paste0(LOADPATH, "val1_fcasts_real.csv"), colClasses = c("Date", "factor", "numeric", "numeric"))
f3 <- read.csv(paste0(LOADPATH, "val3_fcasts_real.csv"), colClasses = c("Date", "factor", "numeric", "numeric"))
f6 <- read.csv(paste0(LOADPATH, "val6_fcasts_real.csv"), colClasses = c("Date", "factor", "numeric", "numeric"))
f12 <- read.csv(paste0(LOADPATH, "val12_fcasts_real.csv"), colClasses = c("Date", "factor", "numeric", "numeric"))
f18 <- read.csv(paste0(LOADPATH, "val18_fcasts_real.csv"), colClasses = c("Date", "factor", "numeric", "numeric"))
f24 <- read.csv(paste0(LOADPATH, "val24_fcasts_real.csv"), colClasses = c("Date", "factor", "numeric", "numeric"))

# Create W.AVG using apes
alpha <- 0.5
alphavec <- c()
for (k in 1:15) {
  alphavec[k] <- 0.5**k
}
w.avg <- c()
for (i in 1:(dim(forecasts)[1] - 1)) {
  w.avg[i] <- sum(alphavec * forecasts[, 2:16][(i + 1), order(APEs[i, 1:15])])
}

# Combine
fN <- f1[, c(1, 3)] %>%
    left_join(f3[, c(1, 3)], by = "date") %>%
    left_join(f6[, c(1, 3)], by = "date") %>%
    left_join(f12[, c(1, 3)], by = "date") %>%
    left_join(f18[, c(1, 3)], by = "date") %>%
    left_join(f24[, c(1, 3)], by = "date")
colnames(fN) <- c("date", "f1", "f3", "f6", "f12", "f18", "f24")
fN$wavg <- w.avg

out <- ensure.window.parity(weekly, fN)
real <- out$first
forecasts <- out$second

APEs_fN <- abs(forecasts[, -1] - real[, -1]) / real[, -1]; APEs_fN <- cbind(date = real$date, APEs_fN)

# Combine with indv
master <- APEs %>% left_join(APEs_fN, by = "date")

# Compute means and significances
master_narm <- na.omit(master)
means <- colMeans(master_narm[, -1])
sig_mat <- as.data.frame(all_vs_all_ttest(master_narm[, -1]))
sig_mat[upper.tri(sig_mat, diag = TRUE)] <- NA

# Find significant
sigs <- which(sig_mat <= 0.05, arr.ind = T)
sdf <- data.frame(x = colnames(sig_mat[, sigs[, 2]]), y = row.names(sig_mat[sigs[, 1], ]))

# For the real data, the only differentiating factor seems to be the ML methods MLP and ELM.
# They are bad enough that their badness statistically significant compared to other methods.

## Now repeat for synth

# Load indv forecasts
forecasts <- read.csv(paste0(LOADPATH, "indv_method_forecasts_synthetic.csv"))
forecasts$date <- as.Date(forecasts$date)

# Compute errors
out <- ensure.window.parity(synthetic, forecasts)
real <- out$first
forecasts <- out$second

APEs <- abs(forecasts[, -1] - real[, -1]) / real[, -1]; APEs <- cbind(date = real$date, APEs)

# Load AutoN
f1 <- read.csv(paste0(LOADPATH, "val1_fcasts_syn.csv"), colClasses = c("Date", "factor", "numeric", "numeric"))
f3 <- read.csv(paste0(LOADPATH, "val3_fcasts_syn.csv"), colClasses = c("Date", "factor", "numeric", "numeric"))
f6 <- read.csv(paste0(LOADPATH, "val6_fcasts_syn.csv"), colClasses = c("Date", "factor", "numeric", "numeric"))
f12 <- read.csv(paste0(LOADPATH, "val12_fcasts_syn.csv"), colClasses = c("Date", "factor", "numeric", "numeric"))
f18 <- read.csv(paste0(LOADPATH, "val18_fcasts_syn.csv"), colClasses = c("Date", "factor", "numeric", "numeric"))
f24 <- read.csv(paste0(LOADPATH, "val24_fcasts_syn.csv"), colClasses = c("Date", "factor", "numeric", "numeric"))

# Create W.AVG using apes
alpha <- 0.5
alphavec <- c()
for (k in 1:15) {
  alphavec[k] <- 0.5**k
}
w.avg <- c()
for (i in 1:(dim(forecasts)[1] - 1)) {
  w.avg[i] <- sum(alphavec * forecasts[, 2:16][(i + 1), order(APEs[i, 1:15])])
}

# Combine
fN <- f1[, c(1, 3)] %>%
    left_join(f3[, c(1, 3)], by = "date") %>%
    left_join(f6[, c(1, 3)], by = "date") %>%
    left_join(f12[, c(1, 3)], by = "date") %>%
    left_join(f18[, c(1, 3)], by = "date") %>%
    left_join(f24[, c(1, 3)], by = "date")
colnames(fN) <- c("date", "f1", "f3", "f6", "f12", "f18", "f24")
fN$wavg <- w.avg

out <- ensure.window.parity(synthetic, fN)
real <- out$first
forecasts <- out$second

APEs_fN <- abs(forecasts[, -1] - real[, -1]) / real[, -1]; APEs_fN <- cbind(date = real$date, APEs_fN)

# Combine with indv
master <- APEs %>% left_join(APEs_fN, by = "date")

# Compute means and significances
master_narm <- na.omit(master)
means <- colMeans(master_narm[, -1])
sig_mat <- as.data.frame(all_vs_all_ttest(master_narm[, -1]))
sig_mat[upper.tri(sig_mat, diag = TRUE)] <- NA

# Find significant
sigs <- which(sig_mat <= 0.05, arr.ind = T)
sdf <- data.frame(x = colnames(sig_mat[, sigs[, 2]]), y = row.names(sig_mat[sigs[, 1], ]))
