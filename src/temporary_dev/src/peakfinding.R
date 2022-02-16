# Proof of concept for finding peaks
# This is for "testing how well different methods perform in anticipating peaks"
# We just need to classify certain points as peaks and check what the accuracies are at those points
# We'll approach this here via visualizations

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
library(nnfor)
library(ggpmisc)

# Load data
weekly <- read.csv(paste0(LOADPATH, "weekly_real.csv"), colClasses = c("Date", "numeric"))

# Parameters
wl <- 25
anomdist <- 2

# Get rolling median and median absolute deviation (MAD)
rollmd <- zoo::rollapply(weekly$pcs, width = wl, FUN = median, align = "center", fill = NA)
rollmad <- zoo::rollapply(weekly$pcs, width = wl, FUN = mad, align = "center", fill = NA)

# Create upper and lower bounds for madband
ub <- lowess(rollmd + anomdist * rollmad)$y
lb <- lowess(rollmd - anomdist * rollmad)$y

# Find peaks
over <- weekly[which(weekly$pcs >= ub), ]
under <- weekly[which(weekly$pcs <= lb), ]
pnv <- rbind(over, under)

# Combine all into a single df for ggplot
plotdata <- data.frame(date = weekly$date, pcs = weekly$pcs, md = rollmd, mad = rollmad, upper = ub, lower = lb)

# Plot your result as a grouped bargraph PostScript
postscript(paste0(SAVEPATH, "peakviz.eps"), width = 15, height = 4, horizontal = FALSE,
           onefile = FALSE, paper = "special", colormodel = "cmyk",
           family = "Helvetica")

ggplot(plotdata, aes(x = date, y = pcs)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
    geom_line() +
    annotate("point", x = pnv$date, y = pnv$pcs, color = "red") +
    theme_minimal() +
    labs(x = "Date",
         y = "Units per week")

dev.off()

# Compare individual method performances at peaks
forecasts <- read.csv(paste0(LOADPATH, "indv_method_forecasts_real.csv"))
forecasts$date <- as.Date(forecasts$date)

out <- ensure.window.parity(weekly, forecasts)
real <- out$first
forecasts <- out$second

APEs <- abs(forecasts[, -1] - real[, -1]) / real[, -1]; APEs <- cbind(date = real$date, APEs)
atpeaks <- APEs %>%
    filter(date %in% pnv$date)

# Compare AutoN method performances at peaks
# Load all
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

APEs <- abs(forecasts[, -1] - real[, -1]) / real[, -1]; APEs <- cbind(date = real$date, APEs)
atpeaks_fN <- APEs %>%
    filter(date %in% pnv$date)

master <- atpeaks %>%
    left_join(atpeaks_fN, by = "date")

# Now get means and test for significance
master_narm <- na.omit(master)
means <- colMeans(master_narm[, -1])
sig_mat <- as.data.frame(all_vs_all_ttest(master_narm[, -1]))
sig_mat[upper.tri(sig_mat, diag = TRUE)] <- NA

# Find significant
sigs <- which(sig_mat <= 0.05, arr.ind = T)
sdf <- data.frame(x = colnames(sig_mat[, sigs[, 2]]), y = row.names(sig_mat[sigs[, 1], ]))
