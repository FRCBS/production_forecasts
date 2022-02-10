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

# Compare performances at peaks
forecasts <- read.csv(paste0(LOADPATH, "indv_method_forecasts_real.csv"))
forecasts$date <- as.Date(forecasts$date)

out <- ensure.window.parity(weekly, forecasts)
real <- out$first
forecasts <- out$second

APEs <- abs(forecasts[, -1] - real[, -1]) / real[, -1]; APEs <- cbind(date = real$date, APEs)
atpeaks <- APEs %>%
    filter(date %in% pnv$date)

names(atpeaks[-1])[apply(atpeaks[, -1], 1, which.min)]

# TODO: Apply to AutoN also
# TODO: Compute means for everybody
