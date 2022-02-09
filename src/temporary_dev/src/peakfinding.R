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

# Load data
weekly <- read.csv(paste0(LOADPATH, "weekly_real.csv"), colClasses = c("Date", "numeric"))

# Get rolling median and median absolute deviation (MAD)
rollmd <- zoo::rollapply(weekly$pcs, width = 105, FUN = median, align = "right", fill = NA)
rollmad <- zoo::rollapply(weekly$pcs, width = 105, FUN = mad, align = "right", fill = NA)

# Create upper and lower bounds for madband
ub <- lowess(rollmd + 1.5 * rollmad)$y
lb <- lowess(rollmd - 1.5 * rollmad)$y

# Combine all into a single df for ggplot
plotdata <- data.frame(date = weekly$date, pcs = weekly$pcs, md = rollmd, mad = rollmad, upper = ub, lower = lb)


ggplot(plotdata, aes(x = date, y = pcs)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    theme_minimal() +
    labs(title = "Finding peaks using moving median absolute deviation bands",
         x = "Date",
         y = "Units")
