## ---------------------------
## Script name: create_synthetic_demand.R
##
## Purpose of script: model an abnormal segment of a time series, noise it,
##                    and overwrite part of the real time series to extend
##                    the abnormal behavior
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

# Load data (resolution: daily)
daily <- read.csv(LOADPATH)

# Format data. Below assumes following column order: 1. date, 2. units
daily[, 1] <- as.Date(daily[, 1])
daily[, 2] <- as.numeric(daily[, 2])
colnames(daily) <- c("date", "pcs")

# Ensure we only have full weeks (first observation Monday, last Sunday)
wdaynum <- as.POSIXlt(daily[1, 1])$wday
wday_shift <- ifelse(wdaynum == 1, 0, (8 - wdaynum))
mon <- daily[(1 + wday_shift):nrow(daily), ]
wdaynum <- as.POSIXlt(mon[nrow(mon), 1])$wday
wday_shift <- ifelse(wdaynum == 7, 0, (wdaynum - 1))
monsun <- mon[1:(nrow(mon) - wday_shift), ]

# Impute near zeros with last week's corresponding value
# KNOW YOUR DATA BEFORE IMPUTING/DROPPING
for (i in 1:nrow(monsun)) {
    ifelse(monsun$pcs[i] == 0, monsun$pcs[i] <- monsun$pcs[i - 7], monsun$pcs[i] <- monsun$pcs[i])
}

# Aggregate FROM daily TO weekly
weekly <- daily_to_weekly(series)

# Now save weekly demand into a file for further analyses
write.csv(weekly, paste0(SAVEPATH, "weekly_real.csv"), row.names = FALSE)

# Plot your time series as PostScript
# Open EPS device
postscript(paste0(SAVEPATH, "real_demand.eps"), width = 15, height = 4, horizontal = FALSE,
           onefile = FALSE, paper = "special", colormodel = "cmyk",
           family = "Helvetica") # Helvetica is proprietary! May cause issues in later editing
                                 # if not installed.

# Plot
p <- ggplot(data = weekly, aes(x = date, y = pcs)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Date",
       y = "Units per week") +
  xlim(as.Date("2012-01-01"), as.Date("2022-01-01")) # Adjust as necessary
p
# Save
dev.off()

# In our research, we noticed the emergence of an anomalous pattern around 2018 and 2019.
# Code below will use these dates. Adjust as necessary.

# Extract the period of interest (2017-07-03 -- 2019-07-01)
start_date <- as.Date("2017-07-03")
end_date <- as.Date("2019-07-01")
slice <- weekly[weekly$date >= start_date & weekly$date <= end_date, ]

# Create a time series object (required by the 'forecast' package)
slice.ts <- ts(slice$pcs, start = c(year(start_date), month(start_date), day(start_date)),
               end = c(year(end_date), month(end_date), day(end_date)), frequency = 52)

# We chose the multi-seasonal ETS modeler (STLF) offered by 'forecast' to model this segment.
# Many others are available for the curious.
# Fit STLF to the extract
fit <- stlf(slice.ts)

# Generate future using fit
sim.future <- forecast(fit, h = 104)$mean # h = 104 is approx. 2 years of weekly data

# Add noise
set.seed(23062021) # For reproducibility
noise <- rnorm(104) * median(slice$pcs) * 0.025
sim.future.noised <- sim.future + noise

# Add dates (continue from end of segment)
sim.future.noised.df <- data.frame(date = seq.Date(from = as.Date("2019-07-08"), to = as.Date("2019-07-08") + weeks(103), by = "weeks"),
                                   pcs = sim.future.noised)

# Now replace the end of real with this generated data
weekly.sim <- rbind(weekly[weekly$date < as.Date("2019-07-08"), 2:3], sim.future.noised.df)

# Plot your synthesized time series as PostScript
# Open EPS device
postscript(paste0(SAVEPATH, "synthetic_demand.eps"), width = 15, height = 4, horizontal = FALSE,
           onefile = FALSE, paper = "special", colormodel = "cmyk",
           family = "Helvetica")

# Check
p2 <- ggplot(data = weekly.sim, aes(x = date, y = pcs)) +
    geom_line() +
    theme_minimal() +
    labs(x = "Date",
         y = "Units per week") +
    geom_vline(xintercept = as.Date("2019-07-08"), linetype = "dotted",
               color = "grey50", size = 1) # Indicate the beginning of fake data.
p2
# Save plot
dev.off()

# Now save synthetic weekly demand into a file for further analyses
write.csv(weekly.sim, paste0(SAVEPATH, "weekly_synthetic.csv"), row.names = FALSE)
