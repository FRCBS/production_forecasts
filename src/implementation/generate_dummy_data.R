## ---------------------------
## Script name: generate_dummy_data.R
##
## Purpose of script: Generates and saves fake data
##                    using makeTSnoise() from the dse
##                    package.
##
## Author: Esa Turkulainen, M.Sc.
##
## Date Created: 2021-08-23
##
## License: GNU General Public License v3.0
## Email: esa.turkulainen@veripalvelu.fi
##
## ---------------------------
## Notes: This script is made public as
##        a companion piece for practical
##        implementation of methods explored
##        in a publication.
##
## ---------------------------

# Load packges
library("dse")

# Set wd
setwd("~/production_forecasts/dummy_data/")

# Generate
data <- makeTSnoise(530, 1, lags = 52, start = as.Date("2010-01-01"), frequency = 52)

# Data is now between [-2, 2]. Scale to something reasonable-ish.
temp1 <- 750 * data$w
temp2 <- temp1 + abs(min(temp1))

# Create a saveable frame
df <- data.frame(seq.Date(from = as.Date("2010-01-01"), along.with = temp2, by = "week"), temp2)
colnames(df) <- c("date", "pcs")

# Save
write.csv(df, "dummy_weekly_red.csv", row.names = FALSE)
