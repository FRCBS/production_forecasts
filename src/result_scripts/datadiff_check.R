## ---------------------------
## Script name: datadiff_check.R
##
## Purpose of script: The FRCBS changed data sources
##                    in sales reporting at some point,
##                    which introduced some missing data
##					  into the series. This script was
##					  written to examine the issue.
##
## Author: Esa Turkulainen, M.Sc.
##
## Date Created: 2021
## Date Modified: 2021-08-03
##
## License: GNU General Public License v3.0
## Email: esa.turkulainen@veripalvelu.fi
##
## ---------------------------
## Notes: This script is made public as
##        part of a publication, for
##		  documentation and transparency
##		  purposes.
## ---------------------------


# Load
longform <- read.csv("~/data/monthly_real_2004.csv",
                     colClasses = c("Date", "numeric", "NULL"))
normform <- read.csv("~/data/monthly_real_2014.csv",
                     colClasses = c("Date", "NULL", "NULL", "numeric"))

# Set window
longform <- longform[longform$date >= "2014-01-01", ]
normform <- normform[normform$date <= "2020-07-01", ]

# Diff
difframe <- data.frame(date = longform$date,
                       diff = abs(longform$red - normform$red.sales))

# Plot
library(ggplot2)
ggplot(difframe, aes(x = date)) +
  geom_line(aes(y = diff)) +
  labs(title = "Absolute differences in datasets from the same period",
       subtitle = "4k unit spike likely affects fits") +
  theme_minimal()
