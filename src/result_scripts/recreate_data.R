# Recreate dataset for reviewer comments

# Load helper functions and required packages
source("~/prodfore_review_edits/src/pffunctions.R")
#source(paste0(getwd(), "/src/helper_functions.R"))
library(data.table)
library(lubridate)
library(forecast)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load un-aggregated real data
FACSDIR <- "/home/esa/prodfore_review_edits/data/FACS/"
DWDIR <- "/home/esa/prodfore_review_edits/data/DWSALES/"
daily_FACS <- read_FACS_red(FACSDIR)
daily_DWSALES <- read_DW_red(DWDIR)

append_time_series <- function(x, y) {
  # Assumes first column is date
  last_of_first <- x[nrow(x), 1]
  appendage <- y[(y[, 1] > last_of_first), ]
  result <- rbind(x, appendage)
  return(result)
}

daily <- append_time_series(daily_FACS, daily_DWSALES)

write.csv(daily, "~/prodfore_review_edits/data/daily_red.csv", row.names = F)

## Below are fixes that need to be made to the data, but they are applied in create_synthetic_demand.R, which expects daily data "as is"

# # Jan 1 2014 is a Wednesday, so we want Jan 6 2014
# series <- daily[daily$date >= as.Date("2014-01-06"), ]
#
# # Impute near zeros with last week's corresponding value
# for (i in 1:nrow(series)) {
#     ifelse(series$pcs[i] == 0, series$pcs[i] <- series$pcs[i - 7], series$pcs[i] <- series$pcs[i])
# }


