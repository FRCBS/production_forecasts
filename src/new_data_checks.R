# Check DW_SALES data

library(data.table)
library(dplyr)
library(lubridate)
library(readr)

## Read, define columns, omit last column
data <- fread("/home/esa/production_forecasts/data/DWSALES/2021-03-16.dat",
              colClasses = c("Date", "numeric", "factor", "character", "character", "NULL"))

## Filter to red cells, rename columns
red.codes <- c("budTR001", "A0071V00", "A0074V00", "A0092VA0", "A0092VB0", 
               "E3844V00", "E3845V00", "E3846VA0", "E3846VB0", "E3846VC0",
               "E3846V00", "E3847VA0", "E3847VB0", "E3847VC0", "E3847V00",
               "E3936VA0", "E3936VB0", "E3939V00", "E3940V00", "E4683V00",
               "E7668V00", "E7673V00", "E4999V00", "E5000V00")

# Dataset filtered by codes
redbycodes <- data %>%
  filter(V4 %in% red.codes ) %>%
  select(c("date" = V1, "pcs" = V2, "type" = V3)) %>%
  arrange(date)

# Dataset filtered by tags
redbytag <- data %>%
  filter(V5 == "Punasoluvalmiste") %>%
  select(c("date" = V1, "pcs" = V2, "type" = V3)) %>%
  arrange(date)

red <- redbytag

# Are they equivalent?
all(redbycodes == redbytag) # returns TRUE

## Monthly aggregation
red.monthly <- aggregate(pcs ~ month(date) + year(date), data = red, FUN = sum)

## Test continuity
any(is.na(data)) # returns FALSE, no NAs

## Daily aggregation
red.daily <- aggregate(pcs ~ day(date) + month(date) + year(date), data = red, FUN = sum)
cont_time <- seq.Date(from = red$date[1], to = red$date[length(red$date)], by = "day")
length(cont_time) == length(red.daily$pcs) # returns TRUE, data has no missing days, missing rows impossible to test for

## Save short aggregate splice
cont_time_monthly <- seq.Date(from = red$date[1], to = red$date[length(red$date)], by = "month")
red.save <- red.monthly %>%
  select(c(pcs))
red.save <- cbind(red.save, cont_time_monthly)[, c(2, 1)] %>%
  select(c("date" = cont_time_monthly, pcs))
# Write to csv
write_csv(red.save, "/home/esa/production_forecasts/data/new_monthly.csv")

## Save the entirety of the monthly series
long <- read_csv("/home/esa/production_forecasts/data/monthly_real_2004.csv", 
                 col_types = "Dn_",
                 col_names = T) %>%
  select(c("date", "pcs" = red))

# Ensure they match on overlapping months
newoverlap <- red.save[red.save$date >= "2020-06-05", "pcs"]
oldoverlap <- long[long$date >= "2020-06-01", "pcs"]
newoverlap - oldoverlap # they do not match :(

# Still, we want to glue them together
amendment <- red.save[red.save$date > "2020-07-05", ] %>% # We assume that the last month of the old data is a full month as it is only missing 35 pcs
  transmute("date" = date - days(4), pcs)
# Save
rbind(long, amendment) %>%
  write_csv("/home/esa/production_forecasts/data/red_full.csv")
