Demand Lab: Red Cells (Ketju)
================

``` r
# Set working directory
knitr::opts_knit$set(root.dir = "/home/esa/production_forecasts") # Working on Ubuntu
# knitr::opts_knit$set(root.dir = "V:/production_forecasts") # Working home
```

## Create original datasets that should remain immutable throughout labbing

``` r
library(forecast)
library(ggplot2)
library(gridExtra)
library(knitr)
library(readxl)
library(plyr)
library(lubridate)
source("src/evalhelp.R")

# Load data
# All deliveries
deliv <- read_excel("./data/ketju_data.xlsx", sheet = "Ketju-punasolutoimitukset 2014-")[, c('Päivämäärä', 'Toimitukset')]
colnames(deliv) <- c("time", "deliveries")  # Change column names
deliv$time <- as.Date(deliv$time)


# Ketju usage 2014 -->
usage <- read.csv("./data/ketju_data.csv", header = TRUE, sep = ",", colClasses=c("NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA, "NULL", NA))
colnames(usage) <- c("time", "pcs")  # Change column names
usage$time <- mdy(usage$time)
usage <- arrange(usage, time)
usage <- aggregate(usage$pcs, by = list(usage$time), sum); colnames(usage) <- c("time", "pcs")



# # HUS usage 2011-2016
# hus16 <- read_excel("./data/hus_data.xlsx", sheet = "Menekki HUS 2011-2016")[, c('verituote_laatu', 'osastolle vientiaika')]
# colnames(hus16) <- c("product", "time")  # Change column names
# hus16 <- hus16[hus16$product == "PUNASOLUVALMISTE", ][, 2]  # Select only red cells and drop label column
# hus16 <- arrange(hus16, time)  # Arrange in time ascending
# hus16$time <- as.Date(hus16$time)  # Convert into workable datetime
# 
# # HUS usage 2017
# hus17 <- read_excel("./data/hus_data.xlsx", sheet = "Menekki HUS 2017")[, c('verituote_laatu', 'osastolle vientiaika')]
# colnames(hus17) <- c("product", "time")  # Change column names
# hus17 <- hus17[hus17$product == "PUNASOLUVALMISTE", ][, 2]  # Select only red cells and drop label column
# hus17 <- arrange(hus17, time)  # Arrange in time ascending
# hus17$time <- as.Date(hus17$time)  # Convert into workable datetime
# 
# # HUS usage 2018 Q1
# hus18 <- read_excel("./data/hus_data.xlsx", sheet = "Menekki HUS 2018 Q1")[, c('verituote_laatu', 'osastolle vientiaika')]
# colnames(hus18) <- c("product", "time")  # Change column names
# hus18 <- hus18[hus18$product == "PUNASOLUVALMISTE", ][, 2]  # Select only red cells and drop label column
# hus18 <- arrange(hus18, time)  # Arrange in time ascending
# hus18$time <- as.Date(hus18$time)  # Convert into workable datetime
# 
# # Combine all HUS usage
# hus.total <- rbind(hus16, hus17, hus18)
# 
```

Check if the series have missing days

``` r
date_range <- seq(min(deliv$time), max(deliv$time), by = 1)
date_range[!date_range %in% deliv$time]
```

    ##  [1] "2014-01-26" "2014-03-09" "2015-01-18" "2015-03-29" "2015-06-27"
    ##  [6] "2016-08-07" "2016-11-06" "2017-01-06" "2017-02-12" "2017-04-02"
    ## [11] "2017-05-21" "2017-06-25" "2017-07-30" "2018-01-14" "2018-02-04"
    ## [16] "2018-05-26" "2018-06-03" "2018-06-10" "2018-07-21" "2018-08-19"
    ## [21] "2019-02-24" "2019-03-02" "2019-03-09" "2019-04-28"

The deliveries are missing 24 days. We will impute.

``` r
deliv.imputed <- as.data.frame(rbind(deliv,
                                     c("2014-01-26", deliv$deliveries[deliv$time == "2014-01-19"]),
                                     c("2014-03-09", deliv$deliveries[deliv$time == "2014-03-02"]),
                                     c("2015-01-18", deliv$deliveries[deliv$time == "2015-01-11"]),
                                     c("2015-03-29", deliv$deliveries[deliv$time == "2015-03-22"]),
                                     c("2015-06-27", deliv$deliveries[deliv$time == "2015-06-20"]),
                                     c("2016-08-07", deliv$deliveries[deliv$time == "2016-07-31"]),
                                     c("2016-11-06", deliv$deliveries[deliv$time == "2016-10-30"]),
                                     c("2017-01-06", deliv$deliveries[deliv$time == "2016-12-30"]),
                                     c("2017-02-12", deliv$deliveries[deliv$time == "2017-02-05"]),
                                     c("2017-04-02", deliv$deliveries[deliv$time == "2017-03-26"]),
                                     c("2017-05-21", deliv$deliveries[deliv$time == "2017-05-14"]),
                                     c("2017-06-25", deliv$deliveries[deliv$time == "2017-06-18"]),
                                     c("2017-07-30", deliv$deliveries[deliv$time == "2017-07-23"]),
                                     c("2018-01-14", deliv$deliveries[deliv$time == "2018-01-07"]),
                                     c("2018-02-04", deliv$deliveries[deliv$time == "2018-01-28"]),
                                     c("2018-05-26", deliv$deliveries[deliv$time == "2018-05-19"]),
                                     c("2018-06-03", deliv$deliveries[deliv$time == "2018-05-27"]),
                                     c("2018-06-10", deliv$deliveries[deliv$time == "2018-06-04"]),
                                     c("2018-07-21", deliv$deliveries[deliv$time == "2018-07-14"]),
                                     c("2018-08-19", deliv$deliveries[deliv$time == "2018-08-12"]),
                                     c("2019-02-24", deliv$deliveries[deliv$time == "2019-02-17"]),
                                     c("2019-03-02", deliv$deliveries[deliv$time == "2019-02-23"]),
                                     c("2019-03-09", deliv$deliveries[deliv$time == "2019-03-03"]),
                                     c("2019-04-28", deliv$deliveries[deliv$time == "2019-04-21"]))); colnames(deliv.imputed) <- c("time", "deliveries")

deliv.imputed <- arrange(deliv.imputed, time)
```

``` r
date_range <- seq(min(deliv.imputed$time), max(deliv.imputed$time), by = 1)
date_range[!date_range %in% deliv.imputed$time]
```

    ## Date of length 0

## Create time series

``` r
ts.deliv <- ts(deliv.imputed$deliveries, start = 2014, frequency = 365)
ts.usage <- ts(usage$pcs, start = 2014, frequency = 365)
```

``` r
autoplot(window(ts.usage, start = 2019)) + autolayer(window(ts.deliv, start = 2019)) + ggtitle("Ketju-menekki vs. Ketju-toimitukset 2019")
```

![](demand_lab_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# Plot
usage.p <- ts(cumsum(usage$pcs), start = 2014, frequency = 265)
deliv.p <- ts(cumsum(deliv.imputed$deliveries), start = 2014, frequency = 265)
difference <- tail(cumsum(deliv.imputed$deliveries), 1) - tail(cumsum(usage$pcs), 1)
autoplot(usage.p) + autolayer(deliv.p) + ggtitle(paste("Cumsum difference at end of series: ", difference))
```

![](demand_lab_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

LEGACY

``` r
# ts.hus.usage.red <- ts.hus.usage.red/max(ts.hus.usage.red)
# ts.hus.deliv.cut <- hus.deliv.cut/max(hus.deliv.cut)
# 
# lag1 <- head(c(0, ts.hus.usage.red), -15)
# lag2 <- head(c(rep(c(0), 2), ts.hus.usage.red), -16)
# lag3 <- head(c(rep(c(0), 3), ts.hus.usage.red), -17)
# lag4 <- head(c(rep(c(0), 4), ts.hus.usage.red), -18)
# lag5 <- head(c(rep(c(0), 5), ts.hus.usage.red), -19)
# lag6 <- head(c(rep(c(0), 6), ts.hus.usage.red), -20)
# lag7 <- head(c(rep(c(0), 7), ts.hus.usage.red), -21)
# lag8 <- head(c(rep(c(0), 8), ts.hus.usage.red), -22)
# lag9 <- head(c(rep(c(0), 9), ts.hus.usage.red), -23)
# lag10 <- head(c(rep(c(0), 10), ts.hus.usage.red), -24)
# 
# lag.m <- cbind(lag1, lag2, lag3, lag4, lag5,
#                lag6, lag7, lag8, lag9, lag10)
# 
# flag.m <- cbind(tail(c(0, ts.hus.usage.red), 14))
# 
# fcast <- forecast(auto.arima(head(ts.hus.deliv.cut, -14), xreg = lag.m), xreg = head(lag.m, 14), h = 14)
# 
# autoplot(fcast) + autolayer(ts.hus.deliv.cut)
```