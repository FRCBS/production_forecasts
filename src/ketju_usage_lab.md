Usage Lab: Red Cells (Ketju)
================

``` r
library(forecast)
library(ggplot2)
library(gridExtra)
library(knitr)
library(readxl)
library(plyr)
library(lubridate)
source("src/evalhelp.R")
```

# Intro

Currently under development. In this notebook we study how to use
hospital blood product usage data to create demand
predictions.

## Create original datasets that should remain immutable throughout labbing

``` r
# Load data
# All deliveries
deliv <- read_excel("./data/ketju_data.xlsx", sheet = "Ketju-punasolutoimitukset 2014-")[, c('Päivämäärä', 'Toimitukset')]
colnames(deliv) <- c("time", "deliveries")  # Change column names
deliv$time <- as.Date(deliv$time)

# Ketju usage 2014 -->
# I'm using read.csv() instead of read_excel() here, because this sheet contains some fields that kills read_excel!
ketju <- read.csv("./data/ketju_data.csv", header = TRUE, sep = ",", colClasses=c("NULL", NA, "NULL", "NULL", "NULL", NA, "NULL", "NULL", NA, NA, NA))
colnames(ketju) <- c("hospital", "type", "time", "exp", "pcs")  # Change column names

# Ensure compliant time format with lubridate
ketju$time <- mdy(ketju$time)
ketju$exp <- mdy(ketju$exp)  # This will produce an error "failed to parse" for fields that aren't dates. It will insert NAs.

# Arrange by time
ketju <- arrange(ketju, time)

# Find usage
usage <- aggregate(ketju$pcs, by = list(ketju$time), sum); colnames(usage) <- c("time", "pcs")
```

## Histograms of how fresh blood is used across hospitals

![](ketju_usage_lab_files/figure-gfm/freshness_per_hospital-1.png)<!-- -->

## Same histogram but with blood types

![](ketju_usage_lab_files/figure-gfm/freshness_per_type-1.png)<!-- -->

## Series of usage

``` r
# Create a convenience vector for hospital tags
hospitals <-c("FIMLAB  HÄMEENLINNA VERIKESKUS", "PHKS VERIKESKUS, LAHTI", "TYKSLAB VERIKESKUS", "SATADIAG VERIKESKUS, RAUMA", 
              "FIMLAB TAMPERE VERIKESKUS", "FIMLAB VERIKESKUS, JYVÄSKYLÄ", "KYMKS VERIKESKUS, KOTKA", "EKKS VERIKESKUS, LAPPEENRANTA", 
              "SEINÄJOEN KS VERIKESKUS", "VAASAN KS VERIKESKUS", "NORDLAB KOKKOLA VERIKESKUS", "ISLAB KUOPIO VERIKESKUS", 
              "NORDLAB OULU VERIKESKUS", "NORDLAB ROVANIEMI VERIKESKUS", "SATADIAG VERIKESKUS, PORI")
plots <- list()
i = 0
for(hospital in hospitals){
   i <- i + 1
  hospital.data <- ketju[ketju$hospital == hospital, ]
  hospital.usage <- aggregate(hospital.data$pcs, by = list(hospital.data$time), sum)
  colnames(hospital.usage) <- c("time", "pcs")
  temp <- make_whole(hospital.usage)
  hospital.whole <- temp[[1]]
  hospital.missing <- temp[[2]]

  # Plot
  hospital.plot <- ggplot() + 
    geom_line(data = hospital.whole, aes(x = time, y = pcs)) +
    geom_vline(xintercept = hospital.missing, color = "red") +
    xlab("time") +
    ggtitle(paste(hospital, "\n missing data: ", round(length(hospital.missing)/length(hospital.whole$pcs)*100, digits = 2), "%")) +
    theme(plot.title = element_text(size = 8))
  
  plots[[i]] <- hospital.plot
 
}

ml <- marrangeGrob(plots, nrow=2, ncol=2)
ml
```

![](ketju_usage_lab_files/figure-gfm/data_goodness-1.png)<!-- -->![](ketju_usage_lab_files/figure-gfm/data_goodness-2.png)<!-- -->![](ketju_usage_lab_files/figure-gfm/data_goodness-3.png)<!-- -->![](ketju_usage_lab_files/figure-gfm/data_goodness-4.png)<!-- -->

The problem might be that 0s are not recorded, so we can’t say what’s
actually missing data and what’s just a data point of zero. Let’s limit
our explorations to 2019 for now. This probably means we’ll have to
exclude
RAUMA.

``` r
hospitals <- c("FIMLAB  HÄMEENLINNA VERIKESKUS", "PHKS VERIKESKUS, LAHTI", "TYKSLAB VERIKESKUS", 
              "FIMLAB TAMPERE VERIKESKUS", "FIMLAB VERIKESKUS, JYVÄSKYLÄ", "KYMKS VERIKESKUS, KOTKA", "EKKS VERIKESKUS, LAPPEENRANTA", 
              "SEINÄJOEN KS VERIKESKUS", "VAASAN KS VERIKESKUS", "NORDLAB KOKKOLA VERIKESKUS", "ISLAB KUOPIO VERIKESKUS", 
              "NORDLAB OULU VERIKESKUS", "NORDLAB ROVANIEMI VERIKESKUS", "SATADIAG VERIKESKUS, PORI")

plots <- list()
i = 0
for(hospital in hospitals){
   i <- i + 1
  hospital.data <- ketju[ketju$hospital == hospital, ]
  hospital.usage <- aggregate(hospital.data$pcs, by = list(hospital.data$time), sum)
  colnames(hospital.usage) <- c("time", "pcs")
  hospital.usage <- hospital.usage[hospital.usage$time >= as.Date("2019-01-01"), ]
  temp <- make_whole(hospital.usage)
  hospital.whole <- temp[[1]]
  hospital.missing <- temp[[2]]

  # Plot
  hospital.plot <- ggplot() + 
    geom_line(data = hospital.whole, aes(x = time, y = pcs)) +
    geom_vline(xintercept = hospital.missing, color = "red") +
    ggtitle(paste(hospital, "\n missing data: ", round(length(hospital.missing)/length(hospital.whole$pcs)*100, digits = 2), "%")) +
    theme(plot.title = element_text(size = 8)) +
    ylab("")
  
  plots[[i]] <- hospital.plot
 
}

ml <- marrangeGrob(plots, nrow=2, ncol=2)
ml
```

![](ketju_usage_lab_files/figure-gfm/2019_series-1.png)<!-- -->![](ketju_usage_lab_files/figure-gfm/2019_series-2.png)<!-- -->![](ketju_usage_lab_files/figure-gfm/2019_series-3.png)<!-- -->![](ketju_usage_lab_files/figure-gfm/2019_series-4.png)<!-- -->

## Total usage across all hospitals (in 2019, *without imputation*)

![](ketju_usage_lab_files/figure-gfm/total_2019_wo_imput-1.png)<!-- -->

## Total usage *with imputation*

``` r
# hospitals <- c("FIMLAB  HÄMEENLINNA VERIKESKUS", "PHKS VERIKESKUS, LAHTI", "TYKSLAB VERIKESKUS", 
#               "FIMLAB TAMPERE VERIKESKUS", "FIMLAB VERIKESKUS, JYVÄSKYLÄ", "KYMKS VERIKESKUS, KOTKA", "EKKS VERIKESKUS, LAPPEENRANTA", 
#               "SEINÄJOEN KS VERIKESKUS", "VAASAN KS VERIKESKUS", "NORDLAB KOKKOLA VERIKESKUS", "ISLAB KUOPIO VERIKESKUS", 
#               "NORDLAB OULU VERIKESKUS", "NORDLAB ROVANIEMI VERIKESKUS", "SATADIAG VERIKESKUS, PORI")
# 
# for(hospital in hospitals){
#   hospital.data <- ketju[ketju$hospital == hospital, ]
#   hospital.usage <- aggregate(hospital.data$pcs, by = list(hospital.data$time), sum)
#   colnames(hospital.usage) <- c("time", "pcs")
#   hospital.usage <- hospital.usage[hospital.usage$time >= as.Date("2019-01-01"), ]
#   temp <- make_whole(hospital.usage)
#   hospital.whole <- temp[[1]]
#   hospital.missing <- temp[[2]]
# }
# 
# total.usage <- aggregate(total$pcs, by = list(total$time), sum)
```
