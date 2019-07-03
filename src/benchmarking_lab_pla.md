Benchmarking Lab: Platelets
================

``` r
# Set working directory
knitr::opts_knit$set(root.dir = "/home/esa/production_forecasts") # Working on Ubuntu
# knitr::opts_knit$set(root.dir = "V:/production_forecasts") # Working home
# knitr::opts_knit$set(root.dir = "C:/Users/esatu/production_forecasts") # Working on laptop
```

## Create original dataset that should remain immutable throughout labbing

``` r
library(forecast)
library(ggplot2)
library(gridExtra)
library(knitr)
library(readxl)
source("src/evalhelp.R")

# Load data
monthly_sales <- read.table("./data/kuukausimyynti.txt", header = T, sep = "\t")

# Separate yyyy/mmm column into months and years
monthly_sales$year <- substr(monthly_sales$kuukausi, 1, 4)
monthly_sales$month <- factor(substr(monthly_sales$kuukausi, 6, 8), levels = c("tam", "hel", "maa", "huh", "tou", "kes", "hei", "elo", "syy", "lok", "mar", "jou"))

# Create a numeric column for months
monthly_sales$month_num <- as.numeric(monthly_sales$month)

# Omit empty values
d <- na.omit(monthly_sales)
```

## Create the time series object

``` r
ts.pla <- ts(d$Trombosyyttivalmisteet, 
             start = as.numeric(c(d$year[1], d$month_num[1])), 
             end = as.numeric(c(tail(d$year, 1), tail(d$month_num, 1))), 
             frequency = 12)  # This tells the series that it is monthly data
```

## Previously used production forecasts

These are our true benchmark before any improvements

``` r
# STL + ETS
old.stl.e <- tsCV(ts.pla, fstl, t.window = 6, h = 1)
old.stl.crit <- cMAPE(old.stl.e, ts.pla)
old.stl.mape <- mean(abs(100*old.stl.e/ts.pla), na.rm = TRUE)
old.stl.rmse <- sqrt(mean(old.stl.e^2, na.rm = TRUE))

# ETS
old.ets.e <- tsCV(ts.pla, fets, h = 1)
old.ets.crit <- cMAPE(old.ets.e, ts.pla)
old.ets.mape <- mean(abs(100*old.ets.e/ts.pla), na.rm = TRUE)
old.ets.rmse <- sqrt(mean(old.ets.e^2, na.rm = TRUE))


# Table
oldbench <- matrix(c(old.stl.crit, old.stl.mape, old.stl.rmse, 
                     old.ets.crit, old.ets.mape, old.ets.rmse),
                   ncol = 3,
                   byrow = TRUE)

colnames(oldbench) <- c("cMAPE", "MAPE", "RMSE")
rownames(oldbench) <- c("old STL", "old ETS")
kable(oldbench, "markdown")
```

|         |    cMAPE |     MAPE |     RMSE |
| :------ | -------: | -------: | -------: |
| old STL | 7.787913 | 5.075908 | 198.7787 |
| old ETS | 7.913621 | 5.217426 | 202.4066 |

## Get baseline benchmarks to beat

These are our absolute worst forecasts we can make. A random walk
forecast takes the previous value of the series and walks randomly to
some direction. A naive forecast will forecast only the previous value
of the series. A seasonal naive will forecast only the previous value of
the same season of the series. Mean forecast will forecast the series
mean.

``` r
# Random walk with drift
rwf.e <- tsCV(ts.pla, rwf, drift = TRUE, h = 1)
rwf.crit <- cMAPE(rwf.e, ts.pla)
rwf.rmse <- sqrt(mean(rwf.e^2, na.rm = TRUE))
rwf.mape <- mean(abs(100*rwf.e/ts.pla), na.rm = TRUE)

# Naive
naive.e <- tsCV(ts.pla, naive, h = 1)
naive.crit <- cMAPE(naive.e, ts.pla)
naive.rmse <- sqrt(mean(naive.e^2, na.rm = TRUE))
naive.mape <- mean(abs(100*naive.e/ts.pla), na.rm = TRUE)

# Seasonal naive
snaive.e <- tsCV(ts.pla, snaive, h = 1)
snaive.crit <- cMAPE(snaive.e, ts.pla)
snaive.rmse <- sqrt(mean(snaive.e^2, na.rm = TRUE))
snaive.mape <- mean(abs(100*snaive.e/ts.pla), na.rm = TRUE)

# Mean forecast
meanf.e <- tsCV(ts.pla, meanf, h = 1)
meanf.crit <- cMAPE(meanf.e, ts.pla)
meanf.rmse <- sqrt(mean(meanf.e^2, na.rm = TRUE))
meanf.mape <- mean(abs(100*meanf.e/ts.pla), na.rm = TRUE)

benchmarks <- matrix(c(rwf.crit, rwf.mape, rwf.rmse,
                       naive.crit, naive.mape, naive.rmse,
                       snaive.crit, snaive.mape, snaive.rmse,
                       meanf.crit, meanf.mape, meanf.rmse),
                     ncol = 3,
                     byrow = TRUE)
colnames(benchmarks) <- c("cMAPE", "MAPE", "RMSE")
rownames(benchmarks) <- c("RWF", "NAIVE", "SNAIVE", "MEANF")
kable(benchmarks, "markdown")
```

|        |     cMAPE |     MAPE |     RMSE |
| :----- | --------: | -------: | -------: |
| RWF    |  9.680319 | 6.439720 | 248.2208 |
| NAIVE  |  9.205568 | 6.267346 | 244.0038 |
| SNAIVE | 10.803218 | 7.383793 | 276.5688 |
| MEANF  | 11.440821 | 8.261303 | 310.0805 |

These are considerably worse than our old forecasts, which is to be
expected. The seasonal naive comes quite close, however, with old
forecasts improving only by 1 pp.

## Cutting our series

Next, we’ll limit our series scope to 2013. This ought to improve our
forecasts some, as the significant level change that occurred in
2012-2013 doesn’t get to affect our modelling. We will later limit this
series to the end of 2018 in order to to business day adjustments.

``` r
# Window
ts.pla.cut <- window(ts.pla, start = 2013)
autoplot(ts.pla.cut) + ggtitle("Platelet sales from 2013 onwards")
```

![](benchmarking_lab_pla_files/figure-gfm/cut_series-1.png)<!-- --> We
will now run all of our benchmarks again in a single chunk (to save
space).

``` r
# STL + ETS
old.stl.e <- tsCV(ts.pla.cut, fstl, t.window = 6, h = 1)
old.stl.crit <- cMAPE(old.stl.e, ts.pla.cut)
old.stl.mape <- mean(abs(100*old.stl.e/ts.pla.cut), na.rm = TRUE)
old.stl.rmse <- sqrt(mean(old.stl.e^2, na.rm = TRUE))

# ETS
old.ets.e <- tsCV(ts.pla.cut, fets, h = 1)
old.ets.crit <- cMAPE(old.ets.e, ts.pla.cut)
old.ets.mape <- mean(abs(100*old.ets.e/ts.pla.cut), na.rm = TRUE)
old.ets.rmse <- sqrt(mean(old.ets.e^2, na.rm = TRUE))

# Random walk with drift
rwf.e <- tsCV(ts.pla.cut, rwf, drift = TRUE, h = 1)
rwf.crit <- cMAPE(rwf.e, ts.pla.cut)
rwf.rmse <- sqrt(mean(rwf.e^2, na.rm = TRUE))
rwf.mape <- mean(abs(100*rwf.e/ts.pla.cut), na.rm = TRUE)

# Naive
naive.e <- tsCV(ts.pla.cut, naive, h = 1)
naive.crit <- cMAPE(naive.e, ts.pla.cut)
naive.rmse <- sqrt(mean(naive.e^2, na.rm = TRUE))
naive.mape <- mean(abs(100*naive.e/ts.pla.cut), na.rm = TRUE)

# Seasonal naive
snaive.e <- tsCV(ts.pla.cut, snaive, h = 1)
snaive.crit <- cMAPE(snaive.e, ts.pla.cut)
snaive.rmse <- sqrt(mean(snaive.e^2, na.rm = TRUE))
snaive.mape <- mean(abs(100*snaive.e/ts.pla.cut), na.rm = TRUE)

# Mean forecast
meanf.e <- tsCV(ts.pla.cut, meanf, h = 1)
meanf.crit <- cMAPE(meanf.e, ts.pla.cut)
meanf.rmse <- sqrt(mean(meanf.e^2, na.rm = TRUE))
meanf.mape <- mean(abs(100*meanf.e/ts.pla.cut), na.rm = TRUE)

# Table
cutbench <- matrix(c(old.stl.crit, old.stl.mape, old.stl.rmse, 
                     old.ets.crit, old.ets.mape, old.ets.rmse,
                     rwf.crit, rwf.mape, rwf.rmse,
                     naive.crit, naive.mape, naive.rmse,
                     snaive.crit, snaive.mape, snaive.rmse,
                     meanf.crit, meanf.mape, meanf.rmse),
                     ncol = 3,
                     byrow = TRUE)

colnames(cutbench) <- c("cMAPE", "MAPE", "RMSE")
rownames(cutbench) <- c("STL", "ETS", "RWF", "NAIVE", "SNAIVE", "MEANF")
kable(cutbench, "markdown")
```

|        |     cMAPE |     MAPE |     RMSE |
| :----- | --------: | -------: | -------: |
| STL    |  8.582551 | 5.412623 | 197.3068 |
| ETS    |  7.937989 | 5.203530 | 199.6698 |
| RWF    |  9.948026 | 6.693210 | 257.8827 |
| NAIVE  |  9.555527 | 6.464347 | 250.5112 |
| SNAIVE | 11.529393 | 7.157038 | 250.7072 |
| MEANF  | 12.019474 | 6.775826 | 246.4070 |

## Business day adjustment (series between 2013 - 2018)

``` r
# Load business day data
biz <- read_excel("./data/workdays_monthly.xlsx", sheet = "Tabella")
```

    ## New names:
    ## * `` -> ...1

``` r
colnames(biz) <- c("month", "days")

# Business day adjustment
ts.pla.adj <- window(ts.pla, start = 2013, end = c(2018, 12)) / tail(head(biz$days, 84), 72)
```

Let’s see how our series looks now

``` r
autoplot(ts.pla.adj) + ggtitle("Platelet sales 2013-2018, adjusted")
```

![](benchmarking_lab_pla_files/figure-gfm/visualize_adj-1.png)<!-- -->

``` r
ggseasonplot(ts.pla.adj)
```

![](benchmarking_lab_pla_files/figure-gfm/visualize_adj-2.png)<!-- -->

We will build all our future models based on this adjustment. Let’s see
if it gives any improvement on our earlier benchmarks:

``` r
# STL + ETS
new.stl.e <- tsCV(ts.pla.adj, fstl, t.window = 6, h = 1)
new.stl.crit <- cMAPE(new.stl.e, ts.pla.adj)
new.stl.mape <- mean(abs(100*new.stl.e/ts.pla.adj), na.rm = TRUE)
new.stl.rmse <- sqrt(mean(new.stl.e^2, na.rm = TRUE))

# ETS
new.ets.e <- tsCV(ts.pla.adj, fets, h = 1)
new.ets.crit <- cMAPE(new.ets.e, ts.pla.adj)
new.ets.mape <- mean(abs(100*new.ets.e/ts.pla.adj), na.rm = TRUE)
new.ets.rmse <- sqrt(mean(new.ets.e^2, na.rm = TRUE))

# Random walk with drift
rwf.e.adj <- tsCV(ts.pla.adj, rwf, drift = TRUE, h = 1)
rwf.crit.adj <- cMAPE(rwf.e.adj, ts.pla.adj)
rwf.rmse.adj <- sqrt(mean(rwf.e.adj^2, na.rm = TRUE))
rwf.mape.adj <- mean(abs(100*rwf.e.adj/ts.pla.adj), na.rm = TRUE)

# Naive
naive.e.adj <- tsCV(ts.pla.adj, naive, h = 1)
naive.crit.adj <- cMAPE(naive.e.adj, ts.pla.adj)
naive.rmse.adj <- sqrt(mean(naive.e.adj^2, na.rm = TRUE))
naive.mape.adj <- mean(abs(100*naive.e.adj/ts.pla.adj), na.rm = TRUE)

# Seasonal naive
snaive.e.adj <- tsCV(ts.pla.adj, snaive, h = 1)
snaive.crit.adj <- cMAPE(snaive.e.adj, ts.pla.adj)
snaive.rmse.adj <- sqrt(mean(snaive.e.adj^2, na.rm = TRUE))
snaive.mape.adj <- mean(abs(100*snaive.e.adj/ts.pla.adj), na.rm = TRUE)

# Mean forecast
meanf.e.adj <- tsCV(ts.pla.adj, meanf, h = 1)
meanf.crit.adj <- cMAPE(meanf.e.adj, ts.pla.adj)
meanf.rmse.adj <- sqrt(mean(meanf.e.adj^2, na.rm = TRUE))
meanf.mape.adj <- mean(abs(100*meanf.e.adj/ts.pla.adj), na.rm = TRUE)

# Table
adjusted <- matrix(c(new.stl.crit, new.stl.mape, new.stl.rmse, 
                     new.ets.crit, new.ets.mape, new.ets.rmse,
                     rwf.crit.adj, rwf.mape.adj, rwf.rmse.adj,
                     naive.crit.adj, naive.mape.adj, naive.rmse.adj,
                     snaive.crit.adj, snaive.mape.adj, snaive.rmse.adj,
                     meanf.crit.adj, meanf.mape.adj, meanf.rmse.adj),
                     ncol = 3,
                     byrow = TRUE)

colnames(adjusted) <- c("cMAPE", "MAPE", "RMSE")
rownames(adjusted) <- c("STL", "ETS", "RWF", "NAIVE", "SNAIVE", "MEANF")
kable(adjusted, "markdown")
```

|        |    cMAPE |     MAPE |     RMSE |
| :----- | -------: | -------: | -------: |
| STL    | 10.04445 | 6.262367 | 10.83817 |
| ETS    | 10.38095 | 6.621028 | 11.98268 |
| RWF    | 11.10333 | 7.414982 | 14.55185 |
| NAIVE  | 10.62563 | 7.313508 | 14.01187 |
| SNAIVE | 13.35542 | 8.729183 | 15.83407 |
| MEANF  | 14.38087 | 8.414290 | 13.85353 |

## Rolling windows

``` r
# 4 YEARS

# STL + ETS
new.stl.4.e <- tsCV(ts.pla.adj, fstl, t.window = 6, h = 1, window = 48)
new.stl.4.crit <- cMAPE(new.stl.4.e, ts.pla.adj)
new.stl.4.mape <- mean(abs(100*new.stl.4.e/ts.pla.adj), na.rm = TRUE)
new.stl.4.rmse <- sqrt(mean(new.stl.4.e^2, na.rm = TRUE))

# ETS
new.ets.4.e <- tsCV(ts.pla.adj, fets, h = 1, window = 48)
new.ets.4.crit <- cMAPE(new.ets.4.e, ts.pla.adj)
new.ets.4.mape <- mean(abs(100*new.ets.4.e/ts.pla.adj), na.rm = TRUE)
new.ets.4.rmse <- sqrt(mean(new.ets.4.e^2, na.rm = TRUE))

# Random walk with drift
rwf.e.4.adj <- tsCV(ts.pla.adj, rwf, drift = TRUE, h = 1, window = 48)
rwf.crit.4.adj <- cMAPE(rwf.e.4.adj, ts.pla.adj)
rwf.rmse.4.adj <- sqrt(mean(rwf.e.4.adj^2, na.rm = TRUE))
rwf.mape.4.adj <- mean(abs(100*rwf.e.4.adj/ts.pla.adj), na.rm = TRUE)

# Naive
naive.e.4.adj <- tsCV(ts.pla.adj, naive, h = 1, window = 48)
naive.crit.4.adj <- cMAPE(naive.e.4.adj, ts.pla.adj)
naive.rmse.4.adj <- sqrt(mean(naive.e.4.adj^2, na.rm = TRUE))
naive.mape.4.adj <- mean(abs(100*naive.e.4.adj/ts.pla.adj), na.rm = TRUE)

# Seasonal naive
snaive.e.4.adj <- tsCV(ts.pla.adj, snaive, h = 1, window = 48)
snaive.crit.4.adj <- cMAPE(snaive.e.4.adj, ts.pla.adj)
snaive.rmse.4.adj <- sqrt(mean(snaive.e.4.adj^2, na.rm = TRUE))
snaive.mape.4.adj <- mean(abs(100*snaive.e.4.adj/ts.pla.adj), na.rm = TRUE)

# Mean forecast
meanf.e.4.adj <- tsCV(ts.pla.adj, meanf, h = 1, window = 48)
meanf.crit.4.adj <- cMAPE(meanf.e.4.adj, ts.pla.adj)
meanf.rmse.4.adj <- sqrt(mean(meanf.e.4.adj^2, na.rm = TRUE))
meanf.mape.4.adj <- mean(abs(100*meanf.e.4.adj/ts.pla.adj), na.rm = TRUE)


# 3 YEARS

# STL + ETS
new.stl.3.e <- tsCV(ts.pla.adj, fstl, t.window = 6, h = 1, window = 36)
new.stl.3.crit <- cMAPE(new.stl.3.e, ts.pla.adj)
new.stl.3.mape <- mean(abs(100*new.stl.3.e/ts.pla.adj), na.rm = TRUE)
new.stl.3.rmse <- sqrt(mean(new.stl.3.e^2, na.rm = TRUE))

# ETS
new.ets.3.e <- tsCV(ts.pla.adj, fets, h = 1, window = 36)
new.ets.3.crit <- cMAPE(new.ets.3.e, ts.pla.adj)
new.ets.3.mape <- mean(abs(100*new.ets.3.e/ts.pla.adj), na.rm = TRUE)
new.ets.3.rmse <- sqrt(mean(new.ets.3.e^2, na.rm = TRUE))

# Random walk with drift
rwf.e.3.adj <- tsCV(ts.pla.adj, rwf, drift = TRUE, h = 1, window = 36)
rwf.crit.3.adj <- cMAPE(rwf.e.3.adj, ts.pla.adj)
rwf.rmse.3.adj <- sqrt(mean(rwf.e.3.adj^2, na.rm = TRUE))
rwf.mape.3.adj <- mean(abs(100*rwf.e.3.adj/ts.pla.adj), na.rm = TRUE)

# Naive
naive.e.3.adj <- tsCV(ts.pla.adj, naive, h = 1, window = 36)
naive.crit.3.adj <- cMAPE(naive.e.3.adj, ts.pla.adj)
naive.rmse.3.adj <- sqrt(mean(naive.e.3.adj^2, na.rm = TRUE))
naive.mape.3.adj <- mean(abs(100*naive.e.3.adj/ts.pla.adj), na.rm = TRUE)

# Seasonal naive
snaive.e.3.adj <- tsCV(ts.pla.adj, snaive, h = 1, window = 36)
snaive.crit.3.adj <- cMAPE(snaive.e.3.adj, ts.pla.adj)
snaive.rmse.3.adj <- sqrt(mean(snaive.e.3.adj^2, na.rm = TRUE))
snaive.mape.3.adj <- mean(abs(100*snaive.e.3.adj/ts.pla.adj), na.rm = TRUE)

# Mean forecast
meanf.e.3.adj <- tsCV(ts.pla.adj, meanf, h = 1, window = 36)
meanf.crit.3.adj <- cMAPE(meanf.e.3.adj, ts.pla.adj)
meanf.rmse.3.adj <- sqrt(mean(meanf.e.3.adj^2, na.rm = TRUE))
meanf.mape.3.adj <- mean(abs(100*meanf.e.3.adj/ts.pla.adj), na.rm = TRUE)


# 2 YEARS

# STL + ETS (not possible)
new.stl.2.crit <- NA
new.stl.2.mape <- NA
new.stl.2.rmse <- NA

# ETS
new.ets.2.e <- tsCV(ts.pla.adj, fets, h = 1, window = 24)
new.ets.2.crit <- cMAPE(new.ets.2.e, ts.pla.adj)
new.ets.2.mape <- mean(abs(100*new.ets.2.e/ts.pla.adj), na.rm = TRUE)
new.ets.2.rmse <- sqrt(mean(new.ets.2.e^2, na.rm = TRUE))

# Random walk with drift
rwf.e.2.adj <- tsCV(ts.pla.adj, rwf, drift = TRUE, h = 1, window = 24)
rwf.crit.2.adj <- cMAPE(rwf.e.2.adj, ts.pla.adj)
rwf.rmse.2.adj <- sqrt(mean(rwf.e.2.adj^2, na.rm = TRUE))
rwf.mape.2.adj <- mean(abs(100*rwf.e.2.adj/ts.pla.adj), na.rm = TRUE)

# Naive
naive.e.2.adj <- tsCV(ts.pla.adj, naive, h = 1, window = 24)
naive.crit.2.adj <- cMAPE(naive.e.2.adj, ts.pla.adj)
naive.rmse.2.adj <- sqrt(mean(naive.e.2.adj^2, na.rm = TRUE))
naive.mape.2.adj <- mean(abs(100*naive.e.2.adj/ts.pla.adj), na.rm = TRUE)

# Seasonal naive
snaive.e.2.adj <- tsCV(ts.pla.adj, snaive, h = 1, window = 24)
snaive.crit.2.adj <- cMAPE(snaive.e.2.adj, ts.pla.adj)
snaive.rmse.2.adj <- sqrt(mean(snaive.e.2.adj^2, na.rm = TRUE))
snaive.mape.2.adj <- mean(abs(100*snaive.e.2.adj/ts.pla.adj), na.rm = TRUE)

# Mean forecast
meanf.e.2.adj <- tsCV(ts.pla.adj, meanf, h = 1, window = 24)
meanf.crit.2.adj <- cMAPE(meanf.e.2.adj, ts.pla.adj)
meanf.rmse.2.adj <- sqrt(mean(meanf.e.2.adj^2, na.rm = TRUE))
meanf.mape.2.adj <- mean(abs(100*meanf.e.2.adj/ts.pla.adj), na.rm = TRUE)


# 1 YEAR

# STL + ETS (not possible)
new.stl.1.crit <- NA
new.stl.1.mape <- NA
new.stl.1.rmse <- NA

# ETS
new.ets.1.e <- tsCV(ts.pla.adj, fets, h = 1, window = 12)
new.ets.1.crit <- cMAPE(new.ets.1.e, ts.pla.adj)
new.ets.1.mape <- mean(abs(100*new.ets.1.e/ts.pla.adj), na.rm = TRUE)
new.ets.1.rmse <- sqrt(mean(new.ets.1.e^2, na.rm = TRUE))

# Random walk with drift
rwf.e.1.adj <- tsCV(ts.pla.adj, rwf, drift = TRUE, h = 1, window = 12)
rwf.crit.1.adj <- cMAPE(rwf.e.1.adj, ts.pla.adj)
rwf.rmse.1.adj <- sqrt(mean(rwf.e.1.adj^2, na.rm = TRUE))
rwf.mape.1.adj <- mean(abs(100*rwf.e.1.adj/ts.pla.adj), na.rm = TRUE)

# Naive
naive.e.1.adj <- tsCV(ts.pla.adj, naive, h = 1, window = 12)
naive.crit.1.adj <- cMAPE(naive.e.1.adj, ts.pla.adj)
naive.rmse.1.adj <- sqrt(mean(naive.e.1.adj^2, na.rm = TRUE))
naive.mape.1.adj <- mean(abs(100*naive.e.1.adj/ts.pla.adj), na.rm = TRUE)

# Seasonal naive
snaive.e.1.adj <- tsCV(ts.pla.adj, snaive, h = 1, window = 12)
snaive.crit.1.adj <- cMAPE(snaive.e.1.adj, ts.pla.adj)
snaive.rmse.1.adj <- sqrt(mean(snaive.e.1.adj^2, na.rm = TRUE))
snaive.mape.1.adj <- mean(abs(100*snaive.e.1.adj/ts.pla.adj), na.rm = TRUE)

# Mean forecast
meanf.e.1.adj <- tsCV(ts.pla.adj, meanf, h = 1, window = 12)
meanf.crit.1.adj <- cMAPE(meanf.e.1.adj, ts.pla.adj)
meanf.rmse.1.adj <- sqrt(mean(meanf.e.1.adj^2, na.rm = TRUE))
meanf.mape.1.adj <- mean(abs(100*meanf.e.1.adj/ts.pla.adj), na.rm = TRUE)

# Table
rolling <- matrix(c(new.stl.4.crit, new.stl.4.mape, new.stl.4.rmse,
                    new.ets.4.crit, new.ets.4.mape, new.ets.4.rmse,
                    rwf.crit.4.adj, rwf.mape.4.adj, rwf.rmse.4.adj,
                    naive.crit.4.adj, naive.mape.4.adj, naive.rmse.4.adj,
                    snaive.crit.4.adj, snaive.mape.4.adj, snaive.rmse.4.adj,
                    meanf.crit.4.adj, meanf.mape.4.adj, meanf.rmse.4.adj,
                    new.stl.3.crit, new.stl.3.mape, new.stl.3.rmse, 
                    new.ets.3.crit, new.ets.3.mape, new.ets.3.rmse,
                    rwf.crit.3.adj, rwf.mape.3.adj, rwf.rmse.3.adj,
                    naive.crit.3.adj, naive.mape.3.adj, naive.rmse.3.adj,
                    snaive.crit.3.adj, snaive.mape.3.adj, snaive.rmse.3.adj,
                    meanf.crit.3.adj, meanf.mape.3.adj, meanf.rmse.3.adj,
                    new.stl.2.crit, new.stl.2.mape, new.stl.2.rmse,
                    new.ets.2.crit, new.ets.2.mape, new.ets.2.rmse,
                    rwf.crit.2.adj, rwf.mape.2.adj, rwf.rmse.2.adj,
                    naive.crit.2.adj, naive.mape.2.adj, naive.rmse.2.adj,
                    snaive.crit.2.adj, snaive.mape.2.adj, snaive.rmse.2.adj,
                    meanf.crit.2.adj, meanf.mape.2.adj, meanf.rmse.2.adj,
                    new.stl.1.crit, new.stl.1.mape, new.stl.1.rmse, 
                    new.ets.1.crit, new.ets.1.mape, new.ets.1.rmse,
                    rwf.crit.1.adj, rwf.mape.1.adj, rwf.rmse.1.adj,
                    naive.crit.1.adj, naive.mape.1.adj, naive.rmse.1.adj,
                    snaive.crit.1.adj, snaive.mape.1.adj, snaive.rmse.1.adj,
                    meanf.crit.1.adj, meanf.mape.1.adj, meanf.rmse.1.adj),
                  ncol = 3,
                  byrow = TRUE)

colnames(rolling) <- c("cMAPE", "MAPE", "RMSE")
rownames(rolling) <- c("STL 4", "ETS 4", "RWF 4", "NAIVE 4", "SNAIVE 4", "MEANF 4",
                       "STL 3", "ETS 3", "RWF 3", "NAIVE 3", "SNAIVE 3", "MEANF 3",
                       "STL 2", "ETS 2", "RWF 2", "NAIVE 2", "SNAIVE 2", "MEANF 2",
                       "STL 1", "ETS 1", "RWF 1", "NAIVE 1", "SNAIVE 1", "MEANF 1")
kable(rolling, "markdown")
```

|          |     cMAPE |     MAPE |      RMSE |
| :------- | --------: | -------: | --------: |
| STL 4    |  8.411499 | 5.489170 |  9.475953 |
| ETS 4    |  8.403598 | 5.779106 | 11.007031 |
| RWF 4    | 10.652695 | 7.535501 | 13.758648 |
| NAIVE 4  | 10.667620 | 7.457297 | 13.519007 |
| SNAIVE 4 |  9.874450 | 6.115029 |  9.908977 |
| MEANF 4  | 17.587225 | 9.447106 | 13.871041 |
| STL 3    |  8.963203 | 5.325770 |  9.314345 |
| ETS 3    |  9.406707 | 6.060783 | 10.916157 |
| RWF 3    | 10.222711 | 7.116673 | 12.575721 |
| NAIVE 3  | 10.202686 | 6.993702 | 12.359739 |
| SNAIVE 3 | 13.364714 | 7.499645 | 12.774538 |
| MEANF 3  | 15.309778 | 8.183424 | 13.108145 |
| STL 2    |        NA |       NA |        NA |
| ETS 2    |  9.205200 | 6.010420 | 11.322514 |
| RWF 2    | 10.487870 | 7.194855 | 12.645358 |
| NAIVE 2  | 10.214284 | 6.924667 | 12.299194 |
| SNAIVE 2 | 13.830325 | 8.257644 | 14.706097 |
| MEANF 2  | 12.114505 | 7.039915 | 12.292759 |
| STL 1    |        NA |       NA |        NA |
| ETS 1    |  8.829862 | 5.866302 | 10.950455 |
| RWF 1    | 11.076918 | 7.541272 | 14.641827 |
| NAIVE 1  | 10.477025 | 7.058037 | 13.704122 |
| SNAIVE 1 | 13.762018 | 8.331658 | 14.754919 |
| MEANF 1  |  9.763668 | 6.074966 | 11.142219 |

*The series isn’t long enough to test rolling windows reliably\!* These
will have to be ignored, probably.

## New models

### Moving average

``` r
# 5-MA
ma5.e.adj <- tsCV(ts.pla.adj, fMA, order = 5, h = 1)
ma5.crit.adj <- cMAPE(ma5.e.adj, ts.pla.adj)
ma5.rmse.adj <- sqrt(mean(ma5.e.adj^2, na.rm = TRUE))
ma5.mape.adj <- mean(abs(100*ma5.e.adj/ts.pla.adj), na.rm = TRUE)

# 7-MA
ma7.e.adj <- tsCV(ts.pla.adj, fMA, order = 7, h = 1)
ma7.crit.adj <- cMAPE(ma7.e.adj, ts.pla.adj)
ma7.rmse.adj <- sqrt(mean(ma7.e.adj^2, na.rm = TRUE))
ma7.mape.adj <- mean(abs(100*ma7.e.adj/ts.pla.adj), na.rm = TRUE)

# 9-MA
ma9.e.adj <- tsCV(ts.pla.adj, fMA, order = 9, h = 1)
ma9.crit.adj <- cMAPE(ma9.e.adj, ts.pla.adj)
ma9.rmse.adj <- sqrt(mean(ma9.e.adj^2, na.rm = TRUE))
ma9.mape.adj <- mean(abs(100*ma9.e.adj/ts.pla.adj), na.rm = TRUE)

# 12-MA
ma12.e.adj <- tsCV(ts.pla.adj, fMA, order = 12, h = 1)
ma12.crit.adj <- cMAPE(ma12.e.adj, ts.pla.adj)
ma12.rmse.adj <- sqrt(mean(ma12.e.adj^2, na.rm = TRUE))
ma12.mape.adj <- mean(abs(100*ma12.e.adj/ts.pla.adj), na.rm = TRUE)

mabench <- matrix(c(ma5.crit.adj, ma5.mape.adj, ma5.rmse.adj,
                    ma7.crit.adj, ma7.mape.adj, ma7.rmse.adj,
                    ma9.crit.adj, ma9.mape.adj, ma9.rmse.adj,
                    ma12.crit.adj, ma12.mape.adj, ma12.rmse.adj),
                   ncol = 3,
                   byrow = TRUE)

colnames(mabench) <- c("cMAPE", "MAPE", "RMSE")
rownames(mabench) <- c("5-MA", "7-MA", "9-MA", "12-MA")
kable(mabench, "markdown")
```

|       |     cMAPE |     MAPE |     RMSE |
| :---- | --------: | -------: | -------: |
| 5-MA  | 10.445999 | 6.833818 | 12.90647 |
| 7-MA  |  9.864105 | 6.343864 | 12.24607 |
| 9-MA  |  9.832367 | 6.323789 | 11.99422 |
| 12-MA |  9.648617 | 6.074162 | 11.14569 |

There was slight improvement in extending the moving average window, but
these scores aren’t even remotely competitive.

### Complex decompositions

``` r
stlf.e.adj <- tsCV(ts.pla.adj, stlf, h = 1)
stlf.crit.adj <- cMAPE(stlf.e.adj, ts.pla.adj)
stlf.mape.adj <- mean(abs(100*stlf.e.adj/ts.pla.adj), na.rm = TRUE)
stlf.rmse.adj <- sqrt(mean(stlf.e.adj^2, na.rm = TRUE))

tbats.e.adj <- tsCV(ts.pla.adj, fTBATS, h = 1)
tbats.crit.adj <- cMAPE(tbats.e.adj, ts.pla.adj)
tbats.mape.adj <- mean(abs(100*tbats.e.adj/ts.pla.adj), na.rm = TRUE)
tbats.rmse.adj <- sqrt(mean(tbats.e.adj^2, na.rm = TRUE))

complex <- matrix(c(stlf.crit.adj, stlf.mape.adj, stlf.rmse.adj,
                    tbats.crit.adj, tbats.mape.adj, tbats.rmse.adj),
                  ncol = 3,
                  byrow = TRUE)
colnames(complex) <- c("cMAPE", "MAPE", "RMSE")
rownames(complex) <- c("STLF", "TBATS")
kable(complex, "markdown")
```

|       |     cMAPE |     MAPE |     RMSE |
| :---- | --------: | -------: | -------: |
| STLF  |  9.724186 | 6.157044 | 10.88087 |
| TBATS | 11.285946 | 7.543292 | 14.90628 |

### A neural network

``` r
nnetar.e <- tsCV(ts.pla.adj, fnnet, h = 1)
nnetar.crit <- cMAPE(nnetar.e, ts.pla.adj)
nnetar.mape <- mean(abs(100*nnetar.e/ts.pla.adj), na.rm = TRUE)
nnetar.rmse <- sqrt(mean(nnetar.e^2, na.rm = TRUE))

nnet <- matrix(c(nnetar.crit, nnetar.mape, nnetar.rmse),
               ncol = 3,
               byrow = TRUE)
colnames(nnet) <- c("cMAPE", "MAPE", "RMSE")
rownames(nnet) <- c("NN")
kable(nnet, "markdown")
```

|    |    cMAPE |     MAPE |     RMSE |
| :- | -------: | -------: | -------: |
| NN | 12.60787 | 7.751734 | 14.49514 |

No improvement with a crude NN, only to be expected.

### Smoothed tests with stlf (might be broken)

This will have to be done “the old way”, because tSCV() cannot be given
two different series for error calculation. **THIS WILL MAKE THE RESULTS
INCOMPARABLE**

``` r
# Filter
smooth25.pla.adj <- ts(itsmr::smooth.fft(ts.pla.adj, .25), start = 2013, 
                   end = c(2018, 12), frequency = 12)
smooth10.pla.adj <- ts(itsmr::smooth.fft(ts.pla.adj, .1), start = 2013, 
                   end = c(2018, 12), frequency = 12)

stlf_smooth_plot <- ggplot() 
stlf_smooth_cMAPE <- c()
stlf_smooth_RMSE <- c()
stlf_smooth_MAPE <- c()
# Loop 
for(i in seq(from = 36, to = 72, by = 1)){
  fit <- stlf(head(smooth25.pla.adj, i))  # Fit based on history so far
  fcast <- forecast(fit, h = 1)  # Forecast the next month
  segment <- ts.pla.adj[i + 1]  # Extract that year from the history for error
  
  # Build the plot piece by piece
  stlf_smooth_plot <- stlf_smooth_plot + autolayer(fcast)
  
  # Calculate raw forecast errors
  pe <- 100*(data.frame(fcast)$Point.Forecast - segment)/segment
  crt <- mean(ifelse(test = pe < 0, yes = abs(pe * 2), no = pe), na.rm = TRUE)
  stlf_smooth_cMAPE <- c(stlf_smooth_cMAPE, crt) 
  
  stlf_smooth_RMSE <- c(stlf_smooth_RMSE, data.frame(accuracy(fcast, segment))$RMSE)
  stlf_smooth_MAPE <- c(stlf_smooth_MAPE, data.frame(accuracy(fcast, segment))$MAPE)
}

stlf_smooth_plot + ggtitle("STLF forecast of adjusted smoothed (.25) platelet sales month by month") +
  scale_x_discrete(limits=c(2013, 2014, 2015, 2016, 2017, 2018)) + xlab("Time") +
  ylab("Unit sales") + autolayer(window(ts.pla.adj, start = 2013), colour = FALSE) +
  geom_text(aes(2018, 200, label = paste("RMSE: ", as.name(mean(stlf_smooth_RMSE, na.rm = TRUE))))) + 
  geom_text(aes(2018, 225, label = paste("MAPE: ", as.name(mean(stlf_smooth_MAPE, na.rm = TRUE))))) +
  geom_text(aes(2018, 250, label = paste("cMAPE: ", as.name(mean(stlf_smooth_cMAPE, na.rm = TRUE)))))
```

![](benchmarking_lab_pla_files/figure-gfm/filtering25-1.png)<!-- -->

``` r
stl_smooth_plot <- ggplot() 
stl_smooth_cMAPE <- c()
stl_smooth_RMSE <- c()
stl_smooth_MAPE <- c()
# Loop 
for(i in seq(from = 36, to = 72, by = 1)){
  fit <- stlf(head(smooth10.pla.adj, i), s.window = "periodic", t.window = 7)  # Fit based on history so far
  fcast <- forecast(fit, h = 1)  # Forecast the next month
  segment <- ts.pla.adj[i + 1]  # Extract that year from the history for error
  
  # Build the plot piece by piece
  stl_smooth_plot <- stl_smooth_plot + autolayer(fcast)
  
  # Calculate raw forecast errors
  pe <- 100*(data.frame(fcast)$Point.Forecast - segment)/segment
  crt <- mean(ifelse(test = pe < 0, yes = abs(pe * 2), no = pe), na.rm = TRUE)
  stl_smooth_cMAPE <- c(stl_smooth_cMAPE, crt) 
  
  stl_smooth_RMSE <- c(stl_smooth_RMSE, data.frame(accuracy(fcast, segment))$RMSE)
  stl_smooth_MAPE <- c(stl_smooth_MAPE, data.frame(accuracy(fcast, segment))$MAPE)
}

stl_smooth_plot + ggtitle("STLF forecast of adjusted smoothed (.10) platelet sales month by month") +
  scale_x_discrete(limits=c(2013, 2014, 2015, 2016, 2017, 2018)) + xlab("Time") +
  ylab("Unit sales") + autolayer(window(ts.pla.adj, start = 2013), colour = FALSE) +
  geom_text(aes(2018, 200, label = paste("RMSE: ", as.name(mean(stl_smooth_RMSE, na.rm = TRUE))))) + 
  geom_text(aes(2018, 215, label = paste("MAPE: ", as.name(mean(stl_smooth_MAPE, na.rm = TRUE))))) +
  geom_text(aes(2018, 230, label = paste("cMAPE: ", as.name(mean(stl_smooth_cMAPE, na.rm = TRUE)))))
```

![](benchmarking_lab_pla_files/figure-gfm/filtering10-1.png)<!-- -->

The smoothing seems to be somewhat effective in minimizing the error.
This is however not exactly comparable. I’ll have to look into making
these comparable with other metrics.

### Regression

``` r
reg.e <- tsCV(ts.pla.adj, freg, h = 1)
reg.crit <- cMAPE(reg.e, ts.pla.adj)
reg.mape <- mean(abs(100*reg.e/ts.pla.adj), na.rm = TRUE)
reg.rmse <- sqrt(mean(reg.e^2, na.rm = TRUE))

regbench <- matrix(c(reg.crit, reg.mape, reg.rmse),
               ncol = 3,
               byrow = TRUE)
colnames(regbench) <- c("cMAPE", "MAPE", "RMSE")
rownames(regbench) <- c("linReg")
kable(regbench, "markdown")
```

|        |    cMAPE |     MAPE |     RMSE |
| :----- | -------: | -------: | -------: |
| linReg | 13.04789 | 7.978745 | 14.86589 |

Simple linear regression does not perform that well. We’ll make it
dynamic by using dummy variables for months:

``` r
jan <- rep(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 6)
feb <- rep(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 6)
mar <- rep(c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 6)
apr <- rep(c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), 6)
may <- rep(c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), 6)
jun <- rep(c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), 6)
jul <- rep(c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), 6)
aug <- rep(c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), 6)
sep <- rep(c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), 6)
oct <- rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), 6)
nov <- rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), 6)
month.m <- matrix(c(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov),
                  ncol = 11,
                  byrow = FALSE)

arima.e <- tsCV(ts.pla.adj, farima, xreg = month.m, h = 1)
arima.crit <- cMAPE(arima.e, ts.pla.adj)
arima.mape <- mean(abs(100*arima.e/ts.pla.adj), na.rm = TRUE)
arima.rmse <- sqrt(mean(arima.e^2, na.rm = TRUE))

arimabench <- matrix(c(arima.crit, arima.mape, arima.rmse),
               ncol = 3,
               byrow = TRUE)
colnames(arimabench) <- c("cMAPE", "MAPE", "RMSE")
rownames(arimabench) <- c("DynReg")
kable(arimabench, "markdown")
```

|        |    cMAPE |     MAPE |     RMSE |
| :----- | -------: | -------: | -------: |
| DynReg | 7.642331 | 7.642331 | 10.37273 |

DynReg seems to work best? (Why though?)