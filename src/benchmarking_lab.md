Benchmarking Lab
================

``` r
# Set working directory
# knitr::opts_knit$set(root.dir = "/home/esa/production_forecasts") # Working on Ubuntu
knitr::opts_knit$set(root.dir = "V:/production_forecasts") # Working home
```

## Create original dataset that should remain immutable throughout labbing

``` r
library(forecast)
library(ggplot2)
library(gridExtra)
library(knitr)
library(xlsx)
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
ts.red <- ts(d$Punasoluvalmisteet, 
             start = as.numeric(c(d$year[1], d$month_num[1])), 
             end = as.numeric(c(tail(d$year, 1), tail(d$month_num, 1))), 
             frequency = 12)  # This tells the series that it is monthly data
```

## Previously used production forecasts

These are our true benchmark before any improvements

``` r
# STL + ETS
old.stl.e <- tsCV(ts.red, fstl, t.window = 6, h = 1)
old.stl.crit <- cMAPE(old.stl.e, ts.red)
old.stl.mape <- mean(abs(100*old.stl.e/ts.red), na.rm = TRUE)
old.stl.rmse <- sqrt(mean(old.stl.e^2, na.rm = TRUE))

# ETS
old.ets.e <- tsCV(ts.red, fets, h = 1)
old.ets.crit <- cMAPE(old.ets.e, ts.red)
old.ets.mape <- mean(abs(100*old.ets.e/ts.red), na.rm = TRUE)
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
| old STL | 7.141126 | 4.564358 | 1076.400 |
| old ETS | 7.654155 | 4.823708 | 1147.769 |

## Get baseline benchmarks to beat

These are our absolute worst forecasts we can make. A random walk
forecast takes the previous value of the series and walks randomly to
some direction. A naive forecast will forecast only the previous value
of the series. A seasonal naive will forecast only the previous value of
the same season of the series. Mean forecast will forecast the series
mean.

``` r
# Random walk with drift
rwf.e <- tsCV(ts.red, rwf, drift = TRUE, h = 1)
rwf.crit <- cMAPE(rwf.e, ts.red)
rwf.rmse <- sqrt(mean(rwf.e^2, na.rm = TRUE))
rwf.mape <- mean(abs(100*rwf.e/ts.red), na.rm = TRUE)

# Naive
naive.e <- tsCV(ts.red, naive, h = 1)
naive.crit <- cMAPE(naive.e, ts.red)
naive.rmse <- sqrt(mean(naive.e^2, na.rm = TRUE))
naive.mape <- mean(abs(100*naive.e/ts.red), na.rm = TRUE)

# Seasonal naive
snaive.e <- tsCV(ts.red, snaive, h = 1)
snaive.crit <- cMAPE(snaive.e, ts.red)
snaive.rmse <- sqrt(mean(snaive.e^2, na.rm = TRUE))
snaive.mape <- mean(abs(100*snaive.e/ts.red), na.rm = TRUE)

# Mean forecast
meanf.e <- tsCV(ts.red, meanf, h = 1)
meanf.crit <- cMAPE(meanf.e, ts.red)
meanf.rmse <- sqrt(mean(meanf.e^2, na.rm = TRUE))
meanf.mape <- mean(abs(100*meanf.e/ts.red), na.rm = TRUE)

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

|        |     cMAPE |      MAPE |     RMSE |
| :----- | --------: | --------: | -------: |
| RWF    | 11.896087 |  8.111792 | 1851.276 |
| NAIVE  | 11.814000 |  7.964862 | 1804.753 |
| SNAIVE |  9.352565 |  5.565415 | 1360.658 |
| MEANF  | 22.727547 | 11.931846 | 2537.142 |

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
ts.red.cut <- window(ts.red, start = 2013)
autoplot(ts.red.cut) + ggtitle("Red cell sales from 2013 onwards")
```

![](benchmarking_lab_files/figure-gfm/cut_series-1.png)<!-- --> We will
now run all of our benchmarks again in a single chunk (to save space).

``` r
# STL + ETS
old.stl.e <- tsCV(ts.red.cut, fstl, t.window = 6, h = 1)
old.stl.crit <- cMAPE(old.stl.e, ts.red.cut)
old.stl.mape <- mean(abs(100*old.stl.e/ts.red.cut), na.rm = TRUE)
old.stl.rmse <- sqrt(mean(old.stl.e^2, na.rm = TRUE))

# ETS
old.ets.e <- tsCV(ts.red.cut, fets, h = 1)
old.ets.crit <- cMAPE(old.ets.e, ts.red.cut)
old.ets.mape <- mean(abs(100*old.ets.e/ts.red.cut), na.rm = TRUE)
old.ets.rmse <- sqrt(mean(old.ets.e^2, na.rm = TRUE))

# Random walk with drift
rwf.e <- tsCV(ts.red.cut, rwf, drift = TRUE, h = 1)
rwf.crit <- cMAPE(rwf.e, ts.red.cut)
rwf.rmse <- sqrt(mean(rwf.e^2, na.rm = TRUE))
rwf.mape <- mean(abs(100*rwf.e/ts.red.cut), na.rm = TRUE)

# Naive
naive.e <- tsCV(ts.red.cut, naive, h = 1)
naive.crit <- cMAPE(naive.e, ts.red.cut)
naive.rmse <- sqrt(mean(naive.e^2, na.rm = TRUE))
naive.mape <- mean(abs(100*naive.e/ts.red.cut), na.rm = TRUE)

# Seasonal naive
snaive.e <- tsCV(ts.red.cut, snaive, h = 1)
snaive.crit <- cMAPE(snaive.e, ts.red.cut)
snaive.rmse <- sqrt(mean(snaive.e^2, na.rm = TRUE))
snaive.mape <- mean(abs(100*snaive.e/ts.red.cut), na.rm = TRUE)

# Mean forecast
meanf.e <- tsCV(ts.red.cut, meanf, h = 1)
meanf.crit <- cMAPE(meanf.e, ts.red.cut)
meanf.rmse <- sqrt(mean(meanf.e^2, na.rm = TRUE))
meanf.mape <- mean(abs(100*meanf.e/ts.red.cut), na.rm = TRUE)

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

|        |     cMAPE |     MAPE |      RMSE |
| :----- | --------: | -------: | --------: |
| STL    |  7.083388 | 4.791456 |  926.5694 |
| ETS    |  7.925400 | 5.304817 | 1073.5925 |
| RWF    | 11.404434 | 8.060954 | 1550.7773 |
| NAIVE  | 11.302589 | 7.572563 | 1483.4533 |
| SNAIVE | 10.633076 | 5.951755 | 1268.0075 |
| MEANF  | 10.598851 | 5.688717 | 1166.2895 |

Here we observe the importance of choosing appropriate metrics. The
critical MAPE and RMSE improved for our old STL+ETS forecast, while the
MAPE got worse. I believe we should primarily follow cMAPE and
secondarily RMSE. For ETS, the cMAPE and MAPE got worse, but RMSE
improved. Our crude benchmarks all improved according to RMSE, but
SNAIVE got slightly worse and now the MEANF outperforms it.

## Business day adjustment (series between 2013 - 2018)

``` r
# Load business day data
biz <- read.xlsx("./data/workdays_monthly.xlsx", sheetName = "Tabella")
colnames(biz) <- c("month", "days")

# Business day adjustment
ts.red.adj <- window(ts.red, start = 2013, end = c(2018, 12)) / tail(head(biz$days, 84), 72)
```

Let’s see how our series looks now

``` r
autoplot(ts.red.adj) + ggtitle("Red cell sales 2013-2018, adjusted")
```

![](benchmarking_lab_files/figure-gfm/visualize_adj-1.png)<!-- -->

``` r
ggseasonplot(ts.red.adj)
```

![](benchmarking_lab_files/figure-gfm/visualize_adj-2.png)<!-- -->

We will build all our future models based on this adjustment. Let’s see
if it gives any improvement on our earlier benchmarks:

``` r
# STL + ETS
new.stl.e <- tsCV(ts.red.adj, fstl, t.window = 6, h = 1)
new.stl.crit <- cMAPE(new.stl.e, ts.red.adj)
new.stl.mape <- mean(abs(100*new.stl.e/ts.red.adj), na.rm = TRUE)
new.stl.rmse <- sqrt(mean(new.stl.e^2, na.rm = TRUE))

# ETS
new.ets.e <- tsCV(ts.red.adj, fets, h = 1)
new.ets.crit <- cMAPE(new.ets.e, ts.red.adj)
new.ets.mape <- mean(abs(100*new.ets.e/ts.red.adj), na.rm = TRUE)
new.ets.rmse <- sqrt(mean(new.ets.e^2, na.rm = TRUE))

# Random walk with drift
rwf.e.adj <- tsCV(ts.red.adj, rwf, drift = TRUE, h = 1)
rwf.crit.adj <- cMAPE(rwf.e.adj, ts.red.adj)
rwf.rmse.adj <- sqrt(mean(rwf.e.adj^2, na.rm = TRUE))
rwf.mape.adj <- mean(abs(100*rwf.e.adj/ts.red.adj), na.rm = TRUE)

# Naive
naive.e.adj <- tsCV(ts.red.adj, naive, h = 1)
naive.crit.adj <- cMAPE(naive.e.adj, ts.red.adj)
naive.rmse.adj <- sqrt(mean(naive.e.adj^2, na.rm = TRUE))
naive.mape.adj <- mean(abs(100*naive.e.adj/ts.red.adj), na.rm = TRUE)

# Seasonal naive
snaive.e.adj <- tsCV(ts.red.adj, snaive, h = 1)
snaive.crit.adj <- cMAPE(snaive.e.adj, ts.red.adj)
snaive.rmse.adj <- sqrt(mean(snaive.e.adj^2, na.rm = TRUE))
snaive.mape.adj <- mean(abs(100*snaive.e.adj/ts.red.adj), na.rm = TRUE)

# Mean forecast
meanf.e.adj <- tsCV(ts.red.adj, meanf, h = 1)
meanf.crit.adj <- cMAPE(meanf.e.adj, ts.red.adj)
meanf.rmse.adj <- sqrt(mean(meanf.e.adj^2, na.rm = TRUE))
meanf.mape.adj <- mean(abs(100*meanf.e.adj/ts.red.adj), na.rm = TRUE)

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

|        |     cMAPE |     MAPE |     RMSE |
| :----- | --------: | -------: | -------: |
| STL    |  5.703543 | 4.070773 | 39.35312 |
| ETS    |  6.950004 | 4.370791 | 45.70078 |
| RWF    |  8.149471 | 5.750502 | 58.54942 |
| NAIVE  |  8.175598 | 5.530553 | 56.05780 |
| SNAIVE |  9.955107 | 5.722944 | 57.35310 |
| MEANF  | 10.900852 | 5.782239 | 55.77007 |

## Rolling windows

``` r
# 4 YEARS

# STL + ETS
new.stl.4.e <- tsCV(ts.red.adj, fstl, t.window = 6, h = 1, window = 48)
new.stl.4.crit <- cMAPE(new.stl.4.e, ts.red.adj)
new.stl.4.mape <- mean(abs(100*new.stl.4.e/ts.red.adj), na.rm = TRUE)
new.stl.4.rmse <- sqrt(mean(new.stl.4.e^2, na.rm = TRUE))

# ETS
new.ets.4.e <- tsCV(ts.red.adj, fets, h = 1, window = 48)
new.ets.4.crit <- cMAPE(new.ets.4.e, ts.red.adj)
new.ets.4.mape <- mean(abs(100*new.ets.4.e/ts.red.adj), na.rm = TRUE)
new.ets.4.rmse <- sqrt(mean(new.ets.4.e^2, na.rm = TRUE))

# Random walk with drift
rwf.e.4.adj <- tsCV(ts.red.adj, rwf, drift = TRUE, h = 1, window = 48)
rwf.crit.4.adj <- cMAPE(rwf.e.4.adj, ts.red.adj)
rwf.rmse.4.adj <- sqrt(mean(rwf.e.4.adj^2, na.rm = TRUE))
rwf.mape.4.adj <- mean(abs(100*rwf.e.4.adj/ts.red.adj), na.rm = TRUE)

# Naive
naive.e.4.adj <- tsCV(ts.red.adj, naive, h = 1, window = 48)
naive.crit.4.adj <- cMAPE(naive.e.4.adj, ts.red.adj)
naive.rmse.4.adj <- sqrt(mean(naive.e.4.adj^2, na.rm = TRUE))
naive.mape.4.adj <- mean(abs(100*naive.e.4.adj/ts.red.adj), na.rm = TRUE)

# Seasonal naive
snaive.e.4.adj <- tsCV(ts.red.adj, snaive, h = 1, window = 48)
snaive.crit.4.adj <- cMAPE(snaive.e.4.adj, ts.red.adj)
snaive.rmse.4.adj <- sqrt(mean(snaive.e.4.adj^2, na.rm = TRUE))
snaive.mape.4.adj <- mean(abs(100*snaive.e.4.adj/ts.red.adj), na.rm = TRUE)

# Mean forecast
meanf.e.4.adj <- tsCV(ts.red.adj, meanf, h = 1, window = 48)
meanf.crit.4.adj <- cMAPE(meanf.e.4.adj, ts.red.adj)
meanf.rmse.4.adj <- sqrt(mean(meanf.e.4.adj^2, na.rm = TRUE))
meanf.mape.4.adj <- mean(abs(100*meanf.e.4.adj/ts.red.adj), na.rm = TRUE)


# 3 YEARS

# STL + ETS
new.stl.3.e <- tsCV(ts.red.adj, fstl, t.window = 6, h = 1, window = 36)
new.stl.3.crit <- cMAPE(new.stl.3.e, ts.red.adj)
new.stl.3.mape <- mean(abs(100*new.stl.3.e/ts.red.adj), na.rm = TRUE)
new.stl.3.rmse <- sqrt(mean(new.stl.3.e^2, na.rm = TRUE))

# ETS
new.ets.3.e <- tsCV(ts.red.adj, fets, h = 1, window = 36)
new.ets.3.crit <- cMAPE(new.ets.3.e, ts.red.adj)
new.ets.3.mape <- mean(abs(100*new.ets.3.e/ts.red.adj), na.rm = TRUE)
new.ets.3.rmse <- sqrt(mean(new.ets.3.e^2, na.rm = TRUE))

# Random walk with drift
rwf.e.3.adj <- tsCV(ts.red.adj, rwf, drift = TRUE, h = 1, window = 36)
rwf.crit.3.adj <- cMAPE(rwf.e.3.adj, ts.red.adj)
rwf.rmse.3.adj <- sqrt(mean(rwf.e.3.adj^2, na.rm = TRUE))
rwf.mape.3.adj <- mean(abs(100*rwf.e.3.adj/ts.red.adj), na.rm = TRUE)

# Naive
naive.e.3.adj <- tsCV(ts.red.adj, naive, h = 1, window = 36)
naive.crit.3.adj <- cMAPE(naive.e.3.adj, ts.red.adj)
naive.rmse.3.adj <- sqrt(mean(naive.e.3.adj^2, na.rm = TRUE))
naive.mape.3.adj <- mean(abs(100*naive.e.3.adj/ts.red.adj), na.rm = TRUE)

# Seasonal naive
snaive.e.3.adj <- tsCV(ts.red.adj, snaive, h = 1, window = 36)
snaive.crit.3.adj <- cMAPE(snaive.e.3.adj, ts.red.adj)
snaive.rmse.3.adj <- sqrt(mean(snaive.e.3.adj^2, na.rm = TRUE))
snaive.mape.3.adj <- mean(abs(100*snaive.e.3.adj/ts.red.adj), na.rm = TRUE)

# Mean forecast
meanf.e.3.adj <- tsCV(ts.red.adj, meanf, h = 1, window = 36)
meanf.crit.3.adj <- cMAPE(meanf.e.3.adj, ts.red.adj)
meanf.rmse.3.adj <- sqrt(mean(meanf.e.3.adj^2, na.rm = TRUE))
meanf.mape.3.adj <- mean(abs(100*meanf.e.3.adj/ts.red.adj), na.rm = TRUE)


# 2 YEARS

# STL + ETS (not possible)
new.stl.2.crit <- NA
new.stl.2.mape <- NA
new.stl.2.rmse <- NA

# ETS
new.ets.2.e <- tsCV(ts.red.adj, fets, h = 1, window = 24)
new.ets.2.crit <- cMAPE(new.ets.2.e, ts.red.adj)
new.ets.2.mape <- mean(abs(100*new.ets.2.e/ts.red.adj), na.rm = TRUE)
new.ets.2.rmse <- sqrt(mean(new.ets.2.e^2, na.rm = TRUE))

# Random walk with drift
rwf.e.2.adj <- tsCV(ts.red.adj, rwf, drift = TRUE, h = 1, window = 24)
rwf.crit.2.adj <- cMAPE(rwf.e.2.adj, ts.red.adj)
rwf.rmse.2.adj <- sqrt(mean(rwf.e.2.adj^2, na.rm = TRUE))
rwf.mape.2.adj <- mean(abs(100*rwf.e.2.adj/ts.red.adj), na.rm = TRUE)

# Naive
naive.e.2.adj <- tsCV(ts.red.adj, naive, h = 1, window = 24)
naive.crit.2.adj <- cMAPE(naive.e.2.adj, ts.red.adj)
naive.rmse.2.adj <- sqrt(mean(naive.e.2.adj^2, na.rm = TRUE))
naive.mape.2.adj <- mean(abs(100*naive.e.2.adj/ts.red.adj), na.rm = TRUE)

# Seasonal naive
snaive.e.2.adj <- tsCV(ts.red.adj, snaive, h = 1, window = 24)
snaive.crit.2.adj <- cMAPE(snaive.e.2.adj, ts.red.adj)
snaive.rmse.2.adj <- sqrt(mean(snaive.e.2.adj^2, na.rm = TRUE))
snaive.mape.2.adj <- mean(abs(100*snaive.e.2.adj/ts.red.adj), na.rm = TRUE)

# Mean forecast
meanf.e.2.adj <- tsCV(ts.red.adj, meanf, h = 1, window = 24)
meanf.crit.2.adj <- cMAPE(meanf.e.2.adj, ts.red.adj)
meanf.rmse.2.adj <- sqrt(mean(meanf.e.2.adj^2, na.rm = TRUE))
meanf.mape.2.adj <- mean(abs(100*meanf.e.2.adj/ts.red.adj), na.rm = TRUE)


# 1 YEAR

# STL + ETS (not possible)
new.stl.1.crit <- NA
new.stl.1.mape <- NA
new.stl.1.rmse <- NA

# ETS
new.ets.1.e <- tsCV(ts.red.adj, fets, h = 1, window = 12)
new.ets.1.crit <- cMAPE(new.ets.1.e, ts.red.adj)
new.ets.1.mape <- mean(abs(100*new.ets.1.e/ts.red.adj), na.rm = TRUE)
new.ets.1.rmse <- sqrt(mean(new.ets.1.e^2, na.rm = TRUE))

# Random walk with drift
rwf.e.1.adj <- tsCV(ts.red.adj, rwf, drift = TRUE, h = 1, window = 12)
rwf.crit.1.adj <- cMAPE(rwf.e.1.adj, ts.red.adj)
rwf.rmse.1.adj <- sqrt(mean(rwf.e.1.adj^2, na.rm = TRUE))
rwf.mape.1.adj <- mean(abs(100*rwf.e.1.adj/ts.red.adj), na.rm = TRUE)

# Naive
naive.e.1.adj <- tsCV(ts.red.adj, naive, h = 1, window = 12)
naive.crit.1.adj <- cMAPE(naive.e.1.adj, ts.red.adj)
naive.rmse.1.adj <- sqrt(mean(naive.e.1.adj^2, na.rm = TRUE))
naive.mape.1.adj <- mean(abs(100*naive.e.1.adj/ts.red.adj), na.rm = TRUE)

# Seasonal naive
snaive.e.1.adj <- tsCV(ts.red.adj, snaive, h = 1, window = 12)
snaive.crit.1.adj <- cMAPE(snaive.e.1.adj, ts.red.adj)
snaive.rmse.1.adj <- sqrt(mean(snaive.e.1.adj^2, na.rm = TRUE))
snaive.mape.1.adj <- mean(abs(100*snaive.e.1.adj/ts.red.adj), na.rm = TRUE)

# Mean forecast
meanf.e.1.adj <- tsCV(ts.red.adj, meanf, h = 1, window = 12)
meanf.crit.1.adj <- cMAPE(meanf.e.1.adj, ts.red.adj)
meanf.rmse.1.adj <- sqrt(mean(meanf.e.1.adj^2, na.rm = TRUE))
meanf.mape.1.adj <- mean(abs(100*meanf.e.1.adj/ts.red.adj), na.rm = TRUE)

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

|          |    cMAPE |     MAPE |     RMSE |
| :------- | -------: | -------: | -------: |
| STL 4    | 4.178543 | 3.169198 | 33.44389 |
| ETS 4    | 5.097152 | 3.941288 | 42.44701 |
| RWF 4    | 7.799263 | 5.605564 | 51.44502 |
| NAIVE 4  | 7.747074 | 5.493773 | 50.62243 |
| SNAIVE 4 | 6.016787 | 4.034754 | 37.50954 |
| MEANF 4  | 9.619202 | 5.346465 | 52.94899 |
| STL 3    | 4.780911 | 3.269495 | 34.22830 |
| ETS 3    | 5.476609 | 3.901995 | 39.99024 |
| RWF 3    | 7.231668 | 5.063043 | 46.46749 |
| NAIVE 3  | 7.249407 | 5.000528 | 45.97713 |
| SNAIVE 3 | 6.723504 | 4.222665 | 38.74439 |
| MEANF 3  | 8.256172 | 4.617649 | 46.71220 |
| STL 2    |       NA |       NA |       NA |
| ETS 2    | 7.182938 | 4.382645 | 44.76268 |
| RWF 2    | 7.826697 | 5.397157 | 49.71760 |
| NAIVE 2  | 7.675344 | 5.222206 | 48.37086 |
| SNAIVE 2 | 7.745853 | 4.730246 | 46.96852 |
| MEANF 2  | 7.170742 | 4.150209 | 43.85499 |
| STL 1    |       NA |       NA |       NA |
| ETS 1    | 6.922765 | 4.286987 | 43.03104 |
| RWF 1    | 8.480759 | 5.758900 | 56.34982 |
| NAIVE 1  | 8.201232 | 5.503663 | 53.54849 |
| SNAIVE 1 | 9.216546 | 5.422790 | 54.08857 |
| MEANF 1  | 6.716493 | 4.132087 | 42.06495 |

*The series isn’t long enough to test rolling windows reliably\!* These
will have to be ignored, probably.

## New models

### Moving average

``` r
# 5-MA
ma5.e.adj <- tsCV(ts.red.adj, fMA, order = 5, h = 1)
ma5.crit.adj <- cMAPE(ma5.e.adj, ts.red.adj)
ma5.rmse.adj <- sqrt(mean(ma5.e.adj^2, na.rm = TRUE))
ma5.mape.adj <- mean(abs(100*ma5.e.adj/ts.red.adj), na.rm = TRUE)

# 7-MA
ma7.e.adj <- tsCV(ts.red.adj, fMA, order = 7, h = 1)
ma7.crit.adj <- cMAPE(ma7.e.adj, ts.red.adj)
ma7.rmse.adj <- sqrt(mean(ma7.e.adj^2, na.rm = TRUE))
ma7.mape.adj <- mean(abs(100*ma7.e.adj/ts.red.adj), na.rm = TRUE)

# 9-MA
ma9.e.adj <- tsCV(ts.red.adj, fMA, order = 9, h = 1)
ma9.crit.adj <- cMAPE(ma9.e.adj, ts.red.adj)
ma9.rmse.adj <- sqrt(mean(ma9.e.adj^2, na.rm = TRUE))
ma9.mape.adj <- mean(abs(100*ma9.e.adj/ts.red.adj), na.rm = TRUE)

# 12-MA
ma12.e.adj <- tsCV(ts.red.adj, fMA, order = 12, h = 1)
ma12.crit.adj <- cMAPE(ma12.e.adj, ts.red.adj)
ma12.rmse.adj <- sqrt(mean(ma12.e.adj^2, na.rm = TRUE))
ma12.mape.adj <- mean(abs(100*ma12.e.adj/ts.red.adj), na.rm = TRUE)

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

|       |    cMAPE |     MAPE |     RMSE |
| :---- | -------: | -------: | -------: |
| 5-MA  | 7.289893 | 4.728964 | 49.37525 |
| 7-MA  | 6.701365 | 4.341072 | 45.10486 |
| 9-MA  | 6.871238 | 4.401900 | 45.06010 |
| 12-MA | 6.698926 | 4.161522 | 42.27363 |

There was slight imporvement in extending the moving average window, but
these scores aren’t even remotely competitive.

### Complex decompositions

``` r
stlf.e.adj <- tsCV(ts.red.adj, stlf, h = 1)
stlf.crit.adj <- cMAPE(stlf.e.adj, ts.red.adj)
stlf.mape.adj <- mean(abs(100*stlf.e.adj/ts.red.adj), na.rm = TRUE)
stlf.rmse.adj <- sqrt(mean(stlf.e.adj^2, na.rm = TRUE))

tbats.e.adj <- tsCV(ts.red.adj, fTBATS, h = 1)
tbats.crit.adj <- cMAPE(tbats.e.adj, ts.red.adj)
tbats.mape.adj <- mean(abs(100*tbats.e.adj/ts.red.adj), na.rm = TRUE)
tbats.rmse.adj <- sqrt(mean(tbats.e.adj^2, na.rm = TRUE))

complex <- matrix(c(stlf.crit.adj, stlf.mape.adj, stlf.rmse.adj,
                    tbats.crit.adj, tbats.mape.adj, tbats.rmse.adj),
                  ncol = 3,
                  byrow = TRUE)
colnames(complex) <- c("cMAPE", "MAPE", "RMSE")
rownames(complex) <- c("STLF", "TBATS")
kable(complex, "markdown")
```

|       |    cMAPE |     MAPE |     RMSE |
| :---- | -------: | -------: | -------: |
| STLF  | 5.421608 | 3.819665 | 37.44415 |
| TBATS | 7.685357 | 5.266274 | 60.58749 |

Multiple seasonal decomposition (STLF) sets our new record at 5.42\!

### A neural network

``` r
nnetar.e <- tsCV(ts.red.adj, fnnet, h = 1)
nnetar.crit <- cMAPE(nnetar.e, ts.red.adj)
nnetar.mape <- mean(abs(100*nnetar.e/ts.red.adj), na.rm = TRUE)
nnetar.rmse <- sqrt(mean(nnetar.e^2, na.rm = TRUE))

nnet <- matrix(c(nnetar.crit, nnetar.mape, nnetar.rmse),
               ncol = 3,
               byrow = TRUE)
colnames(nnet) <- c("cMAPE", "MAPE", "RMSE")
rownames(nnet) <- c("NN")
kable(nnet, "markdown")
```

|    |    cMAPE |    MAPE |     RMSE |
| :- | -------: | ------: | -------: |
| NN | 8.368261 | 5.70301 | 67.31929 |

No improvement with a crude NN, only to be expected.

### Smoothed tests with stlf (might be broken)

This will have to be done “the old way”, because tSCV() cannot be given
two different series for error calculation. **THIS WILL MAKE THE RESULTS
INCOMPARABLE**

``` r
# Filter
smooth25.red.adj <- ts(itsmr::smooth.fft(ts.red.adj, .25), start = 2013, 
                   end = c(2018, 12), frequency = 12)
smooth10.red.adj <- ts(itsmr::smooth.fft(ts.red.adj, .1), start = 2013, 
                   end = c(2018, 12), frequency = 12)

stlf_smooth_plot <- ggplot() 
stlf_smooth_cMAPE <- c()
stlf_smooth_RMSE <- c()
stlf_smooth_MAPE <- c()
# Loop 
for(i in seq(from = 36, to = 72, by = 1)){
  fit <- stlf(head(smooth25.red.adj, i))  # Fit based on history so far
  fcast <- forecast(fit, h = 1)  # Forecast the next month
  segment <- ts.red.adj[i + 1]  # Extract that year from the history for error
  
  # Build the plot piece by piece
  stlf_smooth_plot <- stlf_smooth_plot + autolayer(fcast)
  
  # Calculate raw forecast errors
  pe <- 100*(data.frame(fcast)$Point.Forecast - segment)/segment
  crt <- mean(ifelse(test = pe < 0, yes = abs(pe * 2), no = pe), na.rm = TRUE)
  stlf_smooth_cMAPE <- c(stlf_smooth_cMAPE, crt) 
  
  stlf_smooth_RMSE <- c(stlf_smooth_RMSE, data.frame(accuracy(fcast, segment))$RMSE)
  stlf_smooth_MAPE <- c(stlf_smooth_MAPE, data.frame(accuracy(fcast, segment))$MAPE)
}

stlf_smooth_plot + ggtitle("STLF forecast of adjusted smoothed (.25) red cell sales month by month") +
  scale_x_discrete(limits=c(2013, 2014, 2015, 2016, 2017, 2018)) + xlab("Time") +
  ylab("Unit sales") + autolayer(window(ts.red.adj, start = 2013), colour = FALSE) +
  geom_text(aes(2018, 1000, label = paste("RMSE: ", as.name(mean(stlf_smooth_RMSE, na.rm = TRUE))))) + 
  geom_text(aes(2018, 1050, label = paste("MAPE: ", as.name(mean(stlf_smooth_MAPE, na.rm = TRUE))))) +
  geom_text(aes(2018, 1100, label = paste("cMAPE: ", as.name(mean(stlf_smooth_cMAPE, na.rm = TRUE)))))
```

![](benchmarking_lab_files/figure-gfm/filtering25-1.png)<!-- -->

``` r
stl_smooth_plot <- ggplot() 
stl_smooth_cMAPE <- c()
stl_smooth_RMSE <- c()
stl_smooth_MAPE <- c()
# Loop 
for(i in seq(from = 36, to = 72, by = 1)){
  fit <- stl(head(smooth10.red.adj, i), s.window = "periodic", t.window = 7)  # Fit based on history so far
  fcast <- forecast(fit, h = 1)  # Forecast the next month
  segment <- ts.red.adj[i + 1]  # Extract that year from the history for errror
  
  # Build the plot piece by piece
  stl_smooth_plot <- stl_smooth_plot + autolayer(fcast)
  
  # Calculate raw forecast errors
  pe <- 100*(data.frame(fcast)$Point.Forecast - segment)/segment
  crt <- mean(ifelse(test = pe < 0, yes = abs(pe * 2), no = pe), na.rm = TRUE)
  stl_smooth_cMAPE <- c(stl_smooth_cMAPE, crt) 
  
  stl_smooth_RMSE <- c(stl_smooth_RMSE, data.frame(accuracy(fcast, segment))$RMSE)
  stl_smooth_MAPE <- c(stl_smooth_MAPE, data.frame(accuracy(fcast, segment))$MAPE)
}

stl_smooth_plot + ggtitle("STLF forecast of adjusted smoothed (.10) red cell sales month by month") +
  scale_x_discrete(limits=c(2013, 2014, 2015, 2016, 2017, 2018)) + xlab("Time") +
  ylab("Unit sales") + autolayer(window(ts.red.adj, start = 2013), colour = FALSE) +
  geom_text(aes(2018, 1000, label = paste("RMSE: ", as.name(mean(stl_smooth_RMSE, na.rm = TRUE))))) + 
  geom_text(aes(2018, 1050, label = paste("MAPE: ", as.name(mean(stl_smooth_MAPE, na.rm = TRUE))))) +
  geom_text(aes(2018, 1100, label = paste("cMAPE: ", as.name(mean(stl_smooth_cMAPE, na.rm = TRUE)))))
```

![](benchmarking_lab_files/figure-gfm/filtering10-1.png)<!-- -->

The smoothing seems to be somewhat effective in minimizing the error.
This is however not exactly comparable. I’ll have to look into making
these comparable with other metrics.