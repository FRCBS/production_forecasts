Delivery Lab: Red Cells (all)
================

``` r
library(forecast)
library(ggplot2)
library(gridExtra)
library(knitr)
library(readxl)
library(plyr)
library(lubridate)
library(numbers)
library(data.table)
library(R.utils)
source("src/evalhelp.R")
```

``` r
# Get all the files
dfold <- "/home/esa/production_forecasts/data/FACS/"
files <- list.files(path = dfold, pattern = "FAC0091_*")

# Compile a dataframe by going over all files
dlist <- list()
for (i in files) {
  # Read a single file to a df called d
  d <- read.delim(file = paste0(dfold, "/", i), header = FALSE, sep = ";", stringsAsFactors = FALSE, colClasses = 'character')
  if(length(d) == 26){
    d <- d[, !(names(d) %in% c("V10"))]  # The column numbers unfortunately vary between files, so we'll adjust
    }
  colnames(d) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10",
                   "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20",
                   "V21", "V22", "V23", "V24", "V25")  # This is done so as to have easier column handling later on
  dlist[[i]] <- d
}

d <- as.data.frame(rbindlist(dlist, fill = TRUE))

# Sales
sales <- read.table("./data/kuukausimyynti.txt", header = T, sep = "\t")
# Sales data begins January 2004 and ends on April 2019
sales$date <- seq(from = as.Date("2004-01-01"), to = as.Date("2019-04-01"), by = "month")
# We'll be examining only red cells for now, so we'll drop everything we don't need
keep = c("Punasoluvalmisteet", "date")
sales <- sales[keep]
```

``` r
# Divide into distributions (P) and returns (R)
P <- d[d$V1 == "P", ]
R <- d[d$V1 == "R", ]

# For distributions, we'll keep Distribution date, Quantity, ABO type, Volume, Exp date
keep <- c("V12", "V14", "V18", "V20", "V22", "V24")
distr <- P[keep]
colnames(distr) <- c("date", "product", "quantity", "ABO", "volume", "exp")

# For returns we keep the return date
keep <- c("V4", "V7")
retrn <- R[keep]
colnames(retrn) <- c("date", "quantity")

# Datify
distr$date <- dmy(distr$date); distr$exp <- dmy(distr$exp)
retrn$date <- dmy(retrn$date)

# Numerify
distr$quantity <- as.numeric(distr$quantity); distr$volume <- as.numeric(distr$volume)
retrn$quantity <- as.numeric(retrn$quantity)
```

``` r
# budKpl    # Blood product sale amendment code

# Product codes for red cell products
codes <- c("budTR001", "A0071V00", "A0074V00", "A0092VA0", "A0092VB0", 
           "E3844V00", "E3845V00", "E3846VA0", "E3846VB0", "E3846VC0",
           "E3846V00", "E3847VA0", "E3847VB0", "E3847VC0", "E3847V00", 
           "E3936VA0", "E3936VB0", "E3939V00", "E3940V00", "E4683V00",
           "E7668V00", "E7673V00", "E4999V00", "E5000V00")

distr <- distr[distr$product %in% codes, ]
```

``` r
all.distr <- aggregate(distr$quantity, by = list(distr$date), sum); colnames(all.distr) <- c("date", "pcs")
all.retrn <- aggregate(retrn$quantity, by = list(retrn$date), sum); colnames(all.retrn) <- c("date", "pcs")

# O minus
Ominus <- distr[distr$ABO == "O -", ]
Ominus.distr <- aggregate(Ominus$quantity, by = list(Ominus$date), sum); colnames(Ominus.distr) <- c("date", "pcs")

# O plus
Oplus <- distr[distr$ABO == "O +", ]
Oplus.distr <- aggregate(Oplus$quantity, by = list(Oplus$date), sum); colnames(Oplus.distr) <- c("date", "pcs")

# A minus
Aminus <- distr[distr$ABO == "A -", ]
Aminus.distr <- aggregate(Aminus$quantity, by = list(Aminus$date), sum); colnames(Aminus.distr) <- c("date", "pcs")

# A plus
Aplus <- distr[distr$ABO == "A +", ]
Aplus.distr <- aggregate(Aplus$quantity, by = list(Aplus$date), sum); colnames(Aplus.distr) <- c("date", "pcs")

# B minus
Bminus <- distr[distr$ABO == "B -", ]
Bminus.distr <- aggregate(Bminus$quantity, by = list(Bminus$date), sum); colnames(Bminus.distr) <- c("date", "pcs")

# B plus
Bplus <- distr[distr$ABO == "B +", ]
Bplus.distr <- aggregate(Bplus$quantity, by = list(Bplus$date), sum); colnames(Bplus.distr) <- c("date", "pcs")

# AB minus
ABminus <- distr[distr$ABO == "AB-", ]
ABminus.distr <- aggregate(ABminus$quantity, by = list(ABminus$date), sum); colnames(ABminus.distr) <- c("date", "pcs")

# AB plus
ABplus <- distr[distr$ABO == "AB+", ]
ABplus.distr <- aggregate(ABplus$quantity, by = list(ABplus$date), sum); colnames(ABplus.distr) <- c("date", "pcs")
```

*Notes for development phase: 4% of the data is missing “ABO” and
“volume” information. 4% is too much to be outright omitted, but now
we have the problem where the smaller ABO series don’t add up to
“all.distr”.*

## Data goodness checks

``` r
alldates <- seq(from = as.Date("2014-01-01"), to = as.Date("2019-07-07"), by = "day")
typedates <- list(all.distr$date, Ominus.distr$date, Oplus.distr$date, 
                  Aminus.distr$date, Aplus.distr$date, Bminus.distr$date, 
                  Bplus.distr$date, ABminus.distr$date, ABplus.distr$date)
types <- list("All", "O-", "O+", "A-", "A+", "B-", "B+", "AB-", "AB+")
for(i in seq(9)){
  cat("Missing observations in ", types[[i]], ": ", length(alldates[!alldates %in% typedates[[i]]]), "\n")
}
```

    ## Missing observations in  All :  0 
    ## Missing observations in  O- :  77 
    ## Missing observations in  O+ :  33 
    ## Missing observations in  A- :  210 
    ## Missing observations in  A+ :  37 
    ## Missing observations in  B- :  342 
    ## Missing observations in  B+ :  161 
    ## Missing observations in  AB- :  538 
    ## Missing observations in  AB+ :  350

Some blood types seem to have quite a lot of missing days. Let’s look at
the series more closely to get an estimate of the ratio between zeros
and actual missing data.

``` r
ggplot(data = all.distr, aes(x = pcs)) + geom_histogram(binwidth = 1) + 
  labs(title = "All",
       subtitle = "Smallest value found: 2",
       caption = "", 
       x = "pcs", y = "count")
```

![](delivery_lab_files/figure-gfm/histograms-1.png)<!-- -->

``` r
ggplot(data = Ominus.distr, aes(x = pcs)) + geom_histogram(binwidth = 1) + 
  labs(title = "O-",
       subtitle = "Missing days: 77",
       caption = "", 
       x = "pcs", y = "count")
```

![](delivery_lab_files/figure-gfm/histograms-2.png)<!-- -->

``` r
ggplot(data = Oplus.distr, aes(x = pcs)) + geom_histogram(binwidth = 1) + 
  labs(title = "O+",
       subtitle = "Missing days: 33",
       caption = "", 
       x = "pcs", y = "count")
```

![](delivery_lab_files/figure-gfm/histograms-3.png)<!-- -->

``` r
ggplot(data = Aminus.distr, aes(x = pcs)) + geom_histogram(binwidth = 1) + 
  labs(title = "A-",
       subtitle = "Missing days: 210",
       caption = "", 
       x = "pcs", y = "count")
```

![](delivery_lab_files/figure-gfm/histograms-4.png)<!-- -->

``` r
ggplot(data = Aplus.distr, aes(x = pcs)) + geom_histogram(binwidth = 1) + 
  labs(title = "A+",
       subtitle = "Missing days: 37",
       caption = "", 
       x = "pcs", y = "count")
```

![](delivery_lab_files/figure-gfm/histograms-5.png)<!-- -->

``` r
ggplot(data = Bminus.distr, aes(x = pcs)) + geom_histogram(binwidth = 1) + 
  labs(title = "B-",
       subtitle = "Missing days: 342",
       caption = "", 
       x = "pcs", y = "count")
```

![](delivery_lab_files/figure-gfm/histograms-6.png)<!-- -->

``` r
ggplot(data = Bplus.distr, aes(x = pcs)) + geom_histogram(binwidth = 1) + 
  labs(title = "B+",
       subtitle = "Missing days: 161",
       caption = "", 
       x = "pcs", y = "count")
```

![](delivery_lab_files/figure-gfm/histograms-7.png)<!-- -->

``` r
ggplot(data = ABminus.distr, aes(x = pcs)) + geom_histogram(binwidth = 1) + 
  labs(title = "AB-",
       subtitle = "Missing days: 538",
       caption = "", 
       x = "pcs", y = "count")
```

![](delivery_lab_files/figure-gfm/histograms-8.png)<!-- -->

``` r
ggplot(data = ABplus.distr, aes(x = pcs)) + geom_histogram(binwidth = 1) + 
  labs(title = "AB+",
       subtitle = "Missing days: 350",
       caption = "", 
       x = "pcs", y = "count")
```

![](delivery_lab_files/figure-gfm/histograms-9.png)<!-- -->

## First sanity check: does distribution data agree with sales data?

``` r
# Sum deliveries into monthly bins
distr.monthly <- aggregate(pcs ~ month(date) + year(date), data = all.distr, FUN = sum)
data <- data.frame(date = seq(from = as.Date("2014-01-01"), to = as.Date("2018-12-01"), by = "month"),
                   distr = distr.monthly[distr.monthly$`year(date)` >= 2014 & distr.monthly$`year(date)` <= 2018, ]$pcs,
                   sales = sales[sales$date >= "2014-01-01" & sales$date <= "2018-12-01", ]$Punasoluvalmisteet)
```

``` r
# Plot
ggplot() + 
  geom_line(data = data, aes(x = date, y = distr, colour = "distribution"), size = 1) + 
  geom_point(data = data, aes(x = date, y = distr, colour = "distribution")) +
  geom_line(data = data, aes(x = date, y = sales, colour = "sales"), size = 1) + 
  geom_point(data = data, aes(x = date, y = sales, colour = "sales")) +
  scale_color_manual(values = c("#DF013A", "#298A08")) +
  theme(legend.position = "bottom", legend.margin = margin(t = -20, b = 20)) +
  labs(title = "Distribution vs. Sales",
       subtitle = "Distribution largely agrees with sales?",
       caption = "Note: Only red cell products here", 
       x = "", y = "blood bags") 
```

![](delivery_lab_files/figure-gfm/plot-1.png)<!-- -->

## Considering removals

Removals shouldn’t make much of a difference, since removals contribute
to under a percentage of the entirety of the data. To be thorough
though, we should subtract removals from distributions and see if they
then correspond better with the sales
figures.

``` r
retrn.monthly <- aggregate(pcs ~ month(date) + year(date), data = all.retrn, FUN = sum)
distr.new <- distr.monthly[distr.monthly$`year(date)` >= 2014 & distr.monthly$`year(date)` <= 2018, ]$pcs
retrn.new <- retrn.monthly[retrn.monthly$`year(date)` >= 2014 & retrn.monthly$`year(date)` <= 2018, ]$pcs
tot <- distr.new - retrn.new
data.new <- data.frame(date = seq(from = as.Date("2014-01-01"), to = as.Date("2018-12-01"), by = "month"),
                       distr = distr.new,
                       retrn = retrn.new,
                       tot = tot,
                       sales = sales[sales$date >= "2014-01-01" & sales$date <= "2018-12-01", ]$Punasoluvalmisteet)
```

Plots

``` r
ggplot() + 
  geom_line(data = data.new, aes(x = date, y = tot, colour = "distribution"), size = 1) + 
  geom_point(data = data.new, aes(x = date, y = tot, colour = "distribution")) +
  geom_line(data = data.new, aes(x = date, y = sales, colour = "sales"), size = 1) + 
  geom_point(data = data.new, aes(x = date, y = sales, colour = "sales")) +
  scale_color_manual(values = c("#DF013A", "#298A08")) +
  theme(legend.position = "bottom", legend.margin = margin(t = -20, b = 20)) +
  labs(title = "Distribution minus Returns vs. Sales",
       subtitle = "Distribution still largely corresponds to sales",
       caption = "Note: Might be that some returns have been invoiced regardless", 
       x = "", y = "blood bags")
```

![](delivery_lab_files/figure-gfm/removal_adj_plots-1.png)<!-- -->

## Forecasting deliveries

``` r
distr.mts <- msts(all.distr$pcs, start = decimal_date(as.Date("2013-12-05")), seasonal.periods = c(7, 365.25))
distr.mts <- window(distr.mts, start = 2014)
fit <- tbats(distr.mts)
```

``` r
fc <- predict(fit, h = 21)
autoplot(fc, main = "3 week prediction with TBATS", include = 21)
```

![](delivery_lab_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The model clearly seems to have an idea about the weekly pattern. Let’s
run a rolling partition test (a kind of CV) and average out the MAPEs to
see what kind of errors we are talking about here.

``` r
mapes <- c()
for (i in 1:15){ 
  cat("
  -----------------------------
      RUNNING PARTITION ", i)
  
  nTest <- 7*i
  nTrain <- length(distr.mts) - nTest
  train <- window(distr.mts, 
                  start = decimal_date(as.Date("2014-01-01")), 
                  end = c(decimal_date(as.Date("2014-01-01")), nTrain))
  test <- window(distr.mts, 
                 start = c(decimal_date(as.Date("2014-01-01")), (nTrain + 1)), 
                 end = c(decimal_date(as.Date("2014-01-01")), (nTrain + 7)))
  
  fit <- tbats(train)
  fcast <- predict(fit, h = 7)
  
  mapes <- c(mapes, accuracy(fcast, test)[2, ]["MAPE"])
}
```

    ## 
    ##   -----------------------------
    ##       RUNNING PARTITION  1
    ##   -----------------------------
    ##       RUNNING PARTITION  2
    ##   -----------------------------
    ##       RUNNING PARTITION  3
    ##   -----------------------------
    ##       RUNNING PARTITION  4
    ##   -----------------------------
    ##       RUNNING PARTITION  5
    ##   -----------------------------
    ##       RUNNING PARTITION  6
    ##   -----------------------------
    ##       RUNNING PARTITION  7
    ##   -----------------------------
    ##       RUNNING PARTITION  8
    ##   -----------------------------
    ##       RUNNING PARTITION  9
    ##   -----------------------------
    ##       RUNNING PARTITION  10
    ##   -----------------------------
    ##       RUNNING PARTITION  11
    ##   -----------------------------
    ##       RUNNING PARTITION  12
    ##   -----------------------------
    ##       RUNNING PARTITION  13
    ##   -----------------------------
    ##       RUNNING PARTITION  14
    ##   -----------------------------
    ##       RUNNING PARTITION  15

``` r
cat("
    ==========================
    FINISHED
    ===========================
    
    MAPE
    AVG: ", round(mean(mapes), digits = 2),
    "
    SD: ", round(sd(mapes), digits = 2),
    "
    MAX: ", round(max(mapes), digits = 2),
    "
    MIN: ", round(min(mapes), digits = 2))
```

    ## 
    ##     ==========================
    ##     FINISHED
    ##     ===========================
    ##     
    ##     MAPE
    ##     AVG:  115.21 
    ##     SD:  114.18 
    ##     MAX:  349.2 
    ##     MIN:  22.28

Let’s try linear regression with refitted residuals (to deal with
multiseasonality).

``` r
distr.ts <- ts(all.distr$pcs, start = decimal_date(as.Date("2014-01-01")), frequency = 7)
distr.lm <- tslm(distr.ts ~ trend + season)
```

``` r
res.arima <- auto.arima(distr.lm$residuals)
res.arima.fcast <- forecast(res.arima, h = 21)
resf <- as.numeric(res.arima.fcast$mean)

lmfcast <- forecast(distr.lm, h = 21)
lmf <- as.numeric(lmfcast$mean)

fcast <- lmf + resf
```

``` r
fcast <- data.frame(date = seq(from = as.Date("2019-01-01"), to = as.Date("2019-01-21"), by = "day"), 
                    preds = fcast)

ggplot() + geom_line(data = fcast, aes(x = date, y = preds))
```

![](delivery_lab_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

The produced forecast seems be similar with the TBATS forecast. Let’s
run a similar partitioning check.

``` r
lm.mapes <- c()
for (i in 1:15){
  cat("
  -----------------------------
      RUNNING PARTITION ", i)
  
  nTest <- 7*i  
  nTrain <- length(distr.ts) - nTest 
  train <- window(distr.ts, 
                  start = decimal_date(as.Date("2014-01-01")), 
                  end = c(decimal_date(as.Date("2014-01-01")), nTrain))
  test <- window(distr.ts, 
                 start = c(decimal_date(as.Date("2014-01-01")), (nTrain + 1)), 
                 end = c(decimal_date(as.Date("2014-01-01")), (nTrain + 7)))
  
  trainlm <- tslm(train ~ trend + season)
  trainlmf <- forecast(trainlm, h = 7)
 
  residauto <- auto.arima(trainlm$residuals)
  residf <- forecast(residauto, h = 7)
  
  y <- as.numeric(trainlmf$mean)
  x <- as.numeric(residf$mean)
  fcast <- x + y
  
  lm.mapes <- c(lm.mapes, accuracy(fcast, test)[,]["MAPE"])
}
```

    ## 
    ##   -----------------------------
    ##       RUNNING PARTITION  1
    ##   -----------------------------
    ##       RUNNING PARTITION  2
    ##   -----------------------------
    ##       RUNNING PARTITION  3
    ##   -----------------------------
    ##       RUNNING PARTITION  4
    ##   -----------------------------
    ##       RUNNING PARTITION  5
    ##   -----------------------------
    ##       RUNNING PARTITION  6
    ##   -----------------------------
    ##       RUNNING PARTITION  7
    ##   -----------------------------
    ##       RUNNING PARTITION  8
    ##   -----------------------------
    ##       RUNNING PARTITION  9
    ##   -----------------------------
    ##       RUNNING PARTITION  10
    ##   -----------------------------
    ##       RUNNING PARTITION  11
    ##   -----------------------------
    ##       RUNNING PARTITION  12
    ##   -----------------------------
    ##       RUNNING PARTITION  13
    ##   -----------------------------
    ##       RUNNING PARTITION  14
    ##   -----------------------------
    ##       RUNNING PARTITION  15

``` r
cat("
    ==========================
    FINISHED
    ===========================
    
    MAPE
    AVG: ", round(mean(lm.mapes), digits = 2),
    "
    SD: ", round(sd(lm.mapes), digits = 2),
    "
    MAX: ", round(max(lm.mapes), digits = 2),
    "
    MIN: ", round(min(lm.mapes), digits = 2))
```

    ## 
    ##     ==========================
    ##     FINISHED
    ##     ===========================
    ##     
    ##     MAPE
    ##     AVG:  96.67 
    ##     SD:  124.36 
    ##     MAX:  356.35 
    ##     MIN:  11.32

Double linear performs slightly better.

Let’s see how well this is suited for monthly forecasting.

``` r
# Do daily LM forecasts
es <- c()
# create progress bar
pb <- txtProgressBar(min = 730, max = (length(distr.ts) - 1), style= 3)
for(i in seq(from = 730, to = (length(distr.ts) - 1), by = 1)){
  train <- window(distr.mts,
                  start = c(decimal_date(as.Date("2014-01-01"))),
                  end = c(decimal_date(as.Date("2014-01-01")), i))
  test <- distr.ts[i + 1]
  
  trainlm <- tslm(train ~ trend + season)
  trainlmf <- forecast(trainlm, h = 1)
 
  residauto <- auto.arima(trainlm$residuals)
  residf <- forecast(residauto, h = 1)
  
  y <- as.numeric(trainlmf$mean)
  x <- as.numeric(residf$mean)
  
  fcast <- x + y
  
  e <- test - fcast
  es <- c(es, e)
  setTxtProgressBar(pb, i) # update progress bar
}
close(pb) # close progress bar

fwrite(list(es), file = "results/lm_test_errors.txt")
```

``` r
# Sum up LM errors by month
mons <- rep(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), 4)

moving_index <- 1
mon.es <- c()
for(i in mons){
  mon.e <- sum(es[moving_index:(moving_index -1 + i)])
  moving_index <- moving_index + i
  mon.es <- c(mon.es, mon.e)
}
fwrite(list(mon.es), file = "results/lm_test_monthly_errors.txt")
```

Next, TBATS for comparision

``` r
# Do daily TBATS forecasts
tbats_es <- c()
# create progress bar
pb <- txtProgressBar(min = 730, max = (length(distr.mts) - 1), style= 3)
for(i in seq(from = 730, to = (length(distr.mts) - 1), by = 1)){
  train <- window(distr.mts,
                  start = c(decimal_date(as.Date("2014-01-01"))),
                  end = c(decimal_date(as.Date("2014-01-01")), i))
  test <- distr.mts[i + 1]
  fit <- tbats(train)
  fcast <- predict(fit, h = 1)
  
  e <- test - fcast$mean
  tbats_es <- c(tbats_es, e)
  setTxtProgressBar(pb, i) # update progress bar
}
close(pb) # close progress bar

fwrite(list(tbats_es), file = "results/tbats_test_errors.txt")
```

``` r
# Sum up TBATS errors by month
mons <- rep(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), 4)

moving_index <- 1
tbats_mon.es <- c()

for(i in mons){
  mon.e <- sum(es[moving_index:(moving_index -1 + i)])
  moving_index <- moving_index + i
  tbats_mon.es <- c(tbats_mon.es, mon.e)
}
fwrite(list(tbats_mon.es), file = "results/tbats_test_monthly_errors.txt")
```

## TODO

Smoothed series Rolling windows All types All products (platelets and
plasma also)