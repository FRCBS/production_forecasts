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

``` r
# remember that this won't work if there are missing days
# cut to first monday
conct.distr <- tail(all.distr, -15)

pcslist <- list()
weeklist <- list()
datelist <- list()
j <- 0
k <- 0
for(i in seq(to = length(conct.distr$date), by = 7)){
  if(j == 52){j <- 0}
  j <- j + 1
  k <- k + 1
  pcslist[[k]] <- sum(conct.distr$pcs[i : (i + 6)])
  weeklist[[k]] <- j
  datelist[[k]] <- conct.distr$date[i]
}

distr.weekly <- data.frame(week = unlist(weeklist), 
                           startdate = as.Date.numeric(unlist(datelist), origin = "1970-01-01"), 
                           pcs = unlist(pcslist))
```

``` r
ggplot(data = distr.weekly, aes(x = startdate, y = pcs)) + 
  geom_line() + 
  geom_smooth(data = distr.weekly[1:150,], aes(x = startdate, y = pcs), method = "lm", inherit.aes = FALSE) +
  geom_smooth(data = distr.weekly[151:287,], aes(x = startdate, y = pcs), method = "lm", inherit.aes = FALSE) +
  labs(title = "Weekly Distribution",
       subtitle = "Trend turns around 2017 and variance increases",
       x = "", y = "blood bags") +
  theme_bw()
```

![](delivery_lab_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
distr.weekly.mts <- msts(distr.weekly$pcs, start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(4, 12, 52))
distr.weekly.mts <- window(distr.weekly.mts, start = 2017)
fit.weekly <- stlf(distr.weekly.mts)
autoplot(fit.weekly) +
  labs(title = "2 year forecast with data from 2017 onwards",
       subtitle = "Forecast appears much more certain about the shape of the seasons",
       x = "", y = "blood bags") +
  theme_bw()
```

![](delivery_lab_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
weekly.fc <- predict(fit.weekly, h = 104)
autoplot(weekly.fc, main = "3 week prediction with STLF", include = 104)
```

![](delivery_lab_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The model clearly seems to have an idea about the pattern. Let’s run a
rolling partition test (a kind of CV) and average out the MAPEs to see
what kind of errors we are talking about here.

## Weekly forecast

``` r
# Define the series to be used
distr.weekly.mts <- msts(distr.weekly$pcs, start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
distr.weekly.mts <- window(distr.weekly.mts, start = 2017)

es <- c()
apes <- c()
ses <- c()
capes <- c()

# Create progress bar
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts)-105, style = 3)

for(i in seq(length(distr.weekly.mts)-105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(distr.weekly.mts[(1 + i):(104 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
  
  # Fit
  fit <- stlf(train)
  # Forecast 1 step (week) ahead
  fcast <- forecast(fit, h = 1)
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
  ape <- abs(100 * e/test)  # Absolute percentage error
  se <- e * e   # Squared error (for RMSE later on)
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  es <- c(es, e)
  apes <- c(apes, ape)
  ses <- c(ses, se)
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
# Print errors for model selection
cat("

RESULTS
======================================
    Minimum error: ", min(abs(es)),
"
    Maximum error: ", max(es),
"
    MAPE: ", mean(apes),
"
    cMAPE: ", mean(capes),
"
    RMSE: ", sqrt(mean(ses)))
```

    ## 
    ## 
    ## RESULTS
    ## ======================================
    ##     Minimum error:  13.62961 
    ##     Maximum error:  812.1562 
    ##     MAPE:  5.928009 
    ##     cMAPE:  9.029555 
    ##     RMSE:  293.2051

``` r
# Define the series to be used
distr.weekly.mts <- msts(distr.weekly$pcs, start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
distr.weekly.mts <- window(distr.weekly.mts, start = 2017)

############################
# TBATS
############################
capes <- c()
# Create progress bar
cat("\nRUNNING TBATS\n")
```

    ## 
    ## RUNNING TBATS

``` r
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts) - 105, style = 3)
for(i in seq(length(distr.weekly.mts) - 105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(distr.weekly.mts[(1 + i):(104 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
  # Fit
  fit <- tbats(train)
  # Forecast 1 step (week) ahead
  fcast <- forecast(fit, h = 1)
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
tbats.capes <- capes

############################
# STLF
############################
capes <- c()
# Create progress bar
cat("\nRUNNING STLF\n")
```

    ## 
    ## RUNNING STLF

``` r
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts) - 105, style = 3)
for(i in seq(length(distr.weekly.mts) - 105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(distr.weekly.mts[(1 + i):(104 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
  
  # Fit
  fit <- stlf(train)
  # Forecast 1 step (week) ahead
  fcast <- forecast(fit, h = 1)
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
stlf.capes <- capes

############################
# LMx2
############################
capes <- c()
cat("\nRUNNING LMx2\n")
```

    ## 
    ## RUNNING LMx2

``` r
# Create progress bar
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts) - 105, style = 3)
for(i in seq(length(distr.weekly.mts) - 105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(distr.weekly.mts[(1 + i):(104 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
  
  # Fit & Forecast
  fit1 <- tslm(train ~ trend + season)
  fcast1 <- forecast(fit1, h = 1)
 
  fit2 <- auto.arima(fit1$residuals)
  fcast2 <- forecast(fit2, h = 1)
  
  y <- as.numeric(fcast1$mean)
  x <- as.numeric(fcast2$mean)
  fcast <- x + y
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast)  # Raw error
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
lmx2.capes <- capes

############################
# NAIVE
############################
capes <- c()
cat("\nRUNNING NAIVE\n")
```

    ## 
    ## RUNNING NAIVE

``` r
# Create progress bar
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts) - 105, style = 3)
for(i in seq(length(distr.weekly.mts) - 105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(distr.weekly.mts[(1 + i):(104 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))

  # Fit
  fit <- naive(train)
  # Forecast 1 step (week) ahead
  fcast <- forecast(fit, h = 1)
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
naive.capes <- capes

############################
# SNAIVE
############################
capes <- c()
cat("\nRUNNING SNAIVE\n")
```

    ## 
    ## RUNNING SNAIVE

``` r
# Create progress bar
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts) - 105, style = 3)
for(i in seq(length(distr.weekly.mts) - 105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(distr.weekly.mts[(1 + i):(104 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))

  # Fit
  fit <- snaive(train)
  # Forecast 1 step (week) ahead
  fcast <- forecast(fit, h = 1)
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
snaive.capes <- capes

############################
# RWF
############################
capes <- c()
cat("\nRUNNING RWF\n")
```

    ## 
    ## RUNNING RWF

``` r
# Create progress bar
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts) - 105, style = 3)
for(i in seq(length(distr.weekly.mts) - 105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(distr.weekly.mts[(1 + i):(104 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))

  # Fit
  fit <- rwf(train)
  # Forecast 1 step (week) ahead
  fcast <- forecast(fit, h = 1)
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
rwf.capes <- capes

############################
# MEANF
############################
capes <- c()
cat("\nRUNNING MEANF\n")
```

    ## 
    ## RUNNING MEANF

``` r
# Create progress bar
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts) - 105, style = 3)
for(i in seq(length(distr.weekly.mts) - 105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(distr.weekly.mts[(1 + i):(104 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))

  # Fit
  fit <- meanf(train)
  # Forecast 1 step (week) ahead
  fcast <- forecast(fit, h = 1)
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
meanf.capes <- capes

############################
# Smooth .25 TBATS
############################
capes <- c()
cat("\nRUNNING SMOOTH TBATS\n")
```

    ## 
    ## RUNNING SMOOTH TBATS

``` r
# Create progress bar
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts) - 105, style = 3)
for(i in seq(length(distr.weekly.mts) - 105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(itsmr::smooth.fft(distr.weekly.mts[(1 + i):(104 + i)], .25), 
                start = decimal_date(as.Date("2014-01-06")), 
                seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))

  # Fit
  fit <- tbats(train)
  # Forecast 1 step (week) ahead
  fcast <- forecast(fit, h = 1)
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
smthtbats.capes <- capes

############################
# Smooth .25 STLF
############################
capes <- c()
cat("\nRUNNING SMOOTH STLF\n")
```

    ## 
    ## RUNNING SMOOTH STLF

``` r
# Create progress bar
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts) - 105, style = 3)
for(i in seq(length(distr.weekly.mts) - 105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(itsmr::smooth.fft(distr.weekly.mts[(1 + i):(104 + i)], .25), 
                start = decimal_date(as.Date("2014-01-06")), 
                seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))

  # Fit
  fit <- stlf(train)
  # Forecast 1 step (week) ahead
  fcast <- forecast(fit, h = 1)
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
smthstlf.capes <- capes

############################
# Smooth .25 LMx2
############################
capes <- c()
cat("\nRUNNING SMOOTH LMx2\n")
```

    ## 
    ## RUNNING SMOOTH LMx2

``` r
# Create progress bar
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts) - 105, style = 3)
for(i in seq(length(distr.weekly.mts) - 105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(itsmr::smooth.fft(distr.weekly.mts[(1 + i):(104 + i)], .25), 
                start = decimal_date(as.Date("2014-01-06")), 
                seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))

  # Fit & Forecast
  fit1 <- tslm(train ~ trend + season)
  fcast1 <- forecast(fit1, h = 1)
 
  fit2 <- auto.arima(fit1$residuals)
  fcast2 <- forecast(fit2, h = 1)
  
  y <- as.numeric(fcast1$mean)
  x <- as.numeric(fcast2$mean)
  fcast <- x + y
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast)  # Raw error
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
smthlmx2.capes <- capes

############################
# Smooth .25 NAIVE
############################
capes <- c()
cat("\nRUNNING SMOOTH NAIVE\n")
```

    ## 
    ## RUNNING SMOOTH NAIVE

``` r
# Create progress bar
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts) - 105, style = 3)
for(i in seq(length(distr.weekly.mts) - 105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(itsmr::smooth.fft(distr.weekly.mts[(1 + i):(104 + i)], .25), 
                start = decimal_date(as.Date("2014-01-06")), 
                seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))
  
  # Fit
  fit <- naive(train)
  # Forecast 1 step (week) ahead
  fcast <- forecast(fit, h = 1)
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
smthnaive.capes <- capes

############################
# Smooth .25 SNAIVE
############################
capes <- c()
cat("\nRUNNING SMOOTH SNAIVE\n")
```

    ## 
    ## RUNNING SMOOTH SNAIVE

``` r
# Create progress bar
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts) - 105, style = 3)
for(i in seq(length(distr.weekly.mts) - 105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(itsmr::smooth.fft(distr.weekly.mts[(1 + i):(104 + i)], .25), 
                start = decimal_date(as.Date("2014-01-06")), 
                seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))

  # Fit
  fit <- snaive(train)
  # Forecast 1 step (week) ahead
  fcast <- forecast(fit, h = 1)
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
smthsnaive.capes <- capes

############################
# Smooth .25 RWF
############################
capes <- c()
cat("\nRUNNING SMOOTH RWF\n")
```

    ## 
    ## RUNNING SMOOTH RWF

``` r
# Create progress bar
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts) - 105, style = 3)
for(i in seq(length(distr.weekly.mts) - 105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(itsmr::smooth.fft(distr.weekly.mts[(1 + i):(104 + i)], .25), 
                start = decimal_date(as.Date("2014-01-06")), 
                seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))

  # Fit
  fit <- rwf(train)
  # Forecast 1 step (week) ahead
  fcast <- forecast(fit, h = 1)
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
smthrwf.capes <- capes

############################
# Smooth .25 MEANF
############################
capes <- c()
cat("\nRUNNING SMOOTH MEANF\n")
```

    ## 
    ## RUNNING SMOOTH MEANF

``` r
# Create progress bar
pb <- txtProgressBar(min = 1, max = length(distr.weekly.mts) - 105, style = 3)
for(i in seq(length(distr.weekly.mts) - 105)){
  # Define training and testing set as ROLLING WINDOW
  train <- msts(itsmr::smooth.fft(distr.weekly.mts[(1 + i):(104 + i)], .25), 
                start = decimal_date(as.Date("2014-01-06")), 
                seasonal.periods = c(12, 52))
  test <- msts(distr.weekly.mts[(105 + i)], start = decimal_date(as.Date("2014-01-06")), seasonal.periods = c(12, 52))

  # Fit
  fit <- meanf(train)
  # Forecast 1 step (week) ahead
  fcast <- forecast(fit, h = 1)
  
  # Calculate errors
  e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
  cape <- ifelse(test = e > 0, yes = abs(e * 2), no = abs(e))/test * 100  # Critical APE (for cMAPE later on)
  
  # Save errors
  capes <- c(capes, cape)
  
  setTxtProgressBar(pb, i) # Update progress bar
}
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |=================================================================| 100%

``` r
close(pb) # Close progress bar
```

``` r
smthmeanf.capes <- capes
```

``` r
m <- matrix(c(tbats.capes, stlf.capes, lmx2.capes, naive.capes, snaive.capes, rwf.capes,
              meanf.capes, smthtbats.capes, smthstlf.capes, smthlmx2.capes, smthnaive.capes,
              smthsnaive.capes, smthrwf.capes, smthmeanf.capes),
               ncol = 14,
               byrow = TRUE)
colnames(m) <- c("TBATS", "STLF", "LMx2", "NAIVE", "SNAIVE", "RWF", "MEANF",
                    "TBATS .25", "STLF .25", "LMx2 .25", "NAIVE .25", "SNAIVE .25", "RWF .25", "MEANF .25")
kable(m, "markdown")
```

|      TBATS |       STLF |       LMx2 |      NAIVE |     SNAIVE |        RWF |      MEANF |  TBATS .25 |   STLF .25 |   LMx2 .25 |  NAIVE .25 | SNAIVE .25 |   RWF .25 |  MEANF .25 |
| ---------: | ---------: | ---------: | ---------: | ---------: | ---------: | ---------: | ---------: | ---------: | ---------: | ---------: | ---------: | --------: | ---------: |
|  5.0263920 | 28.8944670 | 31.4224934 | 18.3586754 | 15.4177747 | 13.7678280 |  0.2867426 | 21.1164087 | 22.4147399 | 20.2176098 | 13.1139430 |  4.0950253 |  3.871539 |  1.7701760 |
|  3.3928079 | 12.4248166 |  3.5929401 |  2.6292980 |  5.0056505 |  1.6177209 |  8.5224063 |  8.0114163 | 13.1483632 |  8.4445884 |  2.1919067 |  7.7539363 | 37.853935 |  9.8949047 |
| 14.2367379 | 15.6354028 |  2.2709640 |  3.0168975 |  0.3930311 | 27.4233330 | 21.0157178 |  3.3352812 |  1.1975218 |  5.6144448 |  7.3745494 |  0.5037372 |  4.619524 | 11.8912197 |
|  2.2458874 |  4.6665373 |  6.7402686 | 13.5182116 |  7.6738568 |  0.7104306 | 16.6718908 |  9.7193238 |  1.8699675 |  4.6748411 | 10.4316050 |  6.5760044 |  1.746606 | 13.4389963 |
|  0.1321401 |  0.4751472 |  1.1607273 |  1.3996776 | 24.5060768 |  5.0230769 | 15.0024286 | 14.4239902 |  5.5089451 |  1.1065197 |  5.9606851 | 13.9449464 | 18.699368 |  2.1565515 |
|  2.8071045 |  9.3573965 | 21.7350396 |  8.3077725 |  4.0666566 |  1.8060056 |  4.5408713 |  2.6769011 | 40.8296434 | 11.4545455 | 10.0641342 |  2.0644512 |  5.330151 |  0.0796178 |
|  3.2611674 | 15.5207280 | 29.9605782 | 41.8701299 | 10.0000000 | 14.7167814 |  1.9152954 |  1.7567939 |  0.1374382 |  9.6113287 | 16.8827586 |  3.5762666 |  6.399161 |  1.7614091 |
| 12.3875225 | 26.2183998 |  1.0002632 | 12.8304128 |  9.3431484 |  1.0586552 | 12.7243067 | 10.5974026 |  5.5500740 |  4.7834844 |  5.7809600 |  7.9617834 |  4.055906 |  0.8088979 |
| 17.2798949 |  0.4155844 | 11.1142857 | 22.3928004 | 21.7966010 |  1.5646445 |  4.2605827 | 13.2570051 | 35.9724138 | 15.1720401 |  4.3010753 |  3.9231385 | 11.487702 |  8.3398488 |
|  5.3961569 |  3.3264033 |  5.4926387 |  4.9785408 | 40.8296434 | 11.4545455 | 10.0641342 |  2.0644512 |  5.3301512 |  0.0796178 |  3.2611674 | 15.5207280 | 29.960578 | 41.8701299 |
| 10.0000000 | 14.7167814 |  1.9152954 |  1.7567939 |  0.1374382 |  9.6113287 | 16.8827586 |  3.5762666 |  6.3991608 |  1.7614091 | 12.3875225 | 26.2183998 |  1.000263 | 12.8304128 |
|  9.3431484 |  1.0586552 | 29.1997562 |  9.4405594 | 19.0002277 | 15.1938570 |  5.0593599 |  4.8454597 |  0.7509961 | 14.0818231 | 20.8793465 |  9.1873127 |  4.916484 |  5.7610865 |
|  1.9376025 |  0.7701810 |  0.9152852 | 10.7255429 |  1.3127321 |  0.8107038 |  7.2066211 |  3.7994498 | 10.3972475 |  8.6084882 |  6.5528378 |  9.1561765 |  4.057954 |  5.0707054 |
| 33.3141003 |  7.8368722 | 16.2193567 | 11.4303228 |  2.0733690 |  0.7964755 |  2.3603599 | 11.3837038 | 24.1706797 | 11.0374390 |  4.2414054 |  9.2875079 |  2.101062 |  2.3197972 |
|  5.9162187 |  6.8559523 | 11.0580487 |  7.5814573 | 10.8133710 |  1.9995758 | 16.7186982 |  8.7035336 |  4.4900195 |  8.4497694 |  0.0658568 |  7.2013949 | 33.605816 |  7.4908953 |
| 15.3838660 | 10.0952200 |  2.8502607 |  1.4640661 |  2.4000771 | 12.4254284 | 23.5710523 | 12.8234326 |  3.7461398 |  9.0579361 |  0.7212972 |  0.6991521 |  2.365605 |  8.2502827 |
| 10.7294776 | 10.4101422 | 14.6325031 |  6.3119512 | 15.8768402 |  9.1695890 |  3.3705570 |  9.7205974 |  1.8990551 |  4.7849914 | 30.4981348 |  4.2396444 | 12.619378 |  7.8479921 |
|  3.6038236 |  2.4418675 |  3.6452329 |  9.2435632 | 25.7387732 |  9.0191306 |  5.2392017 |  7.3688473 |  0.6613309 |  0.2460295 |  2.9637012 |  8.6980480 |  6.976551 |  4.5461308 |
|  8.0977489 |  0.0994975 | 18.2072111 |  6.8309870 |  3.2487480 |  9.1475680 |  1.2336396 |  4.6483434 | 34.3686437 |  9.3671373 | 16.8063131 | 10.5426170 |  3.266100 |  2.9184223 |
|  4.0996802 |  9.7420595 | 24.4131493 | 12.5757540 |  2.8564970 | 11.4253306 |  3.1466252 |  0.1759011 |  1.5753699 |  9.4441209 |  7.5284711 |  8.2575098 | 14.108146 |  6.9503568 |
| 15.0939949 |  9.3904606 |  3.4064432 | 10.3562611 |  2.8603444 |  2.7474050 | 35.4198290 |  9.3235799 | 11.9135727 |  3.3394774 |  4.0416971 |  3.0011818 |  4.285501 | 11.3832567 |
| 21.0553824 |  8.8872731 |  5.1135463 |  5.8763831 |  4.5754392 |  4.6496479 |  7.7131736 |  4.5146641 |  6.8471205 |  6.0917539 |  6.7008237 |  0.7700755 | 14.340333 |  2.2235147 |
|  4.9060753 |  6.7431471 |  1.9324507 |  2.6900512 | 34.3686437 |  9.3671373 | 16.8063131 | 10.5426170 |  3.2661003 |  2.9184223 |  4.0996802 |  9.7420595 | 24.413149 | 12.5757540 |
|  2.8564970 | 11.4253306 |  3.1466252 |  0.1759011 |  1.5753699 |  9.4441209 |  7.5284711 |  8.2575098 | 14.1081457 |  6.9503568 | 15.0939949 |  9.3904606 |  3.406443 | 10.3562611 |
|  2.8603444 |  2.7474050 | 29.1997562 |  9.4405594 | 19.0002277 | 15.1938570 |  5.0593599 |  4.8454597 |  0.7509961 | 14.0818231 | 20.8793465 |  9.1873127 |  4.916484 |  5.7610865 |
|  1.9376025 |  0.7701810 |  0.9152852 | 10.7255429 |  1.3127321 |  0.8107038 |  7.2066211 |  3.7994498 | 10.3972475 |  8.6084882 |  6.5528378 |  9.1561765 |  4.057954 |  5.0707054 |

## MDLSL END
