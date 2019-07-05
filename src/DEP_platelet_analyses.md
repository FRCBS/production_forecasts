\[DEPRECATED\] Forecasting Lab: Platelet figures
================

# Intro

This notebook follows the general structure of
**DEP\_adjusted\_first\_analyses**, but only focuses on *PLATELETS*.
This notebook contains couple of additions, “deeper” analyses and some
improvement to the methods and code. This is however also deprecated, as
the methods are still largely incorrect and/or insufficient. For
example, the month adjustment is now better, but still not ideal.

``` r
library(forecast)
library(ggplot2)
library(gridExtra)
library(knitr)
```

## Create original dataset that should remain immutable throughout labbing

``` r
# Load data
monthly_sales <- read.table("/home/esa/production_forecasts/data/kuukausimyynti.txt", header = T, sep = "\t")

# Separate yyyy/mmm column into months and years
monthly_sales$year <- substr(monthly_sales$kuukausi, 1, 4)
monthly_sales$month <- factor(substr(monthly_sales$kuukausi, 6, 8), levels=c("tam", "hel", "maa", "huh", "tou", "kes", "hei", "elo", "syy", "lok", "mar", "jou"))

# Create a numeric column for months
monthly_sales$month_num <- as.numeric(monthly_sales$month)

# Omit empty values
d <- na.omit(monthly_sales)
```

## Create the time series object

``` r
# Platelets
ts.pla <- ts(d$Trombosyyttivalmisteet, 
             start=as.numeric(c(d$year[1], d$month_num[1])), 
             end=as.numeric(c(tail(d$year, 1), tail(d$month_num, 1))), 
             frequency=12)

# Month adjustment
ts.pla <- ts.pla/bizdays(ts.pla, FinCenter = "Zurich")  # Zurich trading days resemble ours
```

``` r
ggseasonplot(ts.pla, year.labels = TRUE, year.labels.left = TRUE) + ggtitle("Adjusted platelet yearly seasonality")
```

![](DEP_platelet_analyses_files/figure-gfm/seasonplot-1.png)<!-- -->

Seems to me that July and December are low points in sales very
consistently, while lots of sales happen in October and November.

## Old forecasts now with better decomp

Old forecasts are built using **STL+ETS** and **ETS** models, including
also a naive 6 month repetition forecast. I will not be considering the
naive “forecast” here, as I’m fairly certain we want to do proper
modelling.

``` r
# Seasonal and Trend decomposition by LOESS + ETS
# The t.window of stl() should be an odd number, but someone has decided against it here. Will investigate.
stl.pla <- forecast(stl(ts.pla, s.window="periodic", t.window=6), h=12)


# Exponential smoothing state space model
# ets() is an automated model selection function, so these are not the same model! Uses AICc, AIC and BIC.
ets.pla <- forecast(ets(ts.pla), h=12)


# Plot
grid.arrange(grobs=list(autoplot(stl.pla),
                        autoplot(ets.pla)),
             layout_matrix=rbind(c(1),
                                 c(2)))
```

![](DEP_platelet_analyses_files/figure-gfm/old_forecasts-1.png)<!-- -->

ETS models are based on weighted averages of past observations, with
*exponentially* decaying weights as the observations move further back
in time. The ETS function used in Tuimala’s script optimize the
smoothing parameters and initial values required by minimising the sum
of the squared errors (SSE). They can be simple explonential smoothing
models (SES), or state space models consisting of Error, Trend and
Seasonal parts. The \(ets()\) function used here tries to automatically
select the best model from the ets model family using information
criteria measures (AIC, AICc and BIC).

The STL+ETS models use a “Seasonal and Trend decomposition by LOESS”
method to extract the seasonal component of the series and feed the
seasonally adjusted series to the \(ets()\) function.

## Model comparisons

``` r
monthly <- ggplot() 
forecast_errors <- c()
stl_RMSE <- c()
stl_MAPE <- c()
# Loop 
for(i in seq(from=48, to=180, by=1)){
  fit <- stl(head(ts.pla, i), s.window="periodic", t.window=6)  # Fit based on history so far
  fcast <- forecast(fit, h=1)  # Forecast the next month
  segment <- ts.pla[i+1]  # Extract that year from the history for plotting purposes
  
  # Build the plot piece by piece
  monthly <- monthly + autolayer(fcast)
  
  # Calculate raw forecast errors
  forecast_errors <- c(forecast_errors, abs(data.frame(fcast)$Point.Forecast - segment))
  
  stl_RMSE <- c(stl_RMSE, data.frame(accuracy(fcast, segment))$RMSE)
  stl_MAPE <- c(stl_MAPE, data.frame(accuracy(fcast, segment))$MAPE)
}

monthly + ggtitle("STL+ETS forecast of platelet sales month by month") +
  scale_x_discrete(limits=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + xlab("Time") +
  ylab("Unit sales") + autolayer(window(ts.pla, start=2008), colour=FALSE)
```

![](DEP_platelet_analyses_files/figure-gfm/stl_monthly_forecasts-1.png)<!-- -->

``` r
autoplot(ts(forecast_errors, start=2008, end=2018, frequency=12)) + ggtitle("STL+ETS historical forecast errors for platelets") + ylab("Unit sales")
```

![](DEP_platelet_analyses_files/figure-gfm/stl_historical_errors-1.png)<!-- -->

``` r
monthly_ets <- ggplot() 
ets_forecast_errors <- c()
ets_RMSE <- c()
ets_MAPE <- c()
# Loop 
for(i in seq(from=48, to=180, by=1)){
  fit <- ets(head(ts.pla, i))  # Fit based on history so far
  fcast <- forecast(fit, h=1)  # Forecast the next month
  segment <- ts.pla[i+1]  # Extract that month from the history for error calculation
  
  # Build the plot piece by piece
  monthly_ets <- monthly_ets + autolayer(fcast)
  
  # Calculate forecast errors
  ets_forecast_errors <- c(ets_forecast_errors, abs(data.frame(fcast)$Point.Forecast - segment))
  
  ets_RMSE <- c(ets_RMSE, data.frame(accuracy(fcast, segment))$RMSE)
  ets_MAPE <- c(ets_MAPE, data.frame(accuracy(fcast, segment))$MAPE)
}

monthly_ets + ggtitle("ETS forecast of platelet sales year by year") +
  scale_x_discrete(limits=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + xlab("Time") +
  ylab("Unit sales") + autolayer(window(ts.pla, start=2008), colour=FALSE)
```

![](DEP_platelet_analyses_files/figure-gfm/ets_monthly_forecasts-1.png)<!-- -->

``` r
autoplot(ts(ets_forecast_errors, start=2008, end=2018, frequency=12)) + ggtitle("ETS historical forecast errors for platelets") + ylab("Unit sales")
```

![](DEP_platelet_analyses_files/figure-gfm/ets_historical_errors-1.png)<!-- -->

## How far back do the models actually look?

``` r
##################################
# 5 YEARS
##################################
monthly_1yr <- ggplot()  # Don't mind the silly legacy variable name
forecast_errors_1yr <- c()
stl_RMSE_1yr <- c()
stl_MAPE_1yr <- c()
# Loop 
for(i in seq(from=48, to=180, by=1)){
  fit <- stl(tail(head(ts.pla, i), 60), s.window="periodic", t.window=6)  # Fit based on 5yr history
  fcast <- forecast(fit, h=1)  # Forecast the next month
  segment <- ts.pla[i+1]  # Extract that month from the history for error calculation
  
  # Build the plot piece by piece
  monthly_1yr <- monthly_1yr + autolayer(fcast)
  
  # Calculate raw forecast errors
  forecast_errors_1yr <- c(forecast_errors_1yr, abs(data.frame(fcast)$Point.Forecast - segment))
  
  stl_RMSE_1yr <- c(stl_RMSE_1yr, data.frame(accuracy(fcast, segment))$RMSE)
  stl_MAPE_1yr <- c(stl_MAPE_1yr, data.frame(accuracy(fcast, segment))$MAPE)
}

monthly_1yr + ggtitle("STL+ETS forecast of platelet sales month by month, 5 years of data") +
  scale_x_discrete(limits=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + xlab("Time") +
  ylab("Unit sales") + autolayer(window(ts.pla, start=2008), colour=FALSE)
```

![](DEP_platelet_analyses_files/figure-gfm/stl_5_rolling-1.png)<!-- -->

``` r
##################################
# 3 YEARS
##################################
monthly_3yr <- ggplot() 
forecast_errors_3yr <- c()
stl_RMSE_3yr <- c()
stl_MAPE_3yr <- c()
# Loop 
for(i in seq(from=48, to=180, by=1)){
  fit <- stl(tail(head(ts.pla, i), 36), s.window="periodic", t.window=6)  # Fit based on 3yr history
  fcast <- forecast(fit, h=1)  # Forecast the next month
  segment <- ts.pla[i+1]  # Extract that month from the history for error calculation
  
  # Build the plot piece by piece
  monthly_3yr <- monthly_3yr + autolayer(fcast)
  
  # Calculate raw forecast errors
  forecast_errors_3yr <- c(forecast_errors_3yr, abs(data.frame(fcast)$Point.Forecast - segment))
  
  stl_RMSE_3yr <- c(stl_RMSE_3yr, data.frame(accuracy(fcast, segment))$RMSE)
  stl_MAPE_3yr <- c(stl_MAPE_3yr, data.frame(accuracy(fcast, segment))$MAPE)
}

monthly_3yr + ggtitle("STL+ETS forecast of platelet sales month by month, 3 years of data") +
  scale_x_discrete(limits=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + xlab("Time") +
  ylab("Unit sales") + autolayer(window(ts.pla, start=2008), colour=FALSE)
```

![](DEP_platelet_analyses_files/figure-gfm/stl_3_rolling-1.png)<!-- -->

``` r
##################################
# 5 YEARS
##################################

monthly_ets_1yr <- ggplot()  # Silly legacy variable names
ets_forecast_errors_1yr <- c()
ets_RMSE_1yr <- c()
ets_MAPE_1yr <- c()
# Loop 
for(i in seq(from=48, to=180, by=1)){
  fit <- ets(tail(head(ts.pla, i), 60))  # Fit based on 5yr history
  fcast <- forecast(fit, h=1)  # Forecast the next month
  segment <- ts.pla[i+1]  # Extract that month from the history for error calculation
  
  # Build the plot piece by piece
  monthly_ets_1yr <- monthly_ets_1yr + autolayer(fcast)
  
  # Calculate forecast errors
  ets_forecast_errors_1yr <- c(ets_forecast_errors_1yr, abs(data.frame(fcast)$Point.Forecast - segment))
  
  ets_RMSE_1yr <- c(ets_RMSE_1yr, data.frame(accuracy(fcast, segment))$RMSE)
  ets_MAPE_1yr <- c(ets_MAPE_1yr, data.frame(accuracy(fcast, segment))$MAPE)
}

monthly_ets_1yr + ggtitle("ETS forecast of platelet sales month by month, 5 years of data") +
  scale_x_discrete(limits=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + xlab("Time") +
  ylab("Unit sales") + autolayer(window(ts.pla, start=2008), colour=FALSE)
```

![](DEP_platelet_analyses_files/figure-gfm/ets_5_rolling-1.png)<!-- -->

``` r
##################################
# 3 YEARS
##################################

monthly_ets_3yr <- ggplot() 
ets_forecast_errors_3yr <- c()
ets_RMSE_3yr <- c()
ets_MAPE_3yr <- c()
# Loop 
for(i in seq(from=48, to=180, by=1)){
  fit <- ets(tail(head(ts.pla, i), 36))  # Fit based on 3yr history
  fcast <- forecast(fit, h=1)  # Forecast the next month
  segment <- ts.pla[i+1]  # Extract that month from the history for error calculation
  
  # Build the plot piece by piece
  monthly_ets_3yr <- monthly_ets_3yr + autolayer(fcast)
  
  # Calculate forecast errors
  ets_forecast_errors_3yr <- c(ets_forecast_errors_3yr, abs(data.frame(fcast)$Point.Forecast - segment))
  
  ets_RMSE_3yr <- c(ets_RMSE_3yr, data.frame(accuracy(fcast, segment))$RMSE)
  ets_MAPE_3yr <- c(ets_MAPE_3yr, data.frame(accuracy(fcast, segment))$MAPE)
}

monthly_ets_3yr + ggtitle("ETS forecast of platelet sales month by month, 3 years of data") +
  scale_x_discrete(limits=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + xlab("Time") +
  ylab("Unit sales") + autolayer(window(ts.pla, start=2008), colour=FALSE)
```

![](DEP_platelet_analyses_files/figure-gfm/ets_3_rolling-1.png)<!-- -->

## Forecasting from 2012 onwards

``` r
monthly_2012 <- ggplot() 
forecast_errors_2012 <- c()
stl_RMSE_2012 <- c()
stl_MAPE_2012 <- c()
# Loop 
for(i in seq(from=96, to=180, by=1)){
  fit <- stl(head(ts.pla, i), s.window="periodic", t.window=6)  # Fit based on history from 2012
  fcast <- forecast(fit, h=1)  # Forecast the next month
  segment <- ts.pla[i+1]  # Extract that year from the history for plotting purposes
  
  # Build the plot piece by piece
  monthly_2012 <- monthly_2012 + autolayer(fcast)
  
  # Calculate raw forecast errors
  forecast_errors_2012 <- c(forecast_errors_2012, abs(data.frame(fcast)$Point.Forecast - segment))
  
  stl_RMSE_2012 <- c(stl_RMSE_2012, data.frame(accuracy(fcast, segment))$RMSE)
  stl_MAPE_2012 <- c(stl_MAPE_2012, data.frame(accuracy(fcast, segment))$MAPE)
}

monthly_2012 + ggtitle("STL+ETS forecast of platelet sales month by month from 2012") +
  scale_x_discrete(limits=c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + xlab("Time") +
  ylab("Unit sales") + autolayer(window(ts.pla, start=2011), colour=FALSE)
```

![](DEP_platelet_analyses_files/figure-gfm/stl_2012-1.png)<!-- -->

## Playing around with a NN

``` r
monthly_NN <- ggplot() 
NN_forecast_errors <- c()
NN_RMSE <- c()
NN_MAPE <- c()
# Loop 
for(i in seq(from=48, to=180, by=1)){
  fit <- nnetar(head(ts.pla, i), PI=TRUE, lambda=0)  # Fit based on history
  fcast <- forecast(fit, h=1)  # Forecast the next month
  segment <- ts.pla[i+1]  # Extract that month from the history for error calculation
  
  # Build the plot piece by piece
  monthly_NN <- monthly_NN + autolayer(fcast)
  
  # Calculate forecast errors
  NN_forecast_errors <- c(NN_forecast_errors, abs(data.frame(fcast)$Point.Forecast - segment))
  
  NN_RMSE <- c(NN_RMSE, data.frame(accuracy(fcast, segment))$RMSE)
  NN_MAPE <- c(NN_MAPE, data.frame(accuracy(fcast, segment))$MAPE)
}

monthly_NN + ggtitle("NN forecast of platelet sales month by month") +
  scale_x_discrete(limits=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + xlab("Time") +
  ylab("Unit sales") + autolayer(window(ts.pla, start=2008), colour=FALSE)
```

![](DEP_platelet_analyses_files/figure-gfm/NN-1.png)<!-- -->

Find out which performed better on average:

``` r
results <- matrix(c(mean(stl_RMSE), mean(stl_MAPE), 
                    mean(ets_RMSE), mean(ets_MAPE),
                    mean(NN_RMSE), mean(NN_MAPE),
                    mean(stl_RMSE_3yr), mean(stl_MAPE_3yr),
                    mean(ets_RMSE_3yr), mean(ets_MAPE_3yr),
                    mean(stl_RMSE_1yr), mean(stl_MAPE_1yr),
                    mean(ets_RMSE_1yr), mean(ets_MAPE_1yr),
                    mean(stl_RMSE_2012), mean(stl_MAPE_2012)), 
                  ncol=2, 
                  byrow=TRUE)

colnames(results) <- c("RMSE", "MAPE")
rownames(results) <- c("STL+ETS whole", 
                       "ETS whole",
                       "NN whole",
                       "STL+ETS 3yr",
                       "ETS 3yr",
                       "STL+ETS 5yr",
                       "ETS 5yr",
                       "STL+ETS 2012")

results <- kable(results, "markdown")
results
```

|               |      RMSE |     MAPE |
| :------------ | --------: | -------: |
| STL+ETS whole |  8.686711 | 5.184515 |
| ETS whole     |  8.885855 | 5.315564 |
| NN whole      |  8.881461 | 5.303951 |
| STL+ETS 3yr   |  8.628638 | 5.186041 |
| ETS 3yr       | 10.462268 | 6.029447 |
| STL+ETS 5yr   |  8.823558 | 5.256420 |
| ETS 5yr       |  9.385459 | 5.534380 |
| STL+ETS 2012  |  8.543512 | 5.118540 |
