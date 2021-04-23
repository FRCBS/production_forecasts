# Final results

# Packages
library(tidyverse)
library(viridis)
library(lubridate)
library(forecast)
library(miscTools)

##################################################################################################################

# Load data
##################################################################################
real <- read_csv("/home/esa/production_forecasts/data/red_full.csv", col_types = "Dd")
forecasts <- read_csv("/home/esa/production_forecasts/histories/long_fcast_history_FI.csv", col_types = "Dfd____")

# Start real at first forecast and omit last (and likely not full) month from both
startdate <- forecasts$time[1]
real_cut <- head(subset(real, date >= startdate), -1)
fcast_cut <- head(forecasts, -1)

# Create dataframe for plotting
plot_df <- data.frame(date = real_cut$date,
                      real = real_cut$pcs,
                      fcast = fcast_cut$forecast,
                      method = fcast_cut$model) %>%
  mutate(method = fct_relevel(method, "12-MA", "9-MA", "5-MA", "DYNREG", "SNAIVE", "STL",
                              "ETS", "ARIMAX", "COMBINED", "STLF", "TBATS", "NN"))
##################################################################################################################

# Plotting
##################################################################################
# Create rects
rects <- data.frame(xstart = seq.Date(plot_df$date[1], plot_df$date[nrow(plot_df) - 1], by = "month"),
                    xend = seq.Date(plot_df$date[2], plot_df$date[nrow(plot_df)], by = "month"),
                    method = head(plot_df$method, -1))

# Plot
p2 <- ggplot() +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = method), alpha = 0.9) +
  geom_line(data = plot_df, aes(x = date, y = real), size = 1.2) +
  labs(title = paste0("Blood units sold and best performing forecasting method monthly between ", year(plot_df$date[1]), " and ", year(plot_df$date[nrow(plot_df)])),
       subtitle = "Data from Finnish Red Cross Blood Service in Finland",
       x = "Date",
       y = "Blood units sold",
       fill = "Method") +
  theme_minimal()
p2
##################################################################################################################

# Create expanding window forecasts for all of our methods
##################################################################################
# Custom forecast function that saves to files
create_fcast <- function(series, start_date, end_date, method){
  
  # Df for saving
  df <- data.frame(date = seq.Date((start_date + months(36)), end_date, by = "month"))
  
  # Vec for collecting fcasts
  f <- c()
  
  # We start training with 3 years of data (36 mo) but then expand it with every iteration.
  # But we also want to start the forecasts where the long_history begins.
  for(i in 1:(length(series) - 36)){
    train <- ts(series[1:(35 + i)], start = c(year(start_date), month(start_date), day(start_date)), frequency = 12)
    
    # xregs for arimax
    nrow <- length(train)
    onehotyear <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                         ncol = 11,
                         byrow = TRUE)
    
    month.m <- matrix(, nrow = nrow, ncol = 11)
    for(j in 1:nrow){
      month.m[j,] <- onehotyear[month(start_date + months(j - 1)), ]
    }
    horizon.m <- matrix(, nrow = 6, ncol = 11)
    k <- 0
    for(j in nrow:(nrow + 5)){
      k <- k + 1
      horizon.m[k,] <- onehotyear[month(start_date + months(j)), ]
    }
    
    # Fit model
    if(method == "snaive"){fit <- snaive(train)}
    if(method == "ma5"){fit <- ma(train, order = 5)}
    if(method == "ma7"){fit <- ma(train, order = 7)}
    if(method == "ma9"){fit <- ma(train, order = 9)}
    if(method == "ma12"){fit <- ma(train, order = 12)}
    if(method == "stl"){fit <- stl(train, s.window = "periodic", t.window = 7)}
    if(method == "ets"){fit <- ets(train)}
    if(method == "tbats"){fit <- tbats(train)}
    if(method == "stlf"){fit <- stlf(train)}
    if(method == "arimax"){
      fit <- auto.arima(train, xreg = month.m)
      forecast <- forecast(fit, xreg = horizon.m, h = 1)$mean
    }
    if(method == "dynreg"){
      fit1 <- tslm(train ~ trend + season)
      fcast1 <- forecast(fit1, h = 1)
      
      fit2 <- auto.arima(fit1$residuals)
      fcast2 <- forecast(fit2, h = 1)
      
      y <- as.numeric(fcast1$mean)
      x <- as.numeric(fcast2$mean)
      forecast <- (x + y)
    }
    if(method == "nn"){
      fit <- nnetar(train)
      forecast <- forecast(fit, h = 1)$mean
    }
    if(method == "combined"){
      # Seasonal naive
      snaive.fit <- snaive(train)
      snaive.fcast <- forecast(snaive.fit, h = 1)$mean
      
      # 5-MA
      ma5.fit <- ma(train, order = 5)
      ma5.fcast <- forecast(ma5.fit, h = 1)$mean
      
      # 7-MA
      ma7.fit <- ma(train, order = 7)
      ma7.fcast <- forecast(ma7.fit, h = 1)$mean
      
      # 9-MA
      ma9.fit <- ma(train, order = 9)
      ma9.fcast <- forecast(ma9.fit, h = 1)$mean
      
      # 12-MA
      ma12.fit <- ma(train, order = 12)
      ma12.fcast <- forecast(ma12.fit, h = 1)$mean
      
      # STL
      stl.fit <- stl(train, s.window = "periodic", t.window = 7)
      stl.fcast <- forecast(stl.fit, h = 1)$mean
      
      # ETS
      ets.fit <- ets(train)
      ets.fcast <- forecast(ets.fit, h = 1)$mean
      
      # TBATS
      tbats.fit <- tbats(train)
      tbats.fcast <- forecast(tbats.fit, h = 1)$mean
      
      # STLF
      stlf.fit <- stlf(train)
      stlf.fcast <- forecast(stlf.fit, h = 1)$mean
      
      # Arimax
      arimax.fit <- auto.arima(train, xreg = month.m)
      arimax.fcast <- forecast(arimax.fit, xreg = horizon.m, h = 1)$mean
      
      # Dynamic regression
      fit1 <- tslm(train ~ trend + season)
      fcast1 <- forecast(fit1, h = 1)
      
      fit2 <- auto.arima(fit1$residuals)
      fcast2 <- forecast(fit2, h = 1)
      
      y <- as.numeric(fcast1$mean)
      x <- as.numeric(fcast2$mean)
      dynreg.fcast <- x + y
      
      # NNETAR
      nn.fit <- nnetar(train)
      nn.fcast <- forecast(nn.fit, h = 1)$mean
      
      forecast <- mean(snaive.fcast, ma5.fcast, ma7.fcast, ma9.fcast, ma12.fcast, stl.fcast,
                       ets.fcast, tbats.fcast, stlf.fcast, arimax.fcast, dynreg.fcast, nn.fcast)
    }
    if(!(method %in% c("dynreg", "nn", "arimax", "combined"))){
      # If model was not any of the above, forecast here
      fcast <- forecast(fit, h = 1)
      forecast <- as.numeric(fcast$mean)
    }
    # Reverse the adjustment done to the series (outside this function)
    # To save the actual forecast without adjustment
    date_of_forecast <- start_date + months(35 + i)
    reverse_adj <- as.numeric(bizdays(ts(seq(1), 
                                         start = c(year(date_of_forecast), month(date_of_forecast), day(date_of_forecast)), 
                                         frequency = 12), 
                                     FinCenter = "Zurich"))
    f[i] <- forecast * reverse_adj
  } # for loop ends here
  
  # Add to df
  fdf <- cbind(df, f)
  # Save to file
  write_csv(fdf, paste0("/home/esa/production_forecasts/histories/single_model_expanding_histories/", method, ".csv"))
}

# Prepare adjusted series and dates for custom function
series <- tail(real$pcs, -11) # We'll cut 11 months from the beginning of the series to make it match long_history fcasts
start_date <- real$date[12]

# Adjustment series
adj <- as.numeric(bizdays(ts(series, 
                             start = c(year(start_date), month(start_date), day(start_date)), #  c(year, month, day) is apparently the only robust way to do this.
                             frequency = 12), 
                          FinCenter = "Zurich"))

adj_series <- series / adj

end_date <- real$date[nrow(real)]
methods <- c("ma5", "ma7", "ma9", "ma12", "snaive", "stl", "ets", 
             "dynreg", "stlf", "tbats", "arimax", "nn", "combined")

# Run for all methods
for(m in 1:length(methods)){
  method <- methods[m]
  create_fcast(series = adj_series, start_date = start_date, end_date = end_date, method = method)
}
##################################################################################################################

# Error comparison
##################################################################################
# Load files we created just now
historydf <- data.frame(date = seq.Date(from = as.Date("2007-12-01"), to = as.Date("2021-03-01"), by = "month"))
for(m in 1:length(methods)){
  method <- methods[m]
  history <- read_csv(paste0("/home/esa/production_forecasts/histories/single_model_expanding_histories/", method, ".csv"))
  historydf <- cbind(historydf, history[, 2])
}
colnames(historydf) <- c("date", "ma5", "ma7", "ma9", "ma12", "snaive", "stl", "ets", 
                         "dynreg", "stlf", "tbats", "arimax", "nn", "combined")

# Remove last row to omit incomplete month from error comparison
historydf <- head(historydf, -1)

long_e <- abs(plot_df$real - plot_df$fcast) / plot_df$real

errdf <- abs(historydf[, 2:14] - plot_df$real) / plot_df$real
mean_errs <- colMeans(errdf)
median_errs <- colMedians(errdf)

# Save to one error file
savethis <- cbind(errdf, long_e)
write_csv(savethis, "/home/esa/production_forecasts/histories/all_errors.csv")
