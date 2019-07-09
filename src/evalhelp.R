
# Helper functions for model evaluation
# For the purposes of FRCBS Production Forecasting.
# Author: Esa Turkulainen
# Genesis: 25.6.2019
# Updated: 8.7.2019


# Time series imputation function
make_whole <- function(df, interpolate = TRUE){
  # If the user chooses to interpolate, a ts object will be returned.
  # Setting interpolate = FALSE will return a dataframe with NAs
  # The function also returns a df of all NA dates. It will be packed into a list
  # with the entire series.
  
  # Create a full sequence of dates
  all.dates <- (seq.Date(min(df$time),
                        max(df$time),
                        "day"))
  # Merge into a whole set with NAs
  whole <- merge(x = data.frame(time = all.dates),
                 y = df,
                 all.x = TRUE)
  # Separate days with NA (because we want to mark 'em on the plot)
  missing <- whole[is.na(whole[, 2]), ]$time
  
  # Interpolate if true
  if(interpolate){
    return(list(data.frame(time = whole[, 1], pcs = zoo::na.approx(ts(whole[, 2], start = 2014, frequency = 365))), 
                missing))
  }
  else{
    return(list(whole, missing))
  }
}

# Critical MAPE for critical model estimation
cMAPE <- function(forecast_errors, actual_values){
  # Using this in model eval is more appropriate in the context of blood product forecasts,
  # as we want to penalize forecasting too low much harsher than forecasting too high. 
  # Blood supply should never run out.
  
  pe <- 100*forecast_errors/actual_values  # Percentage error vector
  error <- mean(ifelse(test = pe < 0, yes = abs(pe * 2), no = pe), na.rm = TRUE)  # cMAPE (Negative errors are multiplied by 2)
  return(error)
}

# ETS for tsCV
fets <- function(x, h, model = "ZZZ", damped = NULL, ...) {
  forecast(ets(x, model = model, damped = damped), h = h)
}

# STL for tsCV
fstl <- function(x, h,  s.window = "periodic", t.window = 7){
  forecast(stl(x, s.window = s.window, t.window = t.window), h = h)
}

# auto.arima for tsCV
farima <- function(x, h, xreg = NULL){
  forecast(auto.arima(x, xreg = xreg), h = h)
}

# Moving Average for tsCV
fMA <- function(x, h, order = 5){
  forecast(ma(x, order = order), h = h)
}

# TBATS for tsCV
fTBATS <- function(x, h){
  forecast(tbats(x), h = h)
}

# NN for tsCV
fnnet <- function(x, h){
  forecast(nnetar(x), h = h)
}

# Linear regression for tsCV
freg <- function(x, h){
  forecast(tslm(x ~ trend + season), h = h)
}