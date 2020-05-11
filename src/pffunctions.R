# Helper functions for forecast_script.Rmd

extract_type <- function(red.distr, type){
  # Separate blood types from the master frame.
  # 
  # param:: red.distr: The master frame. Distribution of red cell products.
  # param:: type: 3-length string of the desired blood type, e.g. "AB+", "O -".
  # return:: Dataframe of the desired blood type.
  
  # Create a full sequence of dates for imputation purposes
  all.dates <- (seq.Date(min(red.distr$date),
                         max(red.distr$date),
                         "day"))
  # Find type
  typed <- red.distr[red.distr$ABO == type, ]
  typed.distr <- aggregate(typed$quantity, by = list(typed$date), sum); colnames(typed.distr) <- c("date", "pcs")
  # Merge into a whole set with NAs
  typed.distr <- merge(x = data.frame(date = all.dates),
                       y = typed.distr,
                       all.x = TRUE)
  # Replace with zeroes
  typed.distr[is.na(typed.distr)] <- 0
  # Cut to time after 2014
  typed.distr <- typed.distr[typed.distr$date >= as.Date("2014-01-06"), ]
  
  return(typed.distr)
}

aggregate_weekly <- function(series){
  # Aggregate distributions into weekly sums.
  # 
  # param:: series: dataframe of a daily series
  # return:: dataframe of a weekly series
  
  pcslist <- list(); weeklist <- list(); datelist <- list()  # Create storage lists
  j <- 0; k <- 0  # j is for resetting week counter, k is for gathering dates
  for(i in seq(to = length(series$date), by = 7)){
    if(j == 52){j <- 0}
    j <- j + 1
    k <- k + 1
    pcslist[[k]] <- sum(series$pcs[i : (i + 6)])
    weeklist[[k]] <- j
    datelist[[k]] <- series$date[i]
  }
  weekly <- data.frame(week = unlist(weeklist), startdate = as.Date.numeric(unlist(datelist), origin = "1970-01-01"), pcs = unlist(pcslist))
  return(weekly)
}

find_errors <- function(beginning, series.ts, method = "none", smooth = "none", freq = "monthly"){
  # Used in model selection
  # 
  # param:: beginning: Beginning of the series
  # param:: series.ts: timeseries object
  # param:: method: str of "tslm", "stl", "ets", "tbats", "stlf", "naive", "snaive", "rwf", or "meanf"
  # param:: smooth: str of "none", ".25", or ".10"
  # param:: freq: str of "monthly" or "weekly"
  # return:: A vector of errors.
  
  apes <- c()
  if(freq == "monthly"){
    for(i in seq(length(series.ts) - 37)){
      # Define training and testing set as ROLLING WINDOW
      if(smooth == ".25"){
        train <- ts(itsmr::smooth.fft(series.ts[(1 + i):(36 + i)], .25), 
                    start = decimal_date(beginning + i), 
                    frequency = 12)
        test <- ts(series.ts[(37 + i)], start = decimal_date(beginning + 37 + i), frequency = 12)
      }
      if(smooth == ".10"){
        train <- ts(itsmr::smooth.fft(series.ts[(1 + i):(36 + i)], .10), 
                    start = decimal_date(beginning + i), 
                    frequency = 12)
        test <- ts(series.ts[(37 + i)], start = decimal_date(beginning + 37 + i), frequency = 12)
      }
      if(smooth == "none"){
        train <- ts(series.ts[(1 + i):(36 + i)], start = decimal_date(beginning + i), frequency = 12)
        test <- ts(series.ts[(37 + i)], start = decimal_date(beginning + 37 + i), frequency = 12)
      }
      
      if(!(method == "tslm")){
        if(method == "stl"){
          # Fit
          fit <- stl(train, s.window = "periodic", t.window = 7)
        }
        if(method == "ets"){
          fit <- ets(train)
        }
        if(method == "tbats"){
          fit <- tbats(train)
        }
        if(method == "stlf"){
          fit <- stlf(train)
        }
        if(method == "naive"){
          fit <- naive(train)
        }
        if(method == "snaive"){
          fit <- snaive(train)
        }
        if(method == "rwf"){
          fit <- rwf(train, drift = TRUE)
        }
        if(method == "meanf"){
          fit <- meanf(train)
        }
        # Forecast 1 step (week) ahead
        fcast <- forecast(fit, h = 1)
        # Calculate errors
        e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
      }
      else{
        # Fit & forecast
        fit1 <- tslm(train ~ trend + season)
        fcast1 <- forecast(fit1, h = 1)
        
        fit2 <- auto.arima(fit1$residuals)
        fcast2 <- forecast(fit2, h = 1)
        
        y <- as.numeric(fcast1$mean)
        x <- as.numeric(fcast2$mean)
        fcast <- x + y
        
        # Calculate errors
        e <- as.numeric(test) - as.numeric(fcast)  # Raw error
      }
      
      ape <- abs(e)/test * 100  # We decided to change this to NON-critical
      
      # Save errors
      apes <- c(apes, ape)
    }
  } 
    if(freq == "weekly"){
      for(i in seq(length(series.ts) - 157)){
        # Define training and testing set as ROLLING WINDOW
        if(smooth == ".25"){
          train <- ts(itsmr::smooth.fft(series.ts[(1 + i):(156 + i)], .25), 
                      start = decimal_date(beginning + i), 
                      frequency = 12)
          test <- ts(series.ts[(157 + i)], start = decimal_date(beginning + 157 + i), frequency = 12)
        }
        if(smooth == ".10"){
          train <- ts(itsmr::smooth.fft(series.ts[(1 + i):(156 + i)], .10), 
                      start = decimal_date(beginning + i), 
                      frequency = 12)
          test <- ts(series.ts[(157 + i)], start = decimal_date(beginning + 157 + i), frequency = 12)
        }
        if(smooth == "none"){
          train <- ts(series.ts[(1 + i):(156 + i)], start = decimal_date(beginning + i), frequency = 12)
          test <- ts(series.ts[(157 + i)], start = decimal_date(beginning + 157 + i), frequency = 12)
        }
        
        if(!(method == "tslm")){
          if(method == "stl"){
            # Fit
            fit <- stl(train, s.window = "periodic", t.window = 7)
          }
          if(method == "ets"){
            fit <- ets(train)
          }
          if(method == "tbats"){
            fit <- tbats(train)
          }
          if(method == "stlf"){
            fit <- stlf(train)
          }
          if(method == "naive"){
            fit <- naive(train)
          }
          if(method == "snaive"){
            fit <- snaive(train)
          }
          if(method == "rwf"){
            fit <- rwf(train, drift = TRUE)
          }
          if(method == "meanf"){
            fit <- meanf(train)
          }
          # Forecast 1 step (week) ahead
          fcast <- forecast(fit, h = 1)
          # Calculate errors
          e <- as.numeric(test) - as.numeric(fcast$mean)  # Raw error
        }
        else{
          # Fit & forecast
          fit1 <- tslm(train ~ trend + season)
          fcast1 <- forecast(fit1, h = 1)
          
          fit2 <- auto.arima(fit1$residuals)
          fcast2 <- forecast(fit2, h = 1)
          
          y <- as.numeric(fcast1$mean)
          x <- as.numeric(fcast2$mean)
          fcast <- x + y
          
          # Calculate errors
          e <- as.numeric(test) - as.numeric(fcast)  # Raw error
        }
        
        ape <- abs(e)/test * 100  # We decided to change this to NON-critical
        
        # Save errors
        apes <- c(apes, ape)
    }
  }
  
  return(apes)
}

chosen_forecast <- function(chosen.model, series.ts, freq = "monthly"){
  if(freq == "monthly"){  # Monthly forecast
    train <- ts(tail(series.ts, 36), start = decimal_date(head(tail(monthly$date, 36), 1)), frequency = 12)  # 3 year window
    train25 <- ts(itsmr::smooth.fft(tail(series.ts, 36), .25), start = decimal_date(head(tail(monthly$date, 36), 1)), frequency = 12)
    train10 <- ts(itsmr::smooth.fft(tail(series.ts, 36), .10), start = decimal_date(head(tail(monthly$date, 36), 1)), frequency = 12)
    
    # Fit model
    if(chosen.model == 1){fit <- ets(train)}
    if(chosen.model == 2){fit <- stl(train, s.window = "periodic", t.window = 7)}
    if(chosen.model == 3){fit <- tbats(train)}
    if(chosen.model == 4){fit <- stlf(train)}
    if(chosen.model == 5){
      fit1 <- tslm(train ~ trend + season)
      fcast1 <- forecast(fit1, h = 6)
      
      fit2 <- auto.arima(fit1$residuals)
      fcast2 <- forecast(fit2, h = 6)
      
      y <- as.numeric(fcast1$mean)
      x <- as.numeric(fcast2$mean)
      forecasts <- (x + y)
      upper80s <- fcast1$upper[[1]]
      upper95s <- fcast1$upper[[2]]
      lower80s <- fcast1$lower[[1]]
      lower95s <- fcast1$lower[[2]]
    }
    if(chosen.model == 6){fit <- naive(train)}
    if(chosen.model == 7){fit <- snaive(train)}
    if(chosen.model == 8){fit <- rwf(train, drift = TRUE)}
    if(chosen.model == 9){fit <- meanf(train)}
    if(chosen.model == 10){fit <- ets(train25)}
    if(chosen.model == 11){fit <- stl(train25, s.window = "periodic", t.window = 7)}
    if(chosen.model == 12){fit <- tbats(train25)}
    if(chosen.model == 13){fit <- stlf(train25)}
    if(chosen.model == 14){
      fit1 <- tslm(train25 ~ trend + season)
      fcast1 <- forecast(fit1, h = 6)
      
      fit2 <- auto.arima(fit1$residuals)
      fcast2 <- forecast(fit2, h = 6)
      
      y <- as.numeric(fcast1$mean)
      x <- as.numeric(fcast2$mean)
      forecasts <- (x + y)
      upper80s <- fcast1$upper[[1]]
      upper95s <- fcast1$upper[[2]]
      lower80s <- fcast1$lower[[1]]
      lower95s <- fcast1$lower[[2]]
    }
    if(chosen.model == 15){fit <- naive(train25)}
    if(chosen.model == 16){fit <- snaive(train25)}
    if(chosen.model == 17){fit <- rwf(train25, drift = TRUE)}
    if(chosen.model == 18){fit <- meanf(train25)}
    if(chosen.model == 19){fit <- ets(train10)}
    if(chosen.model == 20){fit <- stl(train10, s.window = "periodic", t.window = 7)}
    if(chosen.model == 21){fit <- tbats(train10)}
    if(chosen.model == 22){fit <- stlf(train10)}
    if(chosen.model == 23){
      fit1 <- tslm(train10 ~ trend + season)
      fcast1 <- forecast(fit1, h = 6)
      
      fit2 <- auto.arima(fit1$residuals)
      fcast2 <- forecast(fit2, h = 6)
      
      y <- as.numeric(fcast1$mean)
      x <- as.numeric(fcast2$mean)
      forecasts <- (x + y)
      upper80s <- fcast1$upper[[1]]
      upper95s <- fcast1$upper[[2]]
      lower80s <- fcast1$lower[[1]]
      lower95s <- fcast1$lower[[2]]
    }
    if(chosen.model == 24){fit <- naive(train10)}
    if(chosen.model == 25){fit <- snaive(train10)}
    if(chosen.model == 26){fit <- rwf(train10, drift = TRUE)}
    if(chosen.model == 27){fit <- meanf(train10)}
    if(!(chosen.model %in% c(5, 14, 23))){
      # If model was not any of the LMx2 variants, forecast here
      fcast <- forecast(fit, h = 6)
      
      forecasts <- as.numeric(fcast$mean)
      upper80s <- as.numeric(fcast$upper[, 1])
      upper95s <- as.numeric(fcast$upper[, 2])
      lower80s <- as.numeric(fcast$lower[, 1])
      lower95s <- as.numeric(fcast$lower[, 2])
    }
    
    # Save to a returnable dataframe
    fdf <- data.frame(fcast = forecasts,
                      upper80 = upper80s,
                      upper95 = upper95s,
                      lower80 = lower80s,
                      lower95 = lower95s)
  }
  if(freq == "weekly"){  # Weekly forecast
    train <- ts(tail(series.ts, 156), start = decimal_date(head(tail(monthly$date, 156), 1)), frequency = 52)  # Three year window
    train25 <- ts(itsmr::smooth.fft(tail(series.ts, 156), .25), start = decimal_date(head(tail(monthly$date, 156), 1)), frequency = 52)
    train10 <- ts(itsmr::smooth.fft(tail(series.ts, 156), .10), start = decimal_date(head(tail(monthly$date, 156), 1)), frequency = 52)
    
    if(chosen.model == 1){fit <- ets(train)}
    if(chosen.model == 2){fit <- stl(train, s.window = "periodic", t.window = 7)}
    if(chosen.model == 3){fit <- tbats(train)}
    if(chosen.model == 4){fit <- stlf(train)}
    if(chosen.model == 5){
      fit1 <- tslm(train ~ trend + season)
      fcast1 <- forecast(fit1, h = 4)
      
      fit2 <- auto.arima(fit1$residuals)
      fcast2 <- forecast(fit2, h = 4)
      
      y <- as.numeric(fcast1$mean)
      x <- as.numeric(fcast2$mean)
      forecasts <- (x + y)
      upper80s <- fcast1$upper[[1]]
      upper95s <- fcast1$upper[[2]]
      lower80s <- fcast1$lower[[1]]
      lower95s <- fcast1$lower[[2]]
    }
    if(chosen.model == 6){fit <- naive(train)}
    if(chosen.model == 7){fit <- snaive(train)}
    if(chosen.model == 8){fit <- rwf(train, drift = TRUE)}
    if(chosen.model == 9){fit <- meanf(train)}
    if(chosen.model == 10){fit <- ets(train25)}
    if(chosen.model == 11){fit <- stl(train25, s.window = "periodic", t.window = 7)}
    if(chosen.model == 12){fit <- tbats(train25)}
    if(chosen.model == 13){fit <- stlf(train25)}
    if(chosen.model == 14){
      fit1 <- tslm(train25 ~ trend + season)
      fcast1 <- forecast(fit1, h = 4)
      
      fit2 <- auto.arima(fit1$residuals)
      fcast2 <- forecast(fit2, h = 4)
      
      y <- as.numeric(fcast1$mean)
      x <- as.numeric(fcast2$mean)
      forecasts <- (x + y)
      upper80s <- fcast1$upper[[1]]
      upper95s <- fcast1$upper[[2]]
      lower80s <- fcast1$lower[[1]]
      lower95s <- fcast1$lower[[2]]
    }
    if(chosen.model == 15){fit <- naive(train25)}
    if(chosen.model == 16){fit <- snaive(train25)}
    if(chosen.model == 17){fit <- rwf(train25, drift = TRUE)}
    if(chosen.model == 18){fit <- meanf(train25)}
    if(chosen.model == 19){fit <- ets(train10)}
    if(chosen.model == 20){fit <- stl(train10, s.window = "periodic", t.window = 7)}
    if(chosen.model == 21){fit <- tbats(train10)}
    if(chosen.model == 22){fit <- stlf(train10)}
    if(chosen.model == 23){
      fit1 <- tslm(train10 ~ trend + season)
      fcast1 <- forecast(fit1, h = 4)
      
      fit2 <- auto.arima(fit1$residuals)
      fcast2 <- forecast(fit2, h = 4)
      
      y <- as.numeric(fcast1$mean)
      x <- as.numeric(fcast2$mean)
      forecasts <- (x + y)
      upper80s <- fcast1$upper[[1]]
      upper95s <- fcast1$upper[[2]]
      lower80s <- fcast1$lower[[1]]
      lower95s <- fcast1$lower[[2]]
    }
    if(chosen.model == 24){fit <- naive(train10)}
    if(chosen.model == 25){fit <- snaive(train10)}
    if(chosen.model == 26){fit <- rwf(train10, drift = TRUE)}
    if(chosen.model == 27){fit <- meanf(train10)}
    if(!(chosen.model %in% c(5, 14, 23))){
      fcast <- forecast(fit, h = 4)
      forecasts <- as.numeric(fcast$mean)
      upper80s <- as.numeric(fcast$upper[, 1])
      upper95s <- as.numeric(fcast$upper[, 2])
      lower80s <- as.numeric(fcast$lower[, 1])
      lower95s <- as.numeric(fcast$lower[, 2])
    }
    
    fdf <- data.frame(fcast = forecasts,
                      upper80 = upper80s,
                      upper95 = upper95s,
                      lower80 = lower80s,
                      lower95 = lower95s)
  }
  
  return(fdf)
}

#2 An extended forecasting function is needed to safely create longer forecast sequences. It's not efficient per se,
# but keeps everything else intact.
# Forecasted periods are changed from 6 to 23 (to accommodate two whole years). The actual required length is determined in
# forecast_script.Rmd
chosen_forecast_extended <- function(chosen.model, series.ts, freq = "monthly"){
  if(freq == "monthly"){  # Monthly forecast
    train <- ts(tail(series.ts, 36), start = decimal_date(head(tail(monthly$date, 36), 1)), frequency = 12)  # 3 year window
    train25 <- ts(itsmr::smooth.fft(tail(series.ts, 36), .25), start = decimal_date(head(tail(monthly$date, 36), 1)), frequency = 12)
    train10 <- ts(itsmr::smooth.fft(tail(series.ts, 36), .10), start = decimal_date(head(tail(monthly$date, 36), 1)), frequency = 12)
    
    # Fit model
    if(chosen.model == 1){fit <- ets(train)}
    if(chosen.model == 2){fit <- stl(train, s.window = "periodic", t.window = 7)}
    if(chosen.model == 3){fit <- tbats(train)}
    if(chosen.model == 4){fit <- stlf(train)}
    if(chosen.model == 5){
      fit1 <- tslm(train ~ trend + season)
      fcast1 <- forecast(fit1, h = 23)
      
      fit2 <- auto.arima(fit1$residuals)
      fcast2 <- forecast(fit2, h = 23)
      
      y <- as.numeric(fcast1$mean)
      x <- as.numeric(fcast2$mean)
      forecasts <- (x + y)
      upper80s <- fcast1$upper[[1]]
      upper95s <- fcast1$upper[[2]]
      lower80s <- fcast1$lower[[1]]
      lower95s <- fcast1$lower[[2]]
    }
    if(chosen.model == 6){fit <- naive(train)}
    if(chosen.model == 7){fit <- snaive(train)}
    if(chosen.model == 8){fit <- rwf(train, drift = TRUE)}
    if(chosen.model == 9){fit <- meanf(train)}
    if(chosen.model == 10){fit <- ets(train25)}
    if(chosen.model == 11){fit <- stl(train25, s.window = "periodic", t.window = 7)}
    if(chosen.model == 12){fit <- tbats(train25)}
    if(chosen.model == 13){fit <- stlf(train25)}
    if(chosen.model == 14){
      fit1 <- tslm(train25 ~ trend + season)
      fcast1 <- forecast(fit1, h = 23)
      
      fit2 <- auto.arima(fit1$residuals)
      fcast2 <- forecast(fit2, h = 23)
      
      y <- as.numeric(fcast1$mean)
      x <- as.numeric(fcast2$mean)
      forecasts <- (x + y)
      upper80s <- fcast1$upper[[1]]
      upper95s <- fcast1$upper[[2]]
      lower80s <- fcast1$lower[[1]]
      lower95s <- fcast1$lower[[2]]
    }
    if(chosen.model == 15){fit <- naive(train25)}
    if(chosen.model == 16){fit <- snaive(train25)}
    if(chosen.model == 17){fit <- rwf(train25, drift = TRUE)}
    if(chosen.model == 18){fit <- meanf(train25)}
    if(chosen.model == 19){fit <- ets(train10)}
    if(chosen.model == 20){fit <- stl(train10, s.window = "periodic", t.window = 7)}
    if(chosen.model == 21){fit <- tbats(train10)}
    if(chosen.model == 22){fit <- stlf(train10)}
    if(chosen.model == 23){
      fit1 <- tslm(train10 ~ trend + season)
      fcast1 <- forecast(fit1, h = 23)
      
      fit2 <- auto.arima(fit1$residuals)
      fcast2 <- forecast(fit2, h = 23)
      
      y <- as.numeric(fcast1$mean)
      x <- as.numeric(fcast2$mean)
      forecasts <- (x + y)
      upper80s <- fcast1$upper[[1]]
      upper95s <- fcast1$upper[[2]]
      lower80s <- fcast1$lower[[1]]
      lower95s <- fcast1$lower[[2]]
    }
    if(chosen.model == 24){fit <- naive(train10)}
    if(chosen.model == 25){fit <- snaive(train10)}
    if(chosen.model == 26){fit <- rwf(train10, drift = TRUE)}
    if(chosen.model == 27){fit <- meanf(train10)}
    if(!(chosen.model %in% c(5, 14, 23))){
      # If model was not any of the LMx2 variants, forecast here
      fcast <- forecast(fit, h = 23)
      
      forecasts <- as.numeric(fcast$mean)
      upper80s <- as.numeric(fcast$upper[, 1])
      upper95s <- as.numeric(fcast$upper[, 2])
      lower80s <- as.numeric(fcast$lower[, 1])
      lower95s <- as.numeric(fcast$lower[, 2])
    }
    
    # Save to a returnable dataframe
    fdf <- data.frame(fcast = forecasts,
                      upper80 = upper80s,
                      upper95 = upper95s,
                      lower80 = lower80s,
                      lower95 = lower95s)
  }
  if(freq == "weekly"){  # Weekly forecast
    train <- ts(tail(series.ts, 156), start = decimal_date(head(tail(monthly$date, 156), 1)), frequency = 52)  # Three year window
    train25 <- ts(itsmr::smooth.fft(tail(series.ts, 156), .25), start = decimal_date(head(tail(monthly$date, 156), 1)), frequency = 52)
    train10 <- ts(itsmr::smooth.fft(tail(series.ts, 156), .10), start = decimal_date(head(tail(monthly$date, 156), 1)), frequency = 52)
    
    if(chosen.model == 1){fit <- ets(train)}
    if(chosen.model == 2){fit <- stl(train, s.window = "periodic", t.window = 7)}
    if(chosen.model == 3){fit <- tbats(train)}
    if(chosen.model == 4){fit <- stlf(train)}
    if(chosen.model == 5){
      fit1 <- tslm(train ~ trend + season)
      fcast1 <- forecast(fit1, h = 4)
      
      fit2 <- auto.arima(fit1$residuals)
      fcast2 <- forecast(fit2, h = 4)
      
      y <- as.numeric(fcast1$mean)
      x <- as.numeric(fcast2$mean)
      forecasts <- (x + y)
      upper80s <- fcast1$upper[[1]]
      upper95s <- fcast1$upper[[2]]
      lower80s <- fcast1$lower[[1]]
      lower95s <- fcast1$lower[[2]]
    }
    if(chosen.model == 6){fit <- naive(train)}
    if(chosen.model == 7){fit <- snaive(train)}
    if(chosen.model == 8){fit <- rwf(train, drift = TRUE)}
    if(chosen.model == 9){fit <- meanf(train)}
    if(chosen.model == 10){fit <- ets(train25)}
    if(chosen.model == 11){fit <- stl(train25, s.window = "periodic", t.window = 7)}
    if(chosen.model == 12){fit <- tbats(train25)}
    if(chosen.model == 13){fit <- stlf(train25)}
    if(chosen.model == 14){
      fit1 <- tslm(train25 ~ trend + season)
      fcast1 <- forecast(fit1, h = 4)
      
      fit2 <- auto.arima(fit1$residuals)
      fcast2 <- forecast(fit2, h = 4)
      
      y <- as.numeric(fcast1$mean)
      x <- as.numeric(fcast2$mean)
      forecasts <- (x + y)
      upper80s <- fcast1$upper[[1]]
      upper95s <- fcast1$upper[[2]]
      lower80s <- fcast1$lower[[1]]
      lower95s <- fcast1$lower[[2]]
    }
    if(chosen.model == 15){fit <- naive(train25)}
    if(chosen.model == 16){fit <- snaive(train25)}
    if(chosen.model == 17){fit <- rwf(train25, drift = TRUE)}
    if(chosen.model == 18){fit <- meanf(train25)}
    if(chosen.model == 19){fit <- ets(train10)}
    if(chosen.model == 20){fit <- stl(train10, s.window = "periodic", t.window = 7)}
    if(chosen.model == 21){fit <- tbats(train10)}
    if(chosen.model == 22){fit <- stlf(train10)}
    if(chosen.model == 23){
      fit1 <- tslm(train10 ~ trend + season)
      fcast1 <- forecast(fit1, h = 4)
      
      fit2 <- auto.arima(fit1$residuals)
      fcast2 <- forecast(fit2, h = 4)
      
      y <- as.numeric(fcast1$mean)
      x <- as.numeric(fcast2$mean)
      forecasts <- (x + y)
      upper80s <- fcast1$upper[[1]]
      upper95s <- fcast1$upper[[2]]
      lower80s <- fcast1$lower[[1]]
      lower95s <- fcast1$lower[[2]]
    }
    if(chosen.model == 24){fit <- naive(train10)}
    if(chosen.model == 25){fit <- snaive(train10)}
    if(chosen.model == 26){fit <- rwf(train10, drift = TRUE)}
    if(chosen.model == 27){fit <- meanf(train10)}
    if(!(chosen.model %in% c(5, 14, 23))){
      fcast <- forecast(fit, h = 4)
      forecasts <- as.numeric(fcast$mean)
      upper80s <- as.numeric(fcast$upper[, 1])
      upper95s <- as.numeric(fcast$upper[, 2])
      lower80s <- as.numeric(fcast$lower[, 1])
      lower95s <- as.numeric(fcast$lower[, 2])
    }
    
    fdf <- data.frame(fcast = forecasts,
                      upper80 = upper80s,
                      upper95 = upper95s,
                      lower80 = lower80s,
                      lower95 = lower95s)
  }
  
  return(fdf)
}

select_model <- function(beginning, series.ts, freq){
  # Standard
  ets.capes <- find_errors(beginning, series.ts, "ets", smooth = "none", freq = freq)
  stl.capes <- find_errors(beginning, series.ts, "stl", smooth = "none", freq = freq)
  tbats.capes <- find_errors(beginning, series.ts, "tbats", smooth = "none", freq = freq)
  stlf.capes <- find_errors(beginning, series.ts, "stlf", smooth = "none", freq = freq)
  lmx2.capes <- find_errors(beginning, series.ts, "tslm", smooth = "none", freq = freq)
  naive.capes <- find_errors(beginning, series.ts, "naive", smooth = "none", freq = freq)
  snaive.capes <- find_errors(beginning, series.ts, "snaive", smooth = "none", freq = freq)
  rwf.capes <- find_errors(beginning, series.ts, "rwf", smooth = "none", freq = freq)
  meanf.capes <- find_errors(beginning, series.ts, "meanf", smooth = "none", freq = freq)
  # Smooth .25
  smthets.capes <- find_errors(beginning, series.ts, "ets", smooth = ".25", freq = freq)
  smthstl.capes <- find_errors(beginning, series.ts, "stl", smooth = ".25", freq = freq)
  smthtbats.capes <- find_errors(beginning, series.ts, "tbats", smooth = ".25", freq = freq)
  smthstlf.capes <- find_errors(beginning, series.ts, "stlf", smooth = ".25", freq = freq)
  smthlmx2.capes <- find_errors(beginning, series.ts, "tslm", smooth = ".25", freq = freq)
  smthnaive.capes <- find_errors(beginning, series.ts, "naive", smooth = ".25", freq = freq)
  smthsnaive.capes <- find_errors(beginning, series.ts, "snaive", smooth = ".25", freq = freq)
  smthrwf.capes <- find_errors(beginning, series.ts, "rwf", smooth = ".25", freq = freq)
  smthmeanf.capes <- find_errors(beginning, series.ts, "meanf", smooth = ".25", freq = freq)
  # Smooth .10
  smth10ets.capes <- find_errors(beginning, series.ts, "ets", smooth = ".10", freq = freq)
  smth10stl.capes <- find_errors(beginning, series.ts, "stl", smooth = ".10", freq = freq)
  smth10tbats.capes <- find_errors(beginning, series.ts, "tbats", smooth = ".10", freq = freq)
  smth10stlf.capes <- find_errors(beginning, series.ts, "stlf", smooth = ".10", freq = freq)
  smth10lmx2.capes <- find_errors(beginning, series.ts, "tslm", smooth = ".10", freq = freq)
  smth10naive.capes <- find_errors(beginning, series.ts, "naive", smooth = ".10", freq = freq)
  smth10snaive.capes <- find_errors(beginning, series.ts, "snaive", smooth = ".10", freq = freq)
  smth10rwf.capes <- find_errors(beginning, series.ts, "rwf", smooth = ".10", freq = freq)
  smth10meanf.capes <- find_errors(beginning, series.ts, "meanf", smooth = ".10", freq = freq)
  
  
  m <- matrix(c(ets.capes, stl.capes, tbats.capes, stlf.capes, lmx2.capes, 
                naive.capes, snaive.capes, rwf.capes, meanf.capes,
                smthets.capes, smthstl.capes, smthtbats.capes, smthstlf.capes, 
                smthlmx2.capes, smthnaive.capes, smthsnaive.capes, smthrwf.capes, smthmeanf.capes,
                smth10ets.capes, smth10stl.capes, smth10tbats.capes, smth10stlf.capes, 
                smth10lmx2.capes, smth10naive.capes, smth10snaive.capes, smth10rwf.capes, smth10meanf.capes),
              ncol = 27,
              byrow = FALSE)
  
  mdf <- as.data.frame(m)
  
  chosen.model <- which.min(colMeans(mdf))[[1]]
  
  return(chosen.model)
}

draw_forecast <- function(forecast_dataframe, freq, history, type, modelname, palette){
  if(freq == "monthly"){
    ggplot(data = forecast_dataframe, aes(x = seq.Date(from = tail(history$date, 1), to = tail(history$date, 1) + months(5), by = "month"), y = fcast)) + 
      geom_segment(x = tail(history$date, 2)[1], y = tail(history[,type], 2)[1], xend = tail(history$date, 1), yend = head(forecast_dataframe$fcast, 1), color = "#D55E00", linetype = "dashed", alpha = palette[["alphaSeg"]]) +
      geom_ribbon(data = forecast_dataframe, aes(ymin = lower95, ymax = upper95), alpha = palette[["alpha95"]], fill = palette[["fill95"]]) + 
      geom_ribbon(data = forecast_dataframe, aes(ymin = lower80, ymax = upper80), alpha = palette[["alpha80"]], fill = palette[["fill80"]]) +
      geom_line(aes(colour = "Forecast"), size = 0.8) +
      geom_line(data = tail(head(history, -1), 24), aes(x = date, y = get(type), colour = palette[["colData"]]), size = 0.8, alpha = palette[["alphaSeg"]] ) +
      
      labs(title = paste0("6 month forecast: ", type),
           subtitle = paste0("Best performing model of the previous year: ", modelname),
           x = "",
           y = "Units per month",
           colour = "Series: ") +
      theme_minimal() + 
      theme(legend.position = "bottom", legend.margin = margin(t = -20, b = 0)) + 
      scale_colour_manual(values = c(palette[["colData"]], palette[["colPred"]]), labels = c("Data", "Forecast"))
  } else {
    ggplot(data = forecast_dataframe, aes(x = seq.Date(from = tail(history$startdate, 1), to = tail(history$startdate, 1) + weeks(3), by = "week"), y = fcast)) + 
      geom_segment(x = tail(history$startdate, 2)[1], y = tail(history[,type], 2)[1], xend = tail(history$startdate, 1), yend = head(forecast_dataframe$fcast, 1), color = "#D55E00", linetype = "dashed", alpha = palette[["alphaSeg"]]) +
      geom_ribbon(data = forecast_dataframe, aes(ymin = lower95, ymax = upper95), alpha = palette[["alpha95"]], fill = palette[["fill95"]]) + 
      geom_ribbon(data = forecast_dataframe, aes(ymin = lower80, ymax = upper80), alpha = palette[["alpha80"]], fill = palette[["fill80"]]) +
      geom_line(aes(colour = "Forecast"), size = 0.8) +
      geom_line(data = tail(head(history, -1), 16), aes(x = startdate, y = get(type), colour = palette[["colData"]]), size = 0.8, alpha = palette[["alphaSeg"]] ) +
      
      labs(title = paste0("4 week forecast: ", type),
           subtitle = paste0("Best performing model of the previous year: ", modelname),
           x = "",
           y = "Units per week",
           colour = "Series: ") +
      theme_minimal() + 
      theme(legend.position = "bottom", legend.margin = margin(t = -20, b = 0)) + 
      scale_colour_manual(values = c(palette[["colData"]], palette[["colPred"]]), labels = c("Data", "Forecast"))
  }
  
}

save_forecast <- function(fdf, months = TRUE, modelname, history, file, reverse_adj){
  # We will need these stored
  modelnames <- c("ETS", "STL", "TBATS", "STLF", "LMx2", "NAIVE", "SNAIVE", "RWF", "MEANF",
                  "ETS.25", "STL.25", "TBATS.25", "STLF.25", "LMx2.25", "NAIVE.25", "SNAIVE.25", "RWF.25", "MEANF.25",
                  "ETS.10", "STL.10", "TBATS.10", "STLF.10", "LMx2.10", "NAIVE.10", "SNAIVE.10", "RWF.10", "MEANF.10")
  
  # Extract type from filename
  if(months){
    type <- regmatches(filestr, regexec("monthly_(.*).csv", filestr))[[1]][2]
  } else{
    type <- regmatches(filestr, regexec("weekly_(.*).csv", filestr))[[1]][2]
  }
  
  
  if(months){
    # Prepare to save the new forecast into file
    save.this <- data.frame(time = tail(history$date, 1),
                            model = modelname, 
                            forecast = fdf$fcast[1] * reverse_adj[1],
                            upper80 = fdf$upper80[1] * reverse_adj[1],
                            upper95 = fdf$upper95[1] * reverse_adj[1],
                            lower80 = fdf$lower80[1] * reverse_adj[1],
                            lower95 = fdf$lower95[1] * reverse_adj[1]) 
  } else{
    save.this <- data.frame(time = tail(history$startdate, 1), 
                            model = modelname, 
                            forecast = fdf$fcast[1],
                            upper80 = fdf$upper80[1],
                            upper95 = fdf$upper95[1],
                            lower80 = fdf$lower80[1],
                            lower95 = fdf$lower95[1]) 
  }
  
  # Does forecast history exist?
  if(file.exists(file)){
    fcast.history <- read.csv(file, header = TRUE)
    fcast.history$time <- as.Date(fcast.history$time)
    
    if(months){
      # Check if missing dates in forecast history
      all.dates <- seq.Date(min(fcast.history$time), max(fcast.history$time), "month") # Build a sequence of all dates
      missing <- all.dates[!all.dates %in% fcast.history$time] # Check if some are missing
    } else{
      # Check if missing dates in forecast history
      all.dates <- seq.Date(min(fcast.history$time), max(fcast.history$time), "week") # Build a sequence of all dates
      missing <- all.dates[!all.dates %in% fcast.history$time] # Check if some are missing
    }
    
    if(length(missing) == 0){
      # Save newest forecast as usual
      if(save.this$time %in% fcast.history$time){
        warning("Data point already in file!") # Do not save if a forecast exists already!
      } else{
        write.table(save.this, file = file, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
      }
    } else{
      # Fix missing dates
      dat2 <- data.frame(time = all.dates)
      fixed.fhistory <- merge(fcast.history, dat2, all = TRUE)
      
      # Iterate through missing days and compute forecasts for them, then insert to fixed.fhistory
      if(months){
        for(mdate in missing){
          cutreal<- history[history$date <= mdate, ] # Cut real history at the missing date
          beginning <- head(tail(cutreal$date, 49), 1) # Define beginning of 4 year window 
          segment <- head(tail(cutreal[, type], 49), 48) # Define 4 year window
          series.ts <- ts(segment, start = decimal_date(beginning), frequency = 12) # Transform into a ts object
          
          scaleback <- as.numeric(bizdays(ts(seq(1), start = decimal_date(cutreal$date[length(cutreal$date)]), frequency = 12), FinCenter = "Zurich")) # Scaler for saving
          
          # Forecast
          chosen.model <- select_model(beginning, series.ts, freq = "monthly") # Choose model
          mcast <- chosen_forecast(chosen.model, series.ts, freq = "monthly") # Output a forecast
          missing.fcast <- data.frame(time = tail(cutreal$date, 1), 
                                      model = modelnames[chosen.model], 
                                      forecast = mcast$fcast[1] * scaleback,
                                      upper80 = mcast$upper80[1] * scaleback,
                                      upper95 = mcast$upper95[1] * scaleback,
                                      lower80 = mcast$lower80[1] * scaleback,
                                      lower95 = mcast$lower95[1] * scaleback) 
          
          fixed.fhistory[fixed.fhistory$time == mdate, ] <- missing.fcast
        }
      } else{
        for(mdate in missing){
          cutreal<- history[history$startdate <= mdate, ] # Cut real history at the missing date
          wbeginning <- head(tail(cutreal$startdate, 209), 1) # Define beginning of 4 year window 
          segment <- head(tail(cutreal[, type], 209), 208) # Define 4 year window
          series.ts <- ts(segment, start = decimal_date(wbeginning), frequency = 52) # Transform into a ts object
          
          # Forecast
          chosen.model <- select_model(beginning, series.ts, freq = "weekly") # Choose model
          mcast <- chosen_forecast(chosen.model, series.ts, freq = "weekly") # Output a forecast
          missing.fcast <- data.frame(time = tail(cutreal$startdate, 1), 
                                      model = modelnames[chosen.model], 
                                      forecast = mcast$fcast[1],
                                      upper80 = mcast$upper80[1],
                                      upper95 = mcast$upper95[1],
                                      lower80 = mcast$lower80[1],
                                      lower95 = mcast$lower95[1]) 
          
          fixed.fhistory[fixed.fhistory$time == mdate, ] <- missing.fcast
        }
      }
      
      # First save the fixed forecast history into a file
      write.csv(fixed.fhistory, file = file, row.names = FALSE)
      
      # Then save newest forecast as usual
      if(save.this$time %in% fcast.history$time){
        warning("Data point already in file!") # Do not save if a forecast exists already!
      } else{
        write.table(save.this, file = file, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
        }
    }
  
  } else{
    # If history is missing entirely, we choose to generate 2 years for monthly fcasts and 6 months for weekly fcasts (required for plotting)
    if(months){
      N <- 24 # Preallocate 24 rows
      forecast.history <- data.frame(time = rep(as.Date("2016-01-01"), N),
                                     model = rep("", N),
                                     forecast = rep(NA, N),
                                     upper80 = rep(NA, N),
                                     upper95 = rep(NA, N),
                                     lower80 = rep(NA, N),
                                     lower95 = rep(NA, N),
                                     stringsAsFactors = FALSE)
      
      for(i in seq(0, 23)){
        cutreal <- head(history, -(24-i)) # Erase (24-x) months from real history
        beginning <- head(tail(cutreal$date, 49), 1) # Define beginning of 4 year window 
        segment <- head(tail(cutreal[, type], 49), 48) # Define 4 year window
        series.ts <- ts(segment, start = decimal_date(beginning), frequency = 12) # Transform into a ts object
        
        scaleback <- as.numeric(bizdays(ts(seq(1), start = decimal_date(cutreal$date[length(cutreal$date)]), frequency = 12), FinCenter = "Zurich")) # Scaler for saving
        
        # Forecast
        chosen.model <- select_model(beginning, series.ts, freq = "monthly") # Choose model
        mcast <- chosen_forecast(chosen.model, series.ts, freq = "monthly") # Output a forecast
        missing.fcast <- data.frame(time = tail(cutreal$date, 1), 
                                    model = modelnames[chosen.model], 
                                    forecast = mcast$fcast[1] * scaleback,
                                    upper80 = mcast$upper80[1] * scaleback,
                                    upper95 = mcast$upper95[1] * scaleback,
                                    lower80 = mcast$lower80[1] * scaleback,
                                    lower95 = mcast$lower95[1] * scaleback)
        forecast.history[(i+1), ] <- missing.fcast
      }
      
      # Save into a csv
      write.csv(forecast.history, file = file, row.names = FALSE)
      
    } else{
      N <- 24 # Preallocate 52 rows
      forecast.history <- data.frame(time = rep(as.Date("2016-01-01"), N),
                                     model = rep("", N),
                                     forecast = rep(NA, N),
                                     upper80 = rep(NA, N),
                                     upper95 = rep(NA, N),
                                     lower80 = rep(NA, N),
                                     lower95 = rep(NA, N),
                                     stringsAsFactors = FALSE)
      
      for(i in seq(0, 23)){
        cutreal <- head(history, -(24-i)) # Erase (24-x) weeks from real history
        wbeginning <- head(tail(cutreal$startdate, 209), 1) # Define beginning of 4 year window 
        segment <- head(tail(cutreal[, type], 209), 208)
        series.ts <- ts(segment, start = decimal_date(wbeginning), frequency = 52)
        
        # Forecast
        chosen.model <- select_model(wbeginning, series.ts, "weekly") # Choose model
        mcast <- chosen_forecast(chosen.model, series.ts, freq = "weekly") # Output a forecast
        missing.fcast <- data.frame(time = tail(cutreal$startdate, 1), 
                                    model = modelnames[chosen.model], 
                                    forecast = mcast$fcast[1],
                                    upper80 = mcast$upper80[1],
                                    upper95 = mcast$upper95[1],
                                    lower80 = mcast$lower80[1],
                                    lower95 = mcast$lower95[1])
        forecast.history[(i+1), ] <- missing.fcast
      }
      
      # Save into a csv
      write.csv(forecast.history, file = file, row.names = FALSE)
    }
    
    # Then save newest forecast as usual
    write.table(save.this, file = file, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    
  }
  
}

draw_history <- function(forecasthistory, history, freq = "monthly", type, palette){
  # Monthly forecast history plot: 2 years
  # Weekly forecast history plot: 6 months
  # Tables: full history for both
  
  fcast.history.full <- head(read.csv(forecasthistory, header = TRUE), -1)
  fcast.history.full$err <- abs((tail(head(history[[type]], -1), length(fcast.history.full$forecast)) - fcast.history.full$forecast) / tail(head(history[[type]], -1), length(fcast.history.full$forecast))) * 100
  
  # Plot
  if(freq == "monthly"){
    # Cut for plots: 2 years
    fcast.history <- tail(fcast.history.full, 24)
    
    p <- ggplot(data = fcast.history, aes(x = as.Date(time), y = forecast, colour = "Forecast"), label = model) +
      geom_line(aes(colour = "Forecast"), size = 0.9, alpha = palette[["alphaSeg"]]) +
      geom_line(data = head(history, -1), aes(x = date, y = get(type), colour = "Data"), size = 0.8, alpha = palette[["alphaSeg"]]) +
      labs(title = "Monthly history (2 years)",
           subtitle = paste0("Mean error: ", round(mean(fcast.history$err), 2), "%"),
           x = "",
           y = "Units per month",
           colour = "Series: ") +
      theme_minimal() + 
      theme(legend.position = "bottom", legend.margin = margin(t = -20, b = 0)) + 
      scale_colour_manual(values = c(palette[["colData"]], palette[["colPred"]]), labels = c("Data", "Forecast"))
  } else{
    # Cut for plots: 6 months
    fcast.history <- tail(fcast.history.full, 24)
    
    p <- ggplot(data = fcast.history, aes(x = as.Date(time), y = forecast, colour = "Forecast"), label = model) +
      geom_line(aes(colour = "Forecast"), size = 0.9, alpha = palette[["alphaSeg"]]) +
      geom_line(data = head(history, -1), aes(x = startdate, y = get(type), colour = "Data"), size = 0.8, alpha = palette[["alphaSeg"]]) +
      labs(title = "Weekly history (6 months)",
           subtitle = paste0("Mean error: ", round(mean(fcast.history$err), 2), "%"),
           x = "",
           y = "Units per week",
           colour = "Series: ") +
      theme_minimal() + 
      theme(legend.position = "bottom", legend.margin = margin(t = -20, b = 0)) + 
      scale_colour_manual(values = c(palette[["colData"]], palette[["colPred"]]), labels = c("Data", "Forecast"))
  }
  print(p)
  model.freq <- as.data.frame(count(fcast.history.full$model), stringsAsFactors = FALSE); colnames(model.freq) <- c("model", "frequency")
  model.freq$model <- as.character(model.freq$model)
  model.freq$error <- round(aggregate(fcast.history.full$err, by = list(fcast.history.full$model), mean)$x, 2)
  for(name in modelnames){
    if(!(name %in% model.freq$model)){
      model.freq <- rbind(model.freq, list(name, 0, 0))
    }
  }
  model.freq$frequency <- round(model.freq$frequency / sum(model.freq$frequency) * 100, 2)
  model.freq <- data.frame(model.freq, row.names = 1)
  datatable(model.freq, rownames = TRUE, filter="top", options = list(pageLength = 9))
  
}