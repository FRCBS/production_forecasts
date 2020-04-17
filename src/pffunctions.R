# Helper functions


extract_type <- function(red.distr, type){
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

find_errors <- function(segment, beginning, series.ts, method = "none", smooth = "none", freq = "monthly"){
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

select_model <- function(segment, beginning, series.ts, freq){
  # Standard
  ets.capes <- find_errors(segment, beginning, series.ts, "ets", smooth = "none", freq = freq)
  stl.capes <- find_errors(segment, beginning, series.ts, "stl", smooth = "none", freq = freq)
  tbats.capes <- find_errors(segment, beginning, series.ts, "tbats", smooth = "none", freq = freq)
  stlf.capes <- find_errors(segment, beginning, series.ts, "stlf", smooth = "none", freq = freq)
  lmx2.capes <- find_errors(segment, beginning, series.ts, "tslm", smooth = "none", freq = freq)
  naive.capes <- find_errors(segment, beginning, series.ts, "naive", smooth = "none", freq = freq)
  snaive.capes <- find_errors(segment, beginning, series.ts, "snaive", smooth = "none", freq = freq)
  rwf.capes <- find_errors(segment, beginning, series.ts, "rwf", smooth = "none", freq = freq)
  meanf.capes <- find_errors(segment, beginning, series.ts, "meanf", smooth = "none", freq = freq)
  # Smooth .25
  smthets.capes <- find_errors(segment, beginning, series.ts, "ets", smooth = ".25", freq = freq)
  smthstl.capes <- find_errors(segment, beginning, series.ts, "stl", smooth = ".25", freq = freq)
  smthtbats.capes <- find_errors(segment, beginning, series.ts, "tbats", smooth = ".25", freq = freq)
  smthstlf.capes <- find_errors(segment, beginning, series.ts, "stlf", smooth = ".25", freq = freq)
  smthlmx2.capes <- find_errors(segment, beginning, series.ts, "tslm", smooth = ".25", freq = freq)
  smthnaive.capes <- find_errors(segment, beginning, series.ts, "naive", smooth = ".25", freq = freq)
  smthsnaive.capes <- find_errors(segment, beginning, series.ts, "snaive", smooth = ".25", freq = freq)
  smthrwf.capes <- find_errors(segment, beginning, series.ts, "rwf", smooth = ".25", freq = freq)
  smthmeanf.capes <- find_errors(segment, beginning, series.ts, "meanf", smooth = ".25", freq = freq)
  # Smooth .10
  smth10ets.capes <- find_errors(segment, beginning, series.ts, "ets", smooth = ".10", freq = freq)
  smth10stl.capes <- find_errors(segment, beginning, series.ts, "stl", smooth = ".10", freq = freq)
  smth10tbats.capes <- find_errors(segment, beginning, series.ts, "tbats", smooth = ".10", freq = freq)
  smth10stlf.capes <- find_errors(segment, beginning, series.ts, "stlf", smooth = ".10", freq = freq)
  smth10lmx2.capes <- find_errors(segment, beginning, series.ts, "tslm", smooth = ".10", freq = freq)
  smth10naive.capes <- find_errors(segment, beginning, series.ts, "naive", smooth = ".10", freq = freq)
  smth10snaive.capes <- find_errors(segment, beginning, series.ts, "snaive", smooth = ".10", freq = freq)
  smth10rwf.capes <- find_errors(segment, beginning, series.ts, "rwf", smooth = ".10", freq = freq)
  smth10meanf.capes <- find_errors(segment, beginning, series.ts, "meanf", smooth = ".10", freq = freq)
  
  
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

save_forecast <- function(fdf, months = TRUE, modelname, file){
  if(months){
    # Prepare to save the new forecast into file
    save.this <- data.frame(time = tail(monthly$date, 1), 
                            model = modelname, 
                            forecast = fdf$fcast[1] * reverse_adj[1],
                            upper80 = fdf$upper80[1] * reverse_adj[1],
                            upper95 = fdf$upper95[1] * reverse_adj[1],
                            lower80 = fdf$lower80[1] * reverse_adj[1],
                            lower95 = fdf$lower95[1] * reverse_adj[1]) 
  } else{
    save.this <- data.frame(time = tail(weekly$startdate, 1), 
                            model = modelname, 
                            forecast = fdf$fcast[1],
                            upper80 = fdf$upper80[1],
                            upper95 = fdf$upper95[1],
                            lower80 = fdf$lower80[1],
                            lower95 = fdf$lower95[1]) 
  }
  
  #Save only if file already exists (some histories have not been initialized yet)
  if(file.exists(file)){
    # Do not save if forecast already exists in history
    existing.history <- read.csv(file)
    existing.history$time <- as.Date(existing.history$time)
    if(save.this$time %in% existing.history$time){
      warning("Data point already in file!")
    } else{
      # Save to file for historical review
      write.table(save.this, file = file, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    }
  } else{
    warning("History not initialized!")
  }
}

draw_history <- function(forecasthistory, history, freq = "monthly", type, palette){
  # Load history if it exists
  if(file.exists(forecasthistory)){
    FILE.EXISTS <- TRUE
    fcast.history <- head(read.csv(forecasthistory, header = TRUE), -1)
    fcast.history$err <- abs((tail(head(history[[type]], -1), length(fcast.history$forecast)) - fcast.history$forecast) / tail(head(history[[type]], -1),
                                                                                                              length(fcast.history$forecast)))*100
    # Plot
    if(freq == "monthly"){
      p <- ggplot(data = fcast.history, aes(x = as.Date(time), y = forecast, colour = "Forecast"), label = model) +
        geom_line(aes(colour = "Forecast"), size = 0.9, alpha = palette[["alphaSeg"]]) +
        geom_line(data = head(history, -1), aes(x = date, y = get(type), colour = "Data"), size = 0.8, alpha = palette[["alphaSeg"]]) +
        labs(title = "Monthly history",
             subtitle = paste0("Mean error: ", round(mean(fcast.history$err), 2), "%"),
             x = "",
             y = "Units per month",
             colour = "Series: ") +
        theme_minimal() + 
        theme(legend.position = "bottom", legend.margin = margin(t = -20, b = 0)) + 
        scale_colour_manual(values = c(palette[["colData"]], palette[["colPred"]]), labels = c("Data", "Forecast"))
    } else{
      p <- ggplot(data = fcast.history, aes(x = as.Date(time), y = forecast, colour = "Forecast"), label = model) +
        geom_line(aes(colour = "Forecast"), size = 0.9, alpha = palette[["alphaSeg"]]) +
        geom_line(data = head(history, -1), aes(x = startdate, y = get(type), colour = "Data"), size = 0.8, alpha = palette[["alphaSeg"]]) +
        labs(title = "Weekly history",
             subtitle = paste0("Mean error: ", round(mean(fcast.history$err), 2), "%"),
             x = "",
             y = "Units per week",
             colour = "Series: ") +
        theme_minimal() + 
        theme(legend.position = "bottom", legend.margin = margin(t = -20, b = 0)) + 
        scale_colour_manual(values = c(palette[["colData"]], palette[["colPred"]]), labels = c("Data", "Forecast"))
    }
    print(p)
    model.freq <- as.data.frame(count(fcast.history$model), stringsAsFactors = FALSE); colnames(model.freq) <- c("model", "frequency")
    model.freq$model <- as.character(model.freq$model)
    model.freq$error <- round(aggregate(fcast.history$err, by = list(fcast.history$model), mean)$x, 2)
    for(name in modelnames){
      if(!(name %in% model.freq$model)){
        model.freq <- rbind(model.freq, list(name, 0, 0))
      }
    }
    model.freq$frequency <- round(model.freq$frequency / sum(model.freq$frequency) * 100, 2)
    model.freq <- data.frame(model.freq, row.names = 1)
    datatable(model.freq, rownames = TRUE, filter="top", options = list(pageLength = 9))
  } else {
    FILE.EXISTS <- FALSE
    warning("History not available.")
  }
}