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