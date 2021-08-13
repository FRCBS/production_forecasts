## ---------------------------
## Script name: report_functions.R
##
## Purpose of script: Does as it says on the tin.
##                    Various functions needed for running
##                    public_forecast_script.Rmd
##
## Author: Esa Turkulainen, M.Sc.
##
## Date Created: 2021-08-03
##
## License: GNU General Public License v3.0
## Email: esa.turkulainen@veripalvelu.fi
##
## ---------------------------
## Notes: This script is made public as
##        a companion piece for practical
##        implementation of methods explored
##        in a publication.
##
## ---------------------------

##### Define heuristics here
MAPE_selection <- function(history, testN) {
  # Select single method using valN past observations
  #
  # ARGUMENTS
  # history is a dataframe of method accuracies wo a date column
  # valN is the length of the validation period
  #
  # RETURNS
  # An integer: column index of the method with smallest MAPE during
  # valN observations.

  val_segment <- tail(history, testN)
  method_mapes <- colMeans(val_segment)
  selected_col <- which.min(method_mapes)
  return(selected_col)
}

wavg_selection <- function(history, alpha) {
  # Get WAVG configuration
  #
  # ARGUMENTS
  # history: a dataframe of method accuracies wo a date column
  # alpha: coefficient for exp. decay
  #
  # RETURNS
  # A named list: method name, coefs for weighting, order of methods for weighting
  #
  # NOTES: get_forecast expects that selected_method is either just a method name
  #        or a named list object with method name, and any other info needed to
  #        run the method.

  alpahavec <- alpha ** (1:13) # 1:(Number of methods)
  last <- history[nrow(history), ]

  return(list(method = "wavg", coef = alphavec, order = order(last)))
}


##### Other helper functions here

ensure_window_parity <- function(x, y) {
  # Compute accuracies for earlier forecasts (for method selection)
  #
  # ARGUMENTS
  # x: first series
  # y: second series
  #
  # RETURN
  # Both series, trimmed to identical date ranges

  # Assumes date column is first
  if (x[1, 1] > y[1, 1]) start <- x[1, 1] else start <- y[1, 1]
  if (x[nrow(x), 1] < y[nrow(y), 1]) end <- x[nrow(x), 1] else end <- y[nrow(y), 1]
  x <- x[x[, 1] >= start & x[, 1] <= end, ]
  y <- y[y[, 1] >= start & y[, 1] <= end, ]

  return(list(first = x, second = y))
}

generate_fcast_history <- function(series, path = "~/all_fcasts_history.csv", res = "weekly", rw_years = 2) {
  # Create forecasts from all of the methods in the pool
  #
  # ARGUMENTS
  # series is a time series of interest
  # path is the saving location and filename
  # res is the temporal resolution of the series
  #
  # RETURN
  # Nothing

  # Set (rolling) training window size.
  if (res == "daily") {
    rw <- floor(365.25 * rw_years)
  }
  if (res == "weekly") {
    rw <- ceiling(52.17857 * rw_years)
  }
  if (res == "monthly") {
    rw <- 12 * rw_years
  }
  if (res == "yearly") {
    rw <- rw_years
  }

  # Prepare any methods for saving
  snaive <- c()
  ma5 <- c()
  ma7 <- c()
  ma9 <- c()
  ma12 <- c()
  ets <- c()
  stl <- c()
  tbats <- c()
  stlf <- c()
  arimax <- c()
  dynreg <- c()
  nn <- c()

  for (i in 0:(nrow(series) - rw - 1)) {
    train_start <- series[(1 + i), 1]

    if (res == "daily") {
      train <- subset(series[, 2], series[, 1] >= train_start & series[, 1] <= train_start + days(rw - 1))
    }
    if (res == "weekly") {
      train <- subset(series[, 2], series[, 1] >= train_start & series[, 1] <= train_start + weeks(rw - 1))
    }
    if (res == "monthly") {
      train <- subset(series[, 2], series[, 1] >= train_start & series[, 1] <= train_start + months(rw - 1))
    }
    if (res == "yearly") {
      train <- subset(series[, 2], series[, 1] >= train_start & series[, 1] <= train_start + years(rw - 1))
    }

    # Make train set into a ts object for forecast functions
    if (res == "daily") {
      train_ts <- ts(train, start = c(year(train_start), month(train_start), day(train_start)), frequency = 7)
    }
    if (res == "weekly") {
      train_ts <- ts(train, start = c(year(train_start), month(train_start), day(train_start)), frequency = 52)
    }
    if (res == "monthly") {
      train_ts <- ts(train, start = c(year(train_start), month(train_start), day(train_start)), frequency = 12)
    }
    if (res == "yearly") {
      train_ts <- ts(train, start = c(year(train_start), month(train_start), day(train_start)), frequency = 1)
    }

    # Create xregs for ARIMAX
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
    for (j in 1:nrow) {
      if (res == "daily") {
        month.m[j,] <- onehotyear[month(train_start + days(j - 1)), ]
      }
      if (res == "weekly") {
        month.m[j,] <- onehotyear[month(train_start + weeks(j - 1)), ]
      }
      if (res == "monthly") {
        month.m[j,] <- onehotyear[month(train_start + months(j - 1)), ]
      }
      if (res == "yearly") {
        month.m[j,] <- onehotyear[month(train_start + years(j - 1)), ]
      }
    }
    horizon.m <- matrix(, nrow = 1, ncol = 11)
    k <- 0
    for (j in nrow:nrow) {
      k <- k + 1
      if (res == "daily") {
        horizon.m[k,] <- onehotyear[month(train_start + days(j)), ]
      }
      if (res == "weekly") {
        horizon.m[k,] <- onehotyear[month(train_start + weeks(j)), ]
      }
      if (res == "monthly") {
        horizon.m[k,] <- onehotyear[month(train_start + months(j)), ]
      }
      if (res == "yearly") {
        horizon.m[k,] <- onehotyear[month(train_start + years(j)), ]
      }
    }

    # Seasonal naive
    fit <- snaive(train_ts)
    snaive[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

    # MA5
    fit <- ma(train_ts, order = 5)
    ma5[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

    # MA7
    fit <- ma(train_ts, order = 7)
    ma7[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

    # MA9
    fit <- ma(train_ts, order = 9)
    ma9[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

    # M12
    fit <- ma(train_ts, order = 12)
    ma12[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

    # TBATS
    fit <- tbats(train_ts)
    tbats[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

    # STLF
    fit <- stlf(train_ts)
    stlf[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

    # ARIMAX
    fit <- auto.arima(train_ts, xreg = month.m)
    arimax[i + 1] <- as.numeric(forecast(fit, xreg = horizon.m, h = 1)$mean)

    # DYNREG
    fit1 <- tslm(train_ts ~ trend + season)
    fcast1 <- forecast(fit1, h = 1)

    fit2 <- auto.arima(fit1$residuals)
    fcast2 <- forecast(fit2, h = 1)

    y <- as.numeric(fcast1$mean)
    x <- as.numeric(fcast2$mean)
    dynreg[i + 1] <- (x + y)

    # NN
    fit <- nnetar(train_ts)
    nn[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

    # ETS
    fit <- ets(train_ts)
    ets[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)

    # STL
    fit <- stl(train_ts, s.window = "periodic", t.window = 7)
    stl[i + 1] <- as.numeric(forecast(fit, h = 1)$mean)
  }

  # Create a date vector for the forecasts
  if (res == "daily") {
    fcast_dates <- seq.Date(from = series[(rw + 1), 1], to = series[nrow(series), 1], by = "days")
  }
  if (res == "weekly") {
    fcast_dates <- seq.Date(from = series[(rw + 1), 1], to = series[nrow(series), 1], by = "weeks")
  }
  if (res == "monthly") {
    fcast_dates <- seq.Date(from = series[(rw + 1), 1], to = series[nrow(series), 1], by = "months")
  }
  if (res == "yearly") {
    fcast_dates <- seq.Date(from = series[(rw + 1), 1], to = series[nrow(series), 1], by = "years")
  }

  # Combine into a df
  forecasts_wo_avg <- data.frame(date = fcast_dates, snaive = snaive, ma5 = ma5, ma7 = ma7,
                                 ma9 = ma9, ma12 = ma12, ets = ets, stl = stl, tbats = tbats,
                                 nn = nn, arimax = arimax, dynreg = dynreg, stlf = stlf)

  # Create method average (AVG)
  avg <- apply(forecasts_wo_avg[, -1], 1, mean)
  # Add it to
  indv_forecasts <- cbind(forecasts_wo_avg, avg)

  # Save
  write.csv(indv_forecasts, path, row.names = FALSE)
}

select_method <- function(series, path = "~/all_fcasts_history.csv", heur = 12, res = "weekly", rw_years = 2) {
  # Select method for next forecast using heuristics
  #
  # ARGUMENTS
  # series: a time series of interest
  # path: the loading location of the fcast history
  # heur: the desired heuristic; if INT, defaults to MAPE_selection
  #
  # RETURN
  # Either a character vector (method name) or a list object

  if (file.exists(path)) { # If history exists
      fcast_history <- read.csv(path) # use it
      fcast_history[, 1] <- as.Date(fcast_history[, 1])
      method_names <- names(fcast_history)[-1] # Drop first column name (it should be "date", and not a method name)

      # Get accuracies for the fcast_history
      acc_history <- get_accuracies(fcast_history, series)

      if (is.numeric(heur)) { # If heur is an int, use historical MAPE to choose a method
          method_ind <- MAPE_selection(acc_history[, -1], heur)
          return(method_names[method_ind])

      } else { # If heur is not an int, use specific flags for heuristics

          if (heur == "wavg") {
              return(wavg_selection(acc_history[, -1]))
          }

          if (heur == "YOURHEURHERE") {
          }

          if (!(heur %in% c("wavg", "YOURHEURHERE"))) {
              stop("I can't recognize this heuristic! Try e.g., heur = 'wavg' or any integer.")
          }
      }
  } else { # If history doesn't exist, generate it
      generate_acc_history(series, path, res, rw_years)
      # And then select as before
      fcast_history <- read.csv(path) # use it
      fcast_history[, 1] <- as.Date(fcast_history[, 1])
      method_names <- names(fcast_history)[-1] # Drop first column name (it should be "date", and not a method name)

      # Get accuracies for the fcast_history
      acc_history <- get_accuracies(fcast_history, series)

      if (is.numeric(heur)) { # If heur is an int, use historical MAPE to choose a method
          method_ind <- MAPE_selection(acc_history[, -1], heur)
          return(method_names[method_ind])

      } else { # If heur is not an int, use specific flags for heuristics

          if (heur == "wavg") {
              return(wavg_selection(acc_history[, -1]))
          }

          if (heur == "YOURHEURHERE") {
          }

          if (!(heur %in% c("wavg", "YOURHEURHERE"))) {
              stop("I can't recognize this heuristic! Try e.g., heur = 'wavg' or any integer.")
          }
      }
    }
}

get_accuracies <- function(fcasts, series, metric = "MAPE") {
  # Compute accuracies for earlier forecasts (for method selection)
  #
  # ARGUMENTS
  # fcasts: fcast history of all methods
  # series: time series of interest
  # metric: defaults to MAPE. Add others if needed.
  #
  # RETURN
  # History of accuracies for all methods

  # This function currently assumes that neither 'fcasts' nor 'series'
  # have gaps in the series.

  # Ensure that we match lengths
  out <- ensure_window_parity(fcasts, series)
  fcasts <- out$first
  series <- out$second

  if (metric == "MAPE") {
    MAPEs <- 100 * abs(series[, -1] - fcasts) / series[, -1]
    MAPES$date <- series[, 1]

    return(MAPEs)
  }
  if (metric == "something else entirely") {
    # do: stuff
    # return(stuff done)
  }

}

get_forecast <- function(series, selected_method, h = 1, res = "weekly", rw_years = 2, alpha = 0.5, path = "~/all_fcasts_history.csv") {
  # Get requested new forecast using selected method
  #
  # ARGUMENTS
  # series: time series of interest
  # selected_method: either a method name (char) or a list object
  # h: forecasting horizon
  # res: temporal resolution of the series
  # rw_years: size of the rolling window in years
  # alpha: chosen alpha for WAVG forecast (even if not the selected method)
  # path: this function saves all method forecasts, but returns only the selected. path to file.
  #
  # RETURN
  # The forecast

  if (res == "daily") {
    rw <- floor(365.25 * rw_years)
  }
  if (res == "weekly") {
    rw <- ceiling(52.17857 * rw_years)
  }
  if (res == "monthly") {
    rw <- 12 * rw_years
  }
  if (res == "yearly") {
    rw <- rw_years
  }

  # Get training period
  segment <- tail(series, rw)
  train_start <- segment[1, 1]
  train <- segment[, 2]

  # Make train set into a ts object for forecast functions
  if (res == "daily") {
    train_ts <- ts(train, start = c(year(train_start), month(train_start), day(train_start)), frequency = 7)
  }
  if (res == "weekly") {
    train_ts <- ts(train, start = c(year(train_start), month(train_start), day(train_start)), frequency = 52)
  }
  if (res == "monthly") {
    train_ts <- ts(train, start = c(year(train_start), month(train_start), day(train_start)), frequency = 12)
  }
  if (res == "yearly") {
    train_ts <- ts(train, start = c(year(train_start), month(train_start), day(train_start)), frequency = 1)
  }

  # Create xregs for ARIMAX
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
  for (j in 1:nrow) {
    if (res == "daily") {
      month.m[j,] <- onehotyear[month(train_start + days(j - 1)), ]
    }
    if (res == "weekly") {
      month.m[j,] <- onehotyear[month(train_start + weeks(j - 1)), ]
    }
    if (res == "monthly") {
      month.m[j,] <- onehotyear[month(train_start + months(j - 1)), ]
    }
    if (res == "yearly") {
      month.m[j,] <- onehotyear[month(train_start + years(j - 1)), ]
    }
  }
  horizon.m <- matrix(, nrow = h, ncol = 11)
  k <- 0
  for (j in nrow:(nrow + h)) {
    k <- k + 1
    if (res == "daily") {
      horizon.m[k,] <- onehotyear[month(train_start + days(j)), ]
    }
    if (res == "weekly") {
      horizon.m[k,] <- onehotyear[month(train_start + weeks(j)), ]
    }
    if (res == "monthly") {
      horizon.m[k,] <- onehotyear[month(train_start + months(j)), ]
    }
    if (res == "yearly") {
      horizon.m[k,] <- onehotyear[month(train_start + years(j)), ]
    }
  }
  # Seasonal naive
  fit <- snaive(train_ts)
  snaive <- as.numeric(forecast(fit, h = h)$mean)

  # MA5
  fit <- ma(train_ts, order = 5)
  ma5 <- as.numeric(forecast(fit, h = h)$mean)

  # MA7
  fit <- ma(train_ts, order = 7)
  ma7 <- as.numeric(forecast(fit, h = h)$mean)

  # MA9
  fit <- ma(train_ts, order = 9)
  ma9 <- as.numeric(forecast(fit, h = h)$mean)

  # M12
  fit <- ma(train_ts, order = 12)
  ma12 <- as.numeric(forecast(fit, h = h)$mean)

  # TBATS
  fit <- tbats(train_ts)
  tbats <- as.numeric(forecast(fit, h = h)$mean)

  # STLF
  fit <- stlf(train_ts)
  stlf <- as.numeric(forecast(fit, h = h)$mean)

  # ARIMAX
  fit <- auto.arima(train_ts, xreg = month.m)
  arimax <- as.numeric(forecast(fit, xreg = horizon.m, h = h)$mean)

  # DYNREG
  fit1 <- tslm(train_ts ~ trend + season)
  fcast1 <- forecast(fit1, h = h)

  fit2 <- auto.arima(fit1$residuals)
  fcast2 <- forecast(fit2, h = h)

  y <- as.numeric(fcast1$mean)
  x <- as.numeric(fcast2$mean)
  dynreg <- (x + y)

  # NN
  fit <- nnetar(train_ts)
  nn <- as.numeric(forecast(fit, h = h)$mean)

  # ETS
  fit <- ets(train_ts)
  ets <- as.numeric(forecast(fit, h = h)$mean)

  # STL
  fit <- stl(train_ts, s.window = "periodic", t.window = 7)
  stl <- as.numeric(forecast(fit, h = h)$mean)

  # If you want to use others, do so below

  # Create a forecast vector. Order follows acc_history columns
  fcasts <- data.frame(snaive = snaive, ma5 = ma5, ma7 = ma7, ma9 = ma9, ma12 = ma12,
                       ets = ets, stl = stl, tbats = tbats, nn = nn, arimax = arimax,
                       dynreg = dynreg, stlf = stlf) # If you have your own methods, add them here

  # Create AVG
  avg <- apply(fcasts, 1, mean)

  # Create WAVG, these lines are a bit silly :)
  if (selected_method$name == "wavg") {
    scaled_coef <- selected_method$coef / sum(selected_method$coef) # Scale all coefs so they sum to 1
    wavg <- sum(scaled_coef * fcasts[, selected_method$order])
  } else {
      fcast_history <- read.csv(path)
      fcast_history[, 1] <- as.Date(fcast_history[, 1])
      acc_history <- get_accuracies(fcast_history, series)

      wavg_obj <- wavg_selection(acc_history[, -1])
      scaled_coef <- wavg_obj$coef / sum(wavg_obj$coef)
      wavg <- sum(scaled_coef * fcasts[, selected_method$order])
    }

  # Compile everything together for saving
  fcasts$avg <- avg
  fcasts$wavg <- wavg
  # fcasts$yourmethod <- yourmethod

  # Save
  write.csv(fcasts, path, append = TRUE, row.names = FALSE, col.names = FALSE)

  # Then return as per selected method
  if (selected_method == "snaive") {
    return(snaive)
  }
  if (selected_method == "ma5") {
    return(ma5)
  }
  if (selected_method == "ma7") {
    return(ma7)
  }
  if (selected_method == "ma9") {
    return(ma9)
  }
  if (selected_method == "ma12") {
    return(ma12)
  }
  if (selected_method == "ets") {
    return(ets)
  }
  if (selected_method == "stl") {
    return(stl)
  }
  if (selected_method == "tbats") {
    return(tbats)
  }
  if (selected_method == "nn") {
    return(nn)
  }
  if (selected_method == "arimax") {
    return(arimax)
  }
  if (selected_method == "dynreg") {
    return(dynreg)
  }
  if (selected_method == "stlf") {
    return(stlf)
  }
  if (selected_method == "avg") {
    return(avg)
  }
  if (selected_method$name == "wavg") {
    return(wavg)
  }
}

draw_forecast <- function(forecast, series, res = "weekly", product = "RBC", palette = "colorblind") {
  # Plotting helper for displaying forecasts w/series
  #
  # ARGUMENTS
  # forecast: from get_forecast
  # series: time series of interest
  # res: temporal resolution of the series
  # product: RBC / O- / A+, etc. A character vector. For plot title.
  # palette: Plot colors. Defaults to a colorblind accessible palette.
  #
  # RETURN
  # Nothing

  if (palette == "colorblind") {
    # Colorblind palette
    # black, orange, sky blue, green,
    # yellow, blue, vermilion, purple
    cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  } else {
      # Create your own if accessibility is not an issue :)
      custom_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    }

  if (res == "daily") {
    daterange <- seq.Date(from = series[1, 1], to = series[nrow(series), 1] + days(dim(forecast)[1]), by = "day")
  }
  if (res == "weekly") {
    daterange <- seq.Date(from = series[1, 1], to = series[nrow(series), 1] + weeks(dim(forecast)[1]), by = "weekly")
  }
  if (res == "monthly") {
    daterange <- seq.Date(from = series[1, 1], to = series[nrow(series), 1] + months(dim(forecast)[1]), by = "month")
  }
  if (res == "yearly") {
    daterange <- seq.Date(from = series[1, 1], to = series[nrow(series), 1] + years(dim(forecast)[1]), by = "year")
  }

  ggplot(data = series, aes(x = date, y = pcs)) +
    geom_line() +
    geom_line(aes(x = tail(daterange, dim(forecast)[1]), y = forecast)) +
    geom_segment(x = series[nrow(series), 1], xend = daterange[nrow(daterange) - dim(forecast)[1]],
                 y = series[nrow(series), 2], yend = head(forecast, 1),
                 linetype = "-") +
    scale_colour_manual(values = cbPalette) +
    theme_minimal() +
    labs(title = paste(product, res, "forecast"),
         subtitle = paste("Forecast horizon:", dim(forecast)[1]),
         xlab = "Date",
         ylab = "Units")
}
