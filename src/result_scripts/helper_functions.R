## ---------------------------
## Script name: helper_functions.R
##
## Purpose of script: Does as it says on the tin.
##                    Various functions needed for running
##                    create_synthetic_demand.R and
##                    compare_performance.R
##
## Author: Esa Turkulainen, M.Sc.
##
## Date Created: 2021-07-23
##
## License: GNU General Public License v3.0
## Email: esa.turkulainen@veripalvelu.fi
##
## ---------------------------
## Notes: This script is made public as
##        a part of a publication for
##        transparency and reproducibility
##        purposes.
##
## ---------------------------

daily_to_weekly <- function(daily) {
    # Aggregate FROM daily TO weekly
    pcslist <- c(); weeklist <- c(); datelist <- c()  # Create storage lists
    j <- 0; k <- 0  # j is for resetting week counter, k is for gathering dates
    for (i in seq(to = (nrow(series) - 7), by = 7)) {
        if (j == 52) {j <- 0}
        j <- j + 1
        k <- k + 1
        pcslist[k] <- sum(series$pcs[i:(i + 6)])
        weeklist[k] <- j
        datelist[k] <- series$date[i]

    weekly <- data.frame(week = weeklist,
                        date = as.Date.numeric(datelist, origin = "1970-01-01"),
                        pcs = pcslist)

    return(weekly)
    }
}

ensure.window.parity <- function(x, y) {
  # Assumes date column is first
  if (x[1, 1] > y[1, 1]) start <- x[1, 1] else start <- y[1, 1]
  if (x[nrow(x), 1] < y[nrow(y), 1]) end <- x[nrow(x), 1] else end <- y[nrow(y), 1]
  x <- x[x[, 1] >= start & x[, 1] <= end, ]
  y <- y[y[, 1] >= start & y[, 1] <= end, ]

  return(list(first = x, second = y))
}

simulate_forecast <- function(fcast, apes, val){
    # Selects methods and forecasts based on minimum MAPE across val observations.
    selected_method <- c()
    forecast <- c()
    error <- c()
    date <- c()
    for (i in 1:(nrow(apes) - val)) {
        # Select method
        meanerr <- colMeans(apes[i:(i + val - 1), 1:13]) # 1:(number of methods)
        selected_method[i] <- names(apes[, 1:13])[which.min(meanerr)]
        # Fetch forecast of selected method at i + val (- 1 + 1)
        forecast[i] <- fcast[i + val, selected_method[i]]
        error[i] <- apes[i + val, selected_method[i]]
        date[i] <- fcast$date[i + val]
    }
    df <- data.frame(date = as.Date(date, origin = "1970-01-01"), method = selected_method, forecast = forecast, error = error)
    return(df)
}

# One row solutions for compiling final results
# These also ensure proper shape. Columns indicate val.lengths
# And rows are methods.
val_errors_indv <- function(apes, val) {
    indv.mapes <- colMeans(apes[-(1:val), ])
    min.mape <- mean(min.errors[-(1:val)])
    wavg.mape <- mean(wavg.errors[-(1:val)])

    return(c(min.mape, indv.mapes, wavg.mape))
}

val_errors_auto <- function(auto, val.self, val.levels) {
    # This also takes care of the NA padding
    res <- c()
    N <- val.levels[!(val.levels < val.self)]
    nNA <- sum(val.levels < val.self)
    for (i in 1:length(N)) {
        res[i] <- mean(auto$error[-(1:N[i])])
    }
    return(c(rep(NA, nNA), res))
}
