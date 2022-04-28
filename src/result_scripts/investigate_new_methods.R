# Set working directory and paths
setwd("~/prodfore_publ/")
LOADPATH <- paste0(getwd(), "/results/")
SAVEPATH <- paste0(getwd(), "/results/")

# Load helper functions and required packages
source(paste0(getwd(), "/src/helper_functions.R"))
library(ggplot2)
library(forecast)
library(lubridate)
library(nnfor)

# Load data
weekly <- read.csv(paste0(LOADPATH, "weekly_real.csv"), colClasses = c("Date", "numeric"))
weekly.sim <- read.csv(paste0(LOADPATH, "weekly_synthetic.csv"), colClasses = c("Date", "numeric"))

# Set (rolling) training window size. 105 weeks is the minimum for these methods.
rw <- 155

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
mlp <- c()
elm <- c()


fits <- c()
fcasts <- c()
run_start <- Sys.time()
for (i in 0:(nrow(weekly.sim) - rw - 1)) {
    train_start <- weekly.sim$date[1 + i]
    train <- subset(weekly.sim$pcs, weekly.sim$date >= train_start & weekly.sim$date <= train_start + weeks(rw - 1))

    test_date <- weekly.sim$date[rw + 1 + i]

    # Make train set into a ts object for forecast functions
    train_ts <- ts(train, start = c(year(train_start), month(train_start)), frequency = 52)

    # PRINTS
    print(paste("Iteration:", i))
    print(Sys.time() - run_start)

    # MLP
    fit <- elm(train_ts, reps = 1)
    fits[i + 1] <- fit
    plot(fit)
    fcast <- forecast(fit, h = 2)
    fcasts[i + 1] <- fcast
}
