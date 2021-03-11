# Load
longform <- read.csv("/home/esa/production_forecasts/data/monthly_real_2004.csv", 
                     colClasses = c("Date", "numeric", "NULL"))
normform <- read.csv("/home/esa/production_forecasts/data/monthly_real_2014.csv",
                     colClasses = c("Date", "NULL", "NULL", "numeric"))

# Set window
longform <- longform[longform$date >= "2014-01-01", ]
normform <- normform[normform$date <= "2020-07-01", ]

# Diff
difframe <- data.frame(date = longform$date,
                       diff = abs(longform$red - normform$red.sales))

# Plot
library(ggplot2)
ggplot(difframe, aes(x = date)) + 
  geom_line(aes(y = diff)) +
  labs(title = "Absolute differences in datasets from the same period",
       subtitle = "4k unit spike likely affects fits") +
  theme_minimal()
