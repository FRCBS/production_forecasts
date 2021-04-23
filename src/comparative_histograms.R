library(tidyverse)
library(viridis)

frcbs <- read_csv("/home/esa/production_forecasts/histories/long_fcast_history_FI.csv", col_types = "Df-----")
sanquin <- read_csv("/home/esa/production_forecasts/histories/long_fcast_history_NL.csv",  col_types = "Df-----")

frcbs_cut <- frcbs %>%
  filter(time >= as.Date("2012-12-01") & time <= as.Date("2020-11-01"))

data <- data.frame(date = frcbs_cut$time,
                   year = format(frcbs_cut$time, "%Y"),
                   frcbs = frcbs_cut$model,
                   sanquin = sanquin$model) %>%
  mutate(frcbs = fct_relevel(frcbs, "12-MA", "9-MA", "5-MA", "DYNREG", "SNAIVE", "STL",
                                    "ETS", "ARIMAX", "COMBINED", "STLF", "TBATS", "NN"),
         sanquin = fct_relevel(sanquin, "12-MA", "DYNREG", "SNAIVE", "ETS", "ARIMAX", 
                                        "COMBINED", "STLF", "TBATS", "NN"))

# Shave off first entry (single datapoint for 2007) for yearly analysis
data <- data[-1, ]

# Group by year
grouped_data <- data %>%
  group_by(year)

ggplot(data = grouped_data, aes(x = sanquin, color = sanquin, fill = sanquin)) + 
  geom_histogram(stat = "count") + 
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~year) +
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  labs(x = "")

ggplot(data = grouped_data, aes(x = frcbs, color = frcbs, fill = frcbs)) + 
  geom_histogram(stat = "count") + 
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~year) +
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  labs(x = "")

# How about without cutting the FRCBS data?
data_uncut <- data.frame(date = frcbs$time,
                   year = format(frcbs$time, "%Y"),
                   frcbs = frcbs$model) %>%
  mutate(frcbs = fct_relevel(frcbs, "12-MA", "9-MA", "5-MA", "DYNREG", "SNAIVE", "STL",
                             "ETS", "ARIMAX", "COMBINED", "STLF", "TBATS", "NN"))

# Shave off first entry (single datapoint for 2007) for yearly analysis
data_uncut <- data_uncut[-1, ]

# Group by year
grouped_data_u <- data_uncut %>%
  group_by(year)

# Plot
ggplot(data = grouped_data_u, aes(x = frcbs, color = frcbs, fill = frcbs)) + 
  geom_histogram(stat = "count") + 
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~year) +
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  labs(x = "")
