# Create an EPS line graph with CMYK color profile for Transfusion
library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(viridisLite)

# Set paths
SERIESLOC <- "/home/esa/prodfore_publ/results/weekly_synthetic.csv"  # The demand history, "real"/"syn"
FCASTLOC <- "/home/esa/prodfore_publ/results/val12_fcasts_syn.csv"   # The forecast history, N month validation

# Load data
real <- read_csv(SERIESLOC, col_types = "Dd")
forecasts <- read_csv(FCASTLOC, col_types = "Dfdd")

# Start real at first forecast and omit last (and likely not full) month from both
startdate <- forecasts$date[1]
enddate <- forecasts$date[length(forecasts$date) - 1]
real_cut <- subset(real, date >= startdate & date <= enddate)
fcast_cut <- subset(forecasts, date >= startdate & date <= enddate)

# Refactor method names for unity in Transfusion graphics
# fcast_cut$method <- mapvalues(fcast_cut$method, c("ma12", "ma9", "ma7", "ma5", "snaive", "stl", "ets",
#                                                   "arimax", "dynreg", "combined", "stlf", "tbats", "nn"),
#                                                 c("12-MA", "9-MA", "7-MA", "5-MA", "SNAIVE", "STL", "ETS",
#                                                   "ARIMAX", "DYNREG", "AVG", "STLF", "TBATS", "NN"))


# Create dataframe for plotting
plot_df <- data.frame(date = real_cut$date,
                      real = real_cut$pcs,
                      fcast = fcast_cut$forecast,
                      method = fcast_cut$method) %>%
  mutate(method = fct_relevel(method, "MA12", "MA9", "MA7", "MA5", "SNAIVE", "ETS", "STL",
                              "STLF", "ARIMAX", "DYNREG", "TBATS", "MLP", "ELM", "AVG")) # We relevel for the vis.
                                                                                         # From simplest to most complex.
                                                                                         # Map yours accordingly.

# Create rects
rects <- data.frame(xstart = seq.Date(plot_df$date[1], plot_df$date[nrow(plot_df) - 1], by = "week"),
                    xend = seq.Date(plot_df$date[2], plot_df$date[nrow(plot_df)], by = "week"),
                    method = head(plot_df$method, -1))

# Open EPS device
postscript("/home/esa/prodfore_publ/results/colored_methods_syn.eps", width = 15, height = 4, horizontal = FALSE,
           onefile = FALSE, paper = "special", colormodel = "cmyk",
           family = "Helvetica")

# Plot
p2 <- ggplot() +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = method), alpha = 1) +
  geom_line(data = plot_df, aes(x = date, y = fcast), size = 1.2, color = "grey85") +
  geom_line(data = plot_df, aes(x = date, y = real), size = 1.2) +
  labs(x = "Time",
       y = "Units",
       fill = " ") +
  theme_minimal() +
  scale_x_date(date_breaks = "years", date_labels = "%Y")
p2

# Save
dev.off()
