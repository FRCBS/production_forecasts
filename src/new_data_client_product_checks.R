# Format a table for Laura

library(data.table)
library(dplyr)
library(lubridate)
library(readr)

## Read, define columns, omit last column
data <- fread("/home/esa/production_forecasts/data/DWSALES/2021-03-16.dat",
              colClasses = c("Date", "numeric", "factor", "character", "character", "factor"))
codes <- c("budTR001", "A0071V00", "A0074V00", "A0092VA0", "A0092VB0", 
           "E3844V00", "E3845V00", "E3846VA0", "E3846VB0", "E3846VC0",
           "E3846V00", "E3847VA0", "E3847VB0", "E3847VC0", "E3847V00",
           "E3936VA0", "E3936VB0", "E3939V00", "E3940V00", "E4683V00",
           "E7668V00", "E7673V00", "E4999V00", "E5000V00")
data <- data %>%
  filter(V4 %in% codes)

# Filter out internal clients
nointernal <- data %>%
  filter(V4 %in% codes) %>%
  filter(as.integer(as.character(V6)) > 999)

# Aggregate
nointernalm <- aggregate(V2 ~ month(V1) + year(V1), data = nointernal, FUN = sum) %>%
  select("month" = `month(V1)`, "year" = `year(V1)`, V2) %>%
  transmute(V2, date = ymd(paste(year, month, 5)))

# Monthly aggregates by clients
n <- length(unique(data$V6))
namevec <- unique(data$V6)
df <- as.data.frame(matrix(rnorm(11 * n), ncol = n)); colnames(df) <- namevec
seq <- seq.Date(from = min(data$V1), to = max(data$V1), by = "months")
for(i in 1:n){
  temp <- filter(data, data$V6 == unique(data$V6)[i])
  client_aggr <- aggregate(V2 ~ month(V1) + year(V1), data = temp, FUN = sum) %>%
    select("month" = `month(V1)`, "year" = `year(V1)`, V2) %>%
    transmute(V2, date = ymd(paste(year, month, 5)))
  imputed <- merge(x = data.frame(date = seq),
                   y = client_aggr,
                   all.x = TRUE)
  df[, i] <- imputed$V2
}

# Monthly aggregates by products
n <- length(unique(data$V4))
namevec <- unique(data$V4)
df2 <- as.data.frame(matrix(rnorm(11 * n), ncol = n)); colnames(df2) <- namevec
seq <- seq.Date(from = min(data$V1), to = max(data$V1), by = "months")
for(i in 1:n){
  temp <- filter(data, data$V4 == unique(data$V4)[i])
  product_aggr <- aggregate(V2 ~ month(V1) + year(V1), data = temp, FUN = sum) %>%
    select("month" = `month(V1)`, "year" = `year(V1)`, V2) %>%
    transmute(V2, date = ymd(paste(year, month, 5)))
  imputed <- merge(x = data.frame(date = seq),
                   y = product_aggr,
                   all.x = TRUE)
  df2[, i] <- imputed$V2
}

# Combine and add date and truth columns
new <- read_csv("/home/esa/production_forecasts/data/new_monthly.csv")
final <- cbind(new, df2, df)

# Check that the rowsums match
final$pcs[1] == sum(final[1, 15:dim(final)[2]], na.rm = T) # TRUE for clients
final$pcs[1] == sum(final[1, 3:14], na.rm = T) # TRUE for products

# Write into a CSV
write_csv(final, "/home/esa/production_forecasts/data/client_product_sums_for_Laura.csv")

