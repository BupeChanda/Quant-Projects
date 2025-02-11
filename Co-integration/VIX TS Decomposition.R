library(tidyquant)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forecast)
library(zoo)  # For missing value imputation

# Define the ticker symbol for VIX
symbol <- "^VIX"

# Download data for the required date range
vix_data <- tq_get(symbol, from = "2020-03-27", to = "2025-02-10")

# Handle missing values by linear interpolation
vix_data$close <- na.approx(vix_data$close, na.rm = FALSE)

# Remove any remaining NA values after interpolation
vix_data <- vix_data %>% drop_na()

# Plot the VIX time series
ggplot(vix_data, aes(x = date, y = close)) +
  geom_line(color = "blue") +
  labs(title = "VIX Index from 2020 to 2025", 
       x = "Date", 
       y = "Closing Price") +
  theme_minimal()

# Convert data to time series format (252 trading days per year)
vix_ts <- ts(vix_data$close, start = c(2020, 3), frequency = 252)

# Perform STL decomposition with a shorter trend window
# Set your desired window length (e.g., 30 trading days)
cma_window <- 30  
vix_decomp <- stl(vix_ts, s.window = "periodic", t.window = cma_window)

# Plot the decomposition
plot(vix_decomp)
