# ======================
# 1) Libraries
# ======================
library(readxl)
library(quantmod) # For Yahoo Finance
library(urca)     # For the Johansen co-integration test (ca.jo) & Phillips-Perron test (pp.test)
library(tseries)  # For other unit root tests, if needed
library(tseries)  # For the Augmented Dickey-Fuller test (adf.test)

# ======================
# 2) Create the Data Frames
# ======================

# ------------------------------------------------------------
# 2.1) Mueller Industries Revenue
# ------------------------------------------------------------
file_path <- "Mueller Quarterly Revenue.xlsx"
rev_data <- read_excel(file_path)

# Convert and sort the Date column
rev_data$Date <- as.Date(rev_data$Date)
rev_data <- rev_data[order(rev_data$Date), ]

print(rev_data)
# ------------------------------------------------------------
# 2.2) Commodities Futures
# ------------------------------------------------------------

# 2.2.1) Get historical data from Yahoo Finance
symbols <- c("ALI=F", "HG=F")
start_date <- as.Date("2015-12-31")
end_date   <- as.Date("2024-12-31")

getSymbols(symbols, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

# Extract the closing prices as xts objects
aluminum_xts <- Cl(get("ALI=F"))
copper_xts   <- Cl(get("HG=F"))

# Merge both series into one 'xts' object
merged_data <- merge(aluminum_xts, copper_xts, all = FALSE)
colnames(merged_data) <- c("Aluminum", "Copper")

print(merged_data)


# 2.2.2) Create a sequence of quarter-end dates (2016 - 2024)
quarter_ends <- seq(
  from = as.Date("2015-12-31"),
  to   = as.Date("2024-12-31"),
  by   = "3 months"
)

print(quarter_ends)

# Create a data.frame to store quarterly data
quarterly_data <- data.frame(
  QuarterEnd = quarter_ends,
  ActualDate = as.Date(rep(NA, length(quarter_ends))),
  Aluminum   = rep(NA_real_, length(quarter_ends)),
  Copper     = rep(NA_real_, length(quarter_ends)),
  Revenue    = rep(NA_real_, length(quarter_ends))  # We'll fill this shortly
)

# ------------------------------------------------------------
# 2.3) Align Aluminum and Copper to quarter-ends
# ------------------------------------------------------------
for(i in seq_along(quarter_ends)) {
  q_date <- quarter_ends[i]
  # Which commodity dates are available before this quarter-end?
  idx <- which(index(merged_data) <= q_date)
  
  if(length(idx) > 0) {
    last_idx <- tail(idx, 1)
    # Actual date from commodity data
    actual_date <- index(merged_data)[last_idx]
    # Extract prices
    aluminum_price <- as.numeric(merged_data[last_idx, "Aluminum"])
    copper_price   <- as.numeric(merged_data[last_idx, "Copper"])
    
    # Fill in the commodity columns
    quarterly_data$ActualDate[i] <- actual_date
    quarterly_data$Aluminum[i]   <- aluminum_price
    quarterly_data$Copper[i]     <- copper_price
  }
}

# ------------------------------------------------------------
# 2.4) Align Revenue data to the same quarter-ends
# ------------------------------------------------------------
for(i in seq_along(quarter_ends)) {
  q_date <- quarter_ends[i]
  # Which revenue dates are available before this quarter-end?
  rev_idx <- which(rev_data$Date <= q_date)
  
  if(length(rev_idx) > 0) {
    last_rev_idx <- tail(rev_idx, 1)
    # Fill the quarter's revenue with the last available data
    quarterly_data$Revenue[i] <- rev_data$Price[last_rev_idx]
  }
}

# ======================
# 3) Plot the Data
# ======================

# ------------------------------------------------------------
# 3.1) Revenue Plot
# ------------------------------------------------------------
plot(
  rev_data$Date, rev_data$Price,
  type = "l", col = "blue", lwd = 2,
  xlab = "Date", ylab = "Revenue",
  main = "Revenue Over Time"
)
grid()
points(rev_data$Date, rev_data$Price, col = "blue", pch = 16)

# ------------------------------------------------------------
# 3.2) Quarterly Commodities Price Plot
# ------------------------------------------------------------
plot(
  quarterly_data$QuarterEnd,
  quarterly_data$Aluminum,
  type = "l",
  col  = "blue",
  lwd  = 2,
  xlab = "Quarter-End (Notional)",
  ylab = "Aluminum (USD)",
  main = "Quarter-End Aluminum vs. Copper Prices"
)
grid()

# Overlay Copper on the same chart, with axis on the right
par(new = TRUE)
plot(
  quarterly_data$QuarterEnd,
  quarterly_data$Copper,
  type = "l",
  col  = "red",
  lwd  = 2,
  axes = FALSE,
  xlab = "",
  ylab = ""
)
axis(side = 4, col.axis = "red", col = "red")
mtext("Copper (USD)", side = 4, line = 3, col = "red")

legend(
  "topleft",
  legend = c("Aluminum", "Copper"),
  col    = c("blue", "red"),
  lty    = 1,
  bty    = "n"
)
print(quarterly_data)


# ======================
# 4) Check for Stationarity
# ======================

# Convert data columns to time series objects
revenue_ts <- ts(quarterly_data$Revenue, start = c(2016, 1), frequency = 4)  # Quarterly data
aluminum_ts <- ts(quarterly_data$Aluminum, start = c(2016, 1), frequency = 4)
copper_ts <- ts(quarterly_data$Copper, start = c(2016, 1), frequency = 4)

# ------------------------------
# 4.1) Augmented Dickey-Fuller (ADF) Test
# ------------------------------
cat("Augmented Dickey-Fuller Test Results:\n")

adf_revenue <- adf.test(na.omit(revenue_ts))
adf_aluminum <- adf.test(na.omit(aluminum_ts))
adf_copper <- adf.test(na.omit(copper_ts))

print(adf_revenue)
print(adf_aluminum)
print(adf_copper)

# ------------------------------
# 4.2) Phillips-Perron (PP) Test
# ------------------------------
cat("Phillips-Perron Test Results:\n")

pp_revenue <- pp.test(na.omit(revenue_ts))
pp_aluminum <- pp.test(na.omit(aluminum_ts))
pp_copper <- pp.test(na.omit(copper_ts))

print(pp_revenue)
print(pp_aluminum)
print(pp_copper)

# Interpretation:
# - p-value < 0.05 → Stationary
# - p-value > 0.05 → Non-stationary.

# ======================
# 5) Make Data Stationary (Second Differencing)
# ======================

# Remove the NAs and make the data numeric 
revenue_clean <- as.numeric(na.omit(quarterly_data$Revenue))
aluminum_clean <- as.numeric(na.omit(quarterly_data$Aluminum))
copper_clean <- as.numeric(na.omit(quarterly_data$Copper))

# Convert cleaned data to time series
revenue_ts <- ts(revenue_clean, start = c(2016, 1), frequency = 4)
aluminum_ts <- ts(aluminum_clean, start = c(2016, 1), frequency = 4)
copper_ts <- ts(copper_clean, start = c(2016, 1), frequency = 4)

# Second differencing to make data stationary
revenue_diff <- diff(revenue_ts, differences = 2)
aluminum_diff <- diff(aluminum_ts, differences = 2)
copper_diff <- diff(copper_ts, differences = 2)

# Re-run the stationarity tests
cat("Augmented Dickey-Fuller Test on Differenced Data:\n")

adf_revenue_diff <- adf.test(revenue_diff)
adf_aluminum_diff <- adf.test(aluminum_diff)
adf_copper_diff <- adf.test(copper_diff)

print(adf_revenue_diff)
print(adf_aluminum_diff)
print(adf_copper_diff)

cat("Phillips-Perron Test on Differenced Data:\n")

pp_revenue_diff <- pp.test(revenue_diff)
pp_aluminum_diff <- pp.test(aluminum_diff)
pp_copper_diff <- pp.test(copper_diff)

print(pp_revenue_diff)
print(pp_aluminum_diff)
print(pp_copper_diff)


# ======================
# 6) Johansen Cointegration Test
# ======================

# Combine the second-differenced data into a matrix
data_matrix <- cbind(revenue_diff, aluminum_diff, copper_diff)

# Remove any NA values 
data_matrix <- na.omit(data_matrix)

# Run the Johansen test
johansen_test <- ca.jo(data_matrix, type = "trace", ecdet = "none", K = 2)

# Print test summary
summary(johansen_test)

# Interpretation:
# - If the test statistic exceeds the critical value at 5%, 
#   reject the null hypothesis and conclude that a cointegrating relationship exists.
