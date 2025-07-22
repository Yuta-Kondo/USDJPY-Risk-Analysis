# -----------------------------------------------------------------------------
# Final R Script for the Analysis of USD/JPY Exchange Rate Volatility
# -----------------------------------------------------------------------------

# 1. SETUP: Load necessary libraries
# -----------------------------------------------------------------------------
# The 'tseries' package is needed for GARCH modeling and tests.
# The 'knitr' package is used for creating a clean table of coefficients.
library(tseries)
library(knitr)


# 2. DATA PREPARATION: Load and process the data
# -----------------------------------------------------------------------------
# Load the daily USD/JPY exchange rate data from the CSV file.
# The file is expected to have columns: DATE and DEXJPUS.
jpy_data <- read.csv("DEXJPUS.csv", header = TRUE, na.strings = ".")

# Remove rows with missing values, which correspond to non-trading days.
jpy_data <- na.omit(jpy_data)

# Convert the price column into a time series object.
price <- ts(jpy_data$DEXJPUS)

# Calculate log returns to get a stationary series.
# Multiply by 100 to express as a percentage.
returns <- diff(log(price)) * 100


# 3. EXPLORATORY ANALYSIS & PRE-MODELING TESTS
# -----------------------------------------------------------------------------
# This section generates the plot and test result for the "Data and Stylized Facts" section.

# Create the plot of log returns (Figure 1 in the report).
plot(returns, main = "USD/JPY Log Returns", ylab = "Return (%)",
     xlab = "Time")

# Formal Ljung-Box test for ARCH effects on the squared returns.
# A small p-value confirms volatility clustering.
Box.test(returns^2, lag = 12, type = "Ljung-Box")


# 4. GARCH MODELING: Estimation and Diagnostics
# -----------------------------------------------------------------------------
# This section fits the GARCH(1,1) model and prepares outputs for the report.

# Specify and estimate the GARCH(1,1) model.
garch_model <- garch(returns, order = c(1, 1))

# Display the estimated coefficients in a clean table (Table 1 in the report).
knitr::kable(
  summary(garch_model)$coef,
  col.names = c("Estimate", "Std. Error", "t value", "Pr(>|t|)"),
  caption = "GARCH(1,1) Model Coefficients"
)

# Extract standardized residuals for diagnostic checking.
std_residuals <- residuals(garch_model)
std_residuals <- na.omit(std_residuals)

# Create the ACF plot of the squared standardized residuals (Figure 2 in the report).
# This confirms that the model has captured the volatility clustering.
acf(std_residuals^2, main = "ACF of Squared Standardized Residuals")

# Perform the final Ljung-Box test on the squared standardized residuals.
# A large p-value indicates the model is a good fit.
Box.test(std_residuals^2, lag = 12, type = "Ljung-Box")


# 5. FINAL VISUALIZATION: Plotting Volatility
# -----------------------------------------------------------------------------
# This section generates the final plot showing returns and estimated volatility.

# The fitted() function returns a time series of conditional standard deviations.
cond_volatility <- fitted(garch_model)

# Use ts.plot() to correctly align and plot the returns and volatility bands.
ts.plot(returns, cond_volatility, -cond_volatility,
        main = "USD/JPY Returns and Conditional Volatility",
        ylab = "Value",
        gpars = list(col = c("grey", "red", "red")))

# Add a legend to the plot.
legend("topright", legend = c("Returns", "Conditional SD"),
       col = c("grey", "red"), lty = 1, bty = "n")

