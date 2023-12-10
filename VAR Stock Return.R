
######VAR with stock return############
#######################################

library(rugarch)
library(quantmod)
library(urca)
library(stats)
library(vars)
library(ggplot2)
library(tseries)
library(lmtest)
library(zoo)
library(haven)
library(car)

rm(list=ls())

# Define the stock symbols
symbols <- c("NVDA", "AMD", "GOOGL", "AMZN")

# Define the date range
start_date <- "2018-10-08"
end_date <- "2023-10-05"

# Use getSymbols to fetch the stock price data
getSymbols(symbols, from = start_date, to = end_date)

# Extract the adjusted closing prices (Adj.Close)
nvd_price <- Cl(get("NVDA"))
amd_price <- Ad(get("AMD"))
googl_price <- Ad(get("GOOGL"))
amzn_price <- Ad(get("AMZN"))

#daily return
return_nvd <- dailyReturn(nvd_price)
return_amd <- dailyReturn(amd_price)
return_googl <- dailyReturn(googl_price)
return_amzn <- dailyReturn(amzn_price)

# Perform the ADF test for Nvidia's first difference data
adf_nvd <- adf.test(return_nvd, alternative = "stationary", k = 0)

# Print the ADF test results
adf_nvd

# Perform the ADF test for AMD's first difference data
adf_amd <- adf.test(return_amd, alternative = "stationary", k = 0)

# Print the ADF test results
adf_amd

# Perform the ADF test for Google's first difference data
adf_googl <- adf.test(return_googl, alternative = "stationary", k = 0)

# Print the ADF test results
adf_googl

# Perform the ADF test for Amazon's first difference data
adf_amzn <- adf.test(return_amzn, alternative = "stationary", k = 0)

# Print the ADF test results
adf_amzn

diff_amzn <- diff(return_amzn, lag=1)
diff_amzn <- na.locf(diff_amzn, na.rm = TRUE,
                     fromLast = TRUE) ###remember to deal with the N/A

diff_nvd <- diff(return_nvd, lag=1)
diff_nvd <- na.locf(diff_nvd, na.rm = TRUE,
                     fromLast = TRUE) ###remember to deal with the N/A

diff_amd <- diff(return_amd, lag=1)
diff_amd <- na.locf(diff_amd, na.rm = TRUE,
                     fromLast = TRUE) ###remember to deal with the N/A
diff_googl <- diff(return_googl, lag=1)
diff_googl <- na.locf(diff_googl, na.rm = TRUE,
                     fromLast = TRUE) ###remember to deal with the N/A

# Perform the ADF test for Nvidia's first difference data
adf_nvd <- adf.test(diff_nvd, alternative = "stationary", k = 0)

# Print the ADF test results
adf_nvd

# Perform the ADF test for AMD's first difference data
adf_amd <- adf.test(diff_amd, alternative = "stationary", k = 0)

# Print the ADF test results
adf_amd

# Perform the ADF test for Google's first difference data
adf_googl <- adf.test(diff_googl, alternative = "stationary", k = 0)

# Print the ADF test results
adf_googl

# Perform the ADF test for Amazon's first difference data
adf_amzn <- adf.test(diff_amzn, alternative = "stationary", k = 0)

# Print the ADF test results
adf_amzn


par(mfrow = c(2, 2))  # Create a 2x2 grid for subplots
plot(return_nvd, main = "Nvidia stock return data chart")
plot(return_amd, main = "AMD stock return data chart")
plot(return_googl, main = "Google stock return data chart")
plot(return_amzn, main = "Amazon stock return data chart")


###spurious regression?

# Run multiple linear regression
LRmodel <- lm(return_nvd ~  return_amd + return_googl + return_amzn)

# Summary of the regression model
summary(LRmodel)

# Durbin-Watson test for residuals

durbinWatsonTest(LRmodel)


######### Cointegration should be in the last

# Combine the first difference series into a data frame
data_matrix <- data.frame(return_nvd, return_amd, return_googl, return_googl)
data_matrix <- data.frame(return_nvd, return_googl, return_googl)

data_matrix <- data.frame(
  return_nvd = tail(return_nvd, -1), 
  return_amd = tail(return_amd, -1),
  return_googl = tail(return_googl, -1),
  return_amzn = tail(return_amzn, -1)
)

data_matrix <- data.frame(
  return_nvd = tail(return_nvd, -1), 
  return_googl = tail(return_googl, -1),
  return_amzn = tail(return_amzn, -1)
)

# Perform the Johansen test
johansen_result <- ca.jo(data_matrix, type = "trace", ecdet = "none", K = 2)

summary(johansen_result)

# Perform Johansen test with eigenvalue test
johansen_result <- ca.jo(data_matrix, type = "eigen", ecdet = "none", K = 3)

# Extract test results and p-values
test_results <- summary(johansen_result)
p_values <- test_results@teststat[, "p.value"]

# Display test results and p-values
print(test_results)
print(p_values)

######### split

# Set the number of observations for the test set
n_obs <- 250

# Split the data frame into training and test sets
train <- data_matrix[1:(nrow(data_matrix) - n_obs), ]
test <- data_matrix[(nrow(data_matrix) - n_obs + 1):nrow(data_matrix), ]

test



# Determine the optimal lag order
var_order2 <- VARselect(train, lag.max = 10, type = "none")

var_order2

# Estimate the VAR model
estimated_var_return <- VAR(train, p = var_order2$selection[1], type = "none")

estimated_var_return

###############Granger Causality
# Example: Granger causality test from "diff_amd" to all other variables
granger_test_nvd_to_others <- causality(estimated_var_return)



causality(estimated_var_return, cause = "daily.returns")
causality(estimated_var_return, cause = "daily.returns.1")
causality(estimated_var_return, cause = "daily.returns.2")
causality(estimated_var_return, cause = "daily.returns.3")

# Perform Granger causality test for return_nvd causing return_amd
causality(estimated_var_return, cause = "daily.returns", p = 1, type = "const", test = "Granger", series = "daily.returns.1")


############variance decomposition

# Estimate the VAR model (replace with your VAR model)
estimated_var_return <- VAR(data_matrix, p = var_order2$selection[1], type = "none")

# Perform the variance decomposition
variance_decomposition <- irf(estimated_var_return, impulse = "daily.returns", response = c("daily.returns.1", "daily.returns.2", "daily.returns.3"), n.ahead = 10)

# Print the variance decomposition results
print(variance_decomposition)
plot(variance_decomposition)

# Perform the variance decomposition
variance_decomposition2 <- irf(estimated_var_return, impulse = "daily.returns.1", response = c("daily.returns", "daily.returns.2", "daily.returns.3"), n.ahead = 10)

# Print the variance decomposition results
plot(variance_decomposition2)

# Perform the variance decomposition
variance_decomposition3 <- irf(estimated_var_return, impulse = "daily.returns.2", response = c("daily.returns", "daily.returns.1", "daily.returns.3"), n.ahead = 10)

# Print the variance decomposition results
plot(variance_decomposition3)

# Perform the variance decomposition
variance_decomposition4 <- irf(estimated_var_return, impulse = "daily.returns.3", response = c("daily.returns", "daily.returns.1", "daily.returns.2"), n.ahead = 10)

# Print the variance decomposition results
plot(variance_decomposition4)

#########residual

# Extract residuals from the VAR model for the first equation (return_nvd)
residuals_nvd <- residuals(estimated_var_return)[, "daily.returns"]

# Create a time index for the residuals
time_index <- seq(1, length(residuals_nvd))

# Plot the residuals for return_nvd
plot(time_index, residuals_nvd, type = "l", col = "blue", xlab = "Time", ylab = "Residuals", main = "Residual Plot for return_nvd")

# Load the required library
library(forecast)

# Create ACF and PACF plots for the residuals
par(mfrow = c(2, 1))  # Create a 2x2 grid for subplots

# ACF plot
acf(residuals_nvd, main = "ACF of Residuals")

# PACF plot
pacf(residuals_nvd, main = "PACF of Residuals")

# Ljung-Box statistics for testing serial correlation
# Set the maximum lag value
max_lag <- 10

# Create a data frame to store the results
ljung_box_results <- data.frame(Lag = 1:max_lag, Q_Statistic = numeric(max_lag), P_Value = numeric(max_lag))

# Perform Ljung-Box statistics for different lags
for (lag in 1:max_lag) {
  ljung_box <- Box.test(residuals_nvd, type = "Ljung-Box", lag = lag)
  ljung_box_results$Q_Statistic[lag] <- ljung_box$statistic
  ljung_box_results$P_Value[lag] <- ljung_box$p.value
}

# Print the table of Ljung-Box statistics
print(ljung_box_results)

regression_data <- data.frame(
  return_nvd = return_nvd,
  return_amd = return_amd,
  return_googl = return_googl,
  return_amzn = return_amzn
)

# Run the regression
regression_model <- lm(return_nvd ~ return_amd + return_googl + return_amzn, data = regression_data)

# Print the summary of the regression
summary(regression_model)

library(lmtest)

# Extract residuals from the regression model
residuals_regression <- residuals(regression_model)

# Perform Durbin-Watson test
durbin_watson_test <- durbinWatsonTest(residuals_regression)

# Print the Durbin-Watson test statistic and p-value
cat("Durbin-Watson Test Statistic:", durbin_watson_test$statistic, "\n")
cat("P-Value:", durbin_watson_test$p.value, "\n")


# Forecast


# Forecast using the VAR model
forecasted_values <- predict(estimated_var_return, n.ahead = n_obs)
forecasted_values

# Extract the forecasted values for return_nvd
forecasted_return_nvd <- forecasted_values$fcst$daily.returns
forecasted_return_nvd

# Compare the forecasted values with the actual values from the test data
test_return_nvd <- test$daily.returns

test_return_nvd

# Calculate the accuracy measure (e.g., Mean Squared Error)
mse <- mean((forecasted_return_nvd - actual_return_nvd)^2)
mse
rmse <-sqrt(mse)
rmse

# Print the accuracy measure
cat("Mean Squared Error (MSE) on Test Data:", mse, "\n")

# Create a data frame for the forecasted and actual values
forecast_table <- data.frame(Forecasted = forecasted_return_nvd, Actual = test_return_nvd)

# Print the table of forecasted and actual values
print(forecast_table)


forecasted_return_nvd <- forecasted_values$fcst$daily.returns
forecasted_return_nvd

# Extract actual values for return_nvd from the test set
actual_return_nvd <- test$daily.returns
actual_return_nvd

# Create a data frame for comparison
comparison_df <- data.frame(
  Forecasted = forecasted_return_nvd,
  Actual = actual_return_nvd
)



################ Likelihood ratio test to compare the predictive power
# Fit restricted (simple) VAR model
var_model_restricted <- VAR(train[, c("daily.returns", "daily.returns.1")], p = 2)

# Fit unrestricted (complex) VAR model
var_model_unrestricted <- VAR(train[, c("daily.returns", "daily.returns.1", "daily.returns.2","daily.returns.3")], p = 2)

# Compute log-likelihood for both models
log_likelihood_restricted <- logLik(var_model_restricted)
log_likelihood_unrestricted <- logLik(var_model_unrestricted)

# Calculate likelihood ratio test statistic
lrt_statistic <- -2 * (log_likelihood_restricted - log_likelihood_unrestricted)

# Compare with chi-square distribution critical value
degrees_of_freedom <- 2  # Difference in parameters between restricted and unrestricted model
critical_value <- qchisq(0.95, df = degrees_of_freedom)  # Use desired significance level

# Check if the LRT statistic exceeds the critical value
if (lrt_statistic > critical_value) {
  cat("Unrestricted model is significantly better than the restricted model.\n")
} else {
  cat("No significant evidence that the unrestricted model is better.\n")
}




