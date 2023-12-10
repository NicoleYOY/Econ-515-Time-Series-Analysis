######VECM ############
#######################################

install.packages("tsDyn")
library(tsDyn)
library(vars)
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
library(urca)
library(forecast)
library(tidyverse)

rm(list=ls())

###Load the dataset
library(readxl)
Nvidia <- read_xlsx("/Users/ouyangyingrun/Desktop/ECON515_Dataset.xlsx")
Nvidia$Date <- as.Date(Nvidia$Date)
Nvidia_ts <- zoo(Nvidia[, -1], order.by = Nvidia$Date) #convert dataset to time series format

###Set as time series 
price_nvd <- ts(Nvidia$`Close Price`, start = 1, frequency = 1)
vol_nvd <- ts(Nvidia$Volume, start = 1, frequency = 1)
sp500 <- ts(Nvidia$`S&P 500 index`, start = 1, frequency = 1)
price_gold <- ts(Nvidia$`Gold Price`, start = 1, frequency = 1)

# Perform the ADF test for Nvidia's first difference data
adf_nvdp <- adf.test(price_nvd, alternative = "stationary", k = 0)

# Print the ADF test results
adf_nvdp

# Perform the ADF test for AMD's first difference data
adf_vol <- adf.test(vol_nvd, alternative = "stationary", k = 0)

# Print the ADF test results
adf_vol

# Perform the ADF test for Google's first difference data
adf_sp500 <- adf.test(sp500, alternative = "stationary", k = 0)

# Print the ADF test results
adf_sp500

# Perform the ADF test for Amazon's first difference data
adf_gold <- adf.test(price_gold, alternative = "stationary", k = 0)

# Print the ADF test results
adf_gold

##1st diff

diff_nvd <- diff(price_nvd, lag=1)
diff_nvd <- na.locf(diff_nvd, na.rm = TRUE,
                     fromLast = TRUE) ###remember to deal with the N/A

diff_vol <- diff(vol_nvd, lag=1)
diff_vol <- na.locf(diff_vol, na.rm = TRUE,
                    fromLast = TRUE) ###remember to deal with the N/A

diff_sp <- diff(sp500, lag=1)
diff_sp <- na.locf(diff_sp, na.rm = TRUE,
                    fromLast = TRUE) ###remember to deal with the N/A

diff_gold <- diff(price_gold, lag=1)
diff_gold <- na.locf(diff_gold, na.rm = TRUE,
                      fromLast = TRUE) ###remember to deal with the N/A

# Perform the ADF test for Nvidia's first difference data
adf_nvdp <- adf.test(diff_nvd, alternative = "stationary", k = 0)

# Print the ADF test results
adf_nvdp

# Perform the ADF test for AMD's first difference data
adf_vol <- adf.test(diff_vol, alternative = "stationary", k = 0)

# Print the ADF test results
adf_vol

# Perform the ADF test for Google's first difference data
adf_sp500 <- adf.test(diff_sp, alternative = "stationary", k = 0)

# Print the ADF test results
adf_sp500

# Perform the ADF test for Amazon's first difference data
adf_gold <- adf.test(diff_gold, alternative = "stationary", k = 0)

# Print the ADF test results
adf_gold


######### creat data frame and split
# Combine the first difference series into a data frame
data_matrix <- data.frame(price_nvd, vol_nvd, sp500, price_gold)


# Set the number of observations for the test set
n_obs <- 250

# Split the data frame into training and test sets
train <- data_matrix[1:(nrow(data_matrix) - n_obs), ]
test <- data_matrix[(nrow(data_matrix) - n_obs + 1):nrow(data_matrix), ]

test

##Creat a system
##dset <- cbind(price_nvd,vol_nvd,sp500,price_gold)

# Create plots using ggplot2
p1 <- ggplot(Nvidia, aes(x = Date, y = `Close Price`)) +
  geom_line() +
  labs(title = "Nvidia stock data chart", x = "Date", y = "Close Price")


p2 <- ggplot(Nvidia, aes(x = Date, y = Volume)) +
  geom_line() +
  labs(title = "Nvidia volume data chart", x = "Date", y = "Volume")

p3 <- ggplot(Nvidia, aes(x = Date, y = `S&P 500 index`)) +
  geom_line() +
  labs(title = "S&P 500 index data chart", x = "Date", y = "S&P 500 index")

p4 <- ggplot(Nvidia, aes(x = Date, y = `Gold Price`)) +
  geom_line() +
  labs(title = "Gold price data chart", x = "Date", y = "Gold Price")

p1
p2
p3
p4

########################################
#JOHANSEN COINTEGRATION in R
########################################

#Selecting the Optimal Number of Lags (Recall, this is p - 1)

lagselect <- VARselect(train, lag.max = 7, type = "both")
lagselect$selection
lagselect$criteria
#Since 3 came up the most, we use (3-1) or 2 lags

ctest1t <- ca.jo(train, type = "trace", ecdet = "const", K = 2)
summary(ctest1t)

ctest1e <- ca.jo(train, type = "eigen", ecdet = "const", K = 2)
summary(ctest1e)

#Hence, we have one cointegrating relationship in this model

######################################################################

#Build the VECM Model
var1 <- VAR(train, p=2, type="both")
var1
Model1 <- VECM(train, 2, r = 3, estim =("2OLS"))
summary(Model1)

#Diagnostic Tests

#Need to Transform VECM to VAR

Model1VAR <- vec2var(ctest1t, r = 1)

#Serial Correlation

Serial1 <- serial.test(Model1VAR, lags.pt = 5, type = "PT.asymptotic")
Serial1

#ARCH Effects

Arch1 <- arch.test(Model1VAR, lags.multi = 15, multivariate.only = TRUE)
Arch1

#Normality of Residuals

Norm1 <- normality.test(Model1VAR, multivariate.only = TRUE)
Norm1

#Impulse Response Functions

volirf <- irf(Model1VAR, impulse = "vol_nvd", response = "price_nvd", n.ahead = 20, boot = TRUE)
plot(volirf, ylab = "price_nvd", main = "Stock volume's shock to Nvidia stock price")

sp500irf <- irf(Model1VAR, impulse = "sp500", response = "price_nvd", n.ahead = 20, boot = TRUE)
plot(sp500irf, ylab = "price_nvd", main = "SP500's shock to Nvidia stock price")

goldirf <- irf(Model1VAR, impulse = "price_gold", response = "price_nvd", n.ahead = 20, boot = TRUE)
plot(goldirf, ylab = "price_nvd", main = "Gold price's shock to Nvidia stock price")


#Variance Decomposition

FEVD1 <- fevd(Model1VAR, n.ahead = 10)
plot(FEVD1)

# Generate FEVD
VarianceDecomposition <- fevd(Model1VAR, n.ahead = 10)

# Reduce margins for the plot
par(mar = c(4, 4, 2, 2))  # Adjust the margin values as needed

# Plot FEVD
plot(VarianceDecomposition)

# Forecast using the VAR model
forecasted_values <- predict(Model1, n.ahead = n_obs)
forecasted_values <- data.frame(forecasted_values)
forecasted_values

library(ggplot2)

library(ggplot2)
# Plotting actual vs forecasted values
plot(price_nvd, type = "l", col = "blue", xlab = "Time", ylab = "Price", main = "Actual vs Forecasted Price")
lines((length(price_nvd) - n_obs + 1):length(price_nvd), forecasted_values$price_nvd, type = "l", col = "red")
legend("topright", legend = c("Actual", "Forecasted"), col = c("blue", "red"), lty = 1)

# Adding vertical lines to differentiate train and test sets
abline(v = length(price_nvd) - n_obs, col = "green")  # Vertical line between train and test



# Calculate the accuracy measure (e.g., Mean Squared Error)
mse <- mean((forecasted_values$price_nvd - test$price_nvd)^2)
mse
rmse <-sqrt(mse)
rmse



