rm(list=ls())
library(glmnet)

library(readxl)
getwd()
Nvidia <- read_xlsx("ECON515_Dataset.xlsx")

set.seed(123) 
train_indices <- sample(1:nrow(Nvidia), 0.8 * nrow(Nvidia))  # 80% for training, adjust as needed
train_data <- Nvidia[train_indices, ]
test_data <- Nvidia[-train_indices, ]

X_train <- as.matrix(train_data[, c("Volume","S&P500Index","EPS","GoldPrice")])
Y_train <- train_data$ClosePrice

X_test <- as.matrix(test_data[, c("Volume","S&P500Index","EPS","GoldPrice")])
Y_test <- test_data$ClosePrice

lasso_model <- glmnet(X_train, Y_train, alpha=1)  # Alpha=1 for Lasso

cvfit <- cv.glmnet(X_train, Y_train, alpha=1)
plot(cvfit)

best_lambda <- cvfit$lambda.min
lasso_model <- glmnet(X_train, Y_train, alpha=1, lambda=best_lambda)

predictions <- predict(lasso_model, s=best_lambda, newx=X_test)
Y_pred <- as.vector(predictions)

install.packages("ggplot2")
library(ggplot2)
plot_data <- data.frame(Actual = Y_test, Predicted = Y_pred)
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  labs(x = "Actual Stock Price", y = "Predicted Stock Price") +
  ggtitle("Actual vs. Predicted Stock Prices")

# model evaluation
mse <- mean((Y_test - Y_pred)^2)
rmse <- sqrt(mse)
rmse

rsquared <- 1 - mse / var(Y_test)
rsquared
