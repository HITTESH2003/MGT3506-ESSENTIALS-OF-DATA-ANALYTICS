library(ggplot2)

data_path <- "/Users/hitteshkumarm/Desktop/COLLEGE/8th sem/ESSENTIALS OF DATA ANALYTICS/LAB/LAB 3/Advertising.csv"
data <- read.csv(data_path)

head(data)

scaled_data <- data
scaled_data$TV <- scale(data$TV)
scaled_data$Sales <- scale(data$Sales)

set.seed(42)
train_indices <- sample(1:nrow(scaled_data), 0.8 * nrow(scaled_data))
train_data <- scaled_data[train_indices, ]
test_data <- scaled_data[-train_indices, ]

lm_model <- lm(Sales ~ TV, data = train_data)
summary(lm_model)

pred_lm <- predict(lm_model, test_data)

learning_rate <- 0.005
iterations <- 10000
m <- 0
b <- 0

cost_function <- function(x, y, m, b) {
  return(mean((y - (m * x + b))^2))
}

gradient_descent <- function(x, y, m, b, learning_rate, iterations) {
  n <- length(y)
  cost_history <- numeric(iterations)
  
  for (i in 1:iterations) {
    m_grad <- -(2/n) * sum(x * (y - (m * x + b)))
    b_grad <- -(2/n) * sum(y - (m * x + b))
    
    m <- m - learning_rate * m_grad
    b <- b - learning_rate * b_grad
    
    cost_history[i] <- cost_function(x, y, m, b)
  }
  
  list(m = m, b = b, cost_history = cost_history)
}

gd_result <- gradient_descent(train_data$TV, train_data$Sales, m, b, learning_rate, iterations)

gd_m <- gd_result$m
gd_b <- gd_result$b

pred_gd <- gd_m * test_data$TV + gd_b

rsq_lm <- 1 - sum((test_data$Sales - pred_lm)^2) / sum((test_data$Sales - mean(test_data$Sales))^2)
rsq_gd <- 1 - sum((test_data$Sales - pred_gd)^2) / sum((test_data$Sales - mean(test_data$Sales))^2)

mae_lm <- mean(abs(test_data$Sales - pred_lm))
mse_lm <- mean((test_data$Sales - pred_lm)^2)
rmse_lm <- sqrt(mse_lm)

mae_gd <- mean(abs(test_data$Sales - pred_gd))
mse_gd <- mean((test_data$Sales - pred_gd)^2)
rmse_gd <- sqrt(mse_gd)

cat("R-Squared from OLS Linear Regression on Test Data: ", rsq_lm, "\n")
cat("R-Squared from Gradient Descent Linear Regression on Test Data: ", rsq_gd, "\n")
cat("Final model parameters from OLS: Intercept =", lm_model$coefficients[1], ", Slope =", lm_model$coefficients[2], "\n")
cat("Final model parameters from Gradient Descent: Intercept =", gd_b, ", Slope =", gd_m, "\n")

cat("\nOLS Model Metrics on Test Data:\n")
cat("MAE: ", mae_lm, "\n")
cat("MSE: ", mse_lm, "\n")
cat("RMSE: ", rmse_lm, "\n")

cat("\nGradient Descent Model Metrics on Test Data:\n")
cat("MAE: ", mae_gd, "\n")
cat("MSE: ", mse_gd, "\n")
cat("RMSE: ", rmse_gd, "\n")

ggplot(test_data, aes(x = TV, y = Sales)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE, linetype = "solid") +
  geom_abline(slope = gd_m, intercept = gd_b, color = "red", linetype = "dashed") +
  ggtitle("Comparison: OLS vs Gradient Descent Linear Regression (Test Data)") +
  theme_minimal()
