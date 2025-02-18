# Load Required Libraries
library(quantmod)      
library(dplyr)         
library(lubridate)     
library(e1071)         
library(caret)         
library(ggplot2)       
library(zoo)          

# 📌 Define Stock Symbol & Fetch Data
stock_symbol <- "SUNPHARMA.NS"  # Ensure correct stock ticker
start_date <- Sys.Date() - 365
end_date <- Sys.Date()

# 📌 Fetch stock data using quantmod (alternative to tidyquant)
tryCatch({
  getSymbols(stock_symbol, from = start_date, to = end_date, src = "yahoo", auto.assign = TRUE)
}, error = function(e) {
  stop("Error: Unable to fetch stock data. Please check your internet connection and stock symbol.")
})

# Convert to Data Frame
stock_data <- get(stock_symbol) %>%
  as.data.frame() %>%
  mutate(date = index(get(stock_symbol)))

# Rename Columns for Simplicity
colnames(stock_data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Date")

# 📌 Feature Engineering
stock_data <- stock_data %>%
  arrange(Date) %>%
  mutate(
    Daily_Return = (Adjusted - lag(Adjusted)) / lag(Adjusted),  # Daily return
    MA_5 = rollmean(Adjusted, k = 5, fill = NA, align = "right"),
    MA_10 = rollmean(Adjusted, k = 10, fill = NA, align = "right"),
    Target = ifelse(lead(Adjusted) > Adjusted, "Up", "Down")  # Predict next day's movement
  ) %>%
  na.omit()  # Remove NA values

# Convert Target to Factor
stock_data$Target <- as.factor(stock_data$Target)

# 📌 Split Data into Training (80%) and Testing (20%)
set.seed(42)
train_indices <- createDataPartition(stock_data$Target, p = 0.8, list = FALSE)
train_data <- stock_data[train_indices, ]
test_data <- stock_data[-train_indices, ]

# 📌 Train Naïve Bayes Model
model <- naiveBayes(
  Target ~ Open + High + Low + Close + Volume + Daily_Return + MA_5 + MA_10, 
  data = train_data
)

# 📌 Make Predictions
predictions <- predict(model, test_data)

# Convert Predictions to Factor
predictions <- factor(predictions, levels = levels(test_data$Target))

# 📌 Evaluate Model Performance
conf_matrix <- confusionMatrix(predictions, test_data$Target)
print(conf_matrix)

# 📌 Plot Actual vs Predicted Trend
test_data$Predicted <- predictions

ggplot(test_data, aes(x = Date)) +
  geom_line(aes(y = Adjusted, color = "Actual Price")) +
  geom_point(aes(y = Adjusted, shape = Predicted, color = Predicted), size = 2) +
  scale_color_manual(values = c("Up" = "green", "Down" = "red", "Actual Price" = "blue")) +
  theme_minimal() +
  labs(
    title = paste("Stock Price Prediction (", stock_symbol, ")", sep = ""),
    x = "Date", y = "Adjusted Closing Price", color = "Legend"
  ) +
  theme(legend.position = "bottom")

