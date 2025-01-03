library(tidyverse)
library(reshape2)
library(caTools)
library(Metrics)

# Step 1: Load the dataset
data <- read.csv("/Users/hitteshkumarm/Desktop/COLLEGE/8th sem/ESSENTIALS OF DATA ANALYTICS/LAB/LAB2/GoldUP.csv")

# Step 2: Explore the dataset
head(data)  # Preview the first few rows
summary(data)  # Summary statistics
str(data)  # Structure of the dataset

# Step 3: Data Cleaning and Transformation
# Convert 'Date' to Date format and ensure all columns are numeric where needed
data$Date <- as.Date(data$Date, format = "%d-%m-%Y")
data <- data %>%
  mutate(across(-Date, as.numeric))

# Drop 'Date' column for correlation analysis
data <- data %>% select(-Date)

# Step 4: Compute the correlation matrix
cor_matrix <- cor(data, use = "complete.obs") 

# Step 5: Melt the correlation matrix into long format for ggplot2
cor_melt <- melt(cor_matrix)

# Step 6: Create the heatmap
ggplot(data = cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables")

data_filtered <- data %>%
  select(Gold_Price, CPI) %>%
  drop_na() 

set.seed(123) 
split <- sample.split(data_filtered$CPI, SplitRatio = 0.8)

train_data <- subset(data_filtered, split == TRUE)
test_data <- subset(data_filtered, split == FALSE)

cat("Training Data Rows:", nrow(train_data), "\n")
cat("Testing Data Rows:", nrow(test_data), "\n")

#Training the Model
model <- lm(CPI ~ Gold_Price, data = train_data)  
summary(model)

# Predict on the test dataset
predictions <- predict(model, newdata = test_data)

# Calculate the number of correct predictions
correct_predictions <- sum(predicted_classes == test_data$CPI_Class)
total_predictions <- length(predicted_classes)
# Model accuracy calculation
accuracy <- correct_predictions / total_predictions
# Print model accuracy
cat("Model Accuracy:", round(accuracy * 100, 2), "%\n")



# Calculate evaluation metrics
mae_value <- mae(test_data$CPI, predictions)  # Mean Absolute Error
mse_value <- mse(test_data$CPI, predictions)  # Mean Squared Error
r_squared <- cor(test_data$CPI, predictions)^2  # R-squared value

# Display the results
cat("Mean Absolute Error (MAE):", mae_value, "\n")
cat("Mean Squared Error (MSE):", mse_value, "\n")
cat("R-squared:", r_squared, "\n")

# Scatter plot of actual vs predicted
ggplot(data = test_data, aes(x = CPI, y = predictions)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted CPI",
       x = "Actual CPI",
       y = "Predicted CPI") +
  theme_minimal()


# Categorize CPI into 'High' and 'Low' based on median value
threshold <- median(data_filtered$CPI)
train_data$CPI_Class <- ifelse(train_data$CPI > threshold, "High", "Low")
test_data$CPI_Class <- ifelse(test_data$CPI > threshold, "High", "Low")

# Categorize predictions based on the same threshold
predicted_classes <- ifelse(predictions > threshold, "High", "Low")
library(caret)

# Create a confusion matrix
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$CPI_Class))

# Print the confusion matrix
print(confusion_matrix)

# Convert confusion matrix to a data frame
conf_matrix_df <- as.data.frame.table(confusion_matrix$table)

ggplot(conf_matrix_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix",
       x = "Predicted Class",
       y = "Actual Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


