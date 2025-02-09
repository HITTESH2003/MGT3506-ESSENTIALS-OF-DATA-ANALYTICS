library(e1071) 
library(caret)  
library(ggplot2) 
library(readr) 

data <- read_csv('/Users/hitteshkumarm/Desktop/COLLEGE/8th sem/ESSENTIALS OF DATA ANALYTICS/LAB/LAB 3/Customer_Behaviour.csv')

data$Purchased <- as.factor(data$Purchased)  
data$Gender <- as.factor(data$Gender)

print(sum(is.na(data)))  # If any missing values exist, handle them

#Split the data into training (70%) and test (30%) sets
set.seed(123)  
trainIndex <- createDataPartition(data$Purchased, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

#Training the Naïve Bayes classifier
naive_model <- naiveBayes(Purchased ~ Gender + Age + EstimatedSalary, data = trainData)

#Make predictions on the test set
predictions <- predict(naive_model, testData)

#Evaluate performance using confusion matrix
conf_matrix <- confusionMatrix(predictions, testData$Purchased)
print(conf_matrix)

#Visualization - Decision Boundary using Age vs. Estimated Salary
ggplot(data, aes(x = Age, y = EstimatedSalary, color = Purchased)) +
  geom_point(size = 3) +
  labs(title = "Age vs Estimated Salary - Purchase Decision",
       x = "Age", y = "Estimated Salary") +
  theme_minimal()

#Visualization - Bar plot of Purchase Distribution
ggplot(data, aes(x = Purchased, fill = Purchased)) +
  geom_bar() +
  labs(title = "Purchase Distribution", x = "Purchased (0 = No, 1 = Yes)", y = "Count") +
  theme_minimal()
