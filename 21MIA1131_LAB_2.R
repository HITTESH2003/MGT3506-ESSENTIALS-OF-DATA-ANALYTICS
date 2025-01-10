library(dplyr)

data <- read.csv("/Users/hitteshkumarm/Desktop/COLLEGE/8th sem/ESSENTIALS OF DATA ANALYTICS/LAB/LAB 2/HR_comma_sep.csv")

# H1: One-Way ANOVA for Satisfaction Level by Salary
anova_h1 <- aov(satisfaction_level ~ salary, data = data)
cat("H1: One-Way ANOVA for Satisfaction Level by Salary\n")
summary(anova_h1)

# H2: Two-Way ANOVA for Satisfaction Level by Salary and Promotion
anova_h2 <- aov(satisfaction_level ~ salary * promotion_last_5years, data = data)
cat("\nH2: Two-Way ANOVA for Satisfaction Level by Salary and Promotion\n")
summary(anova_h2)

# H3: One-Way ANOVA for Average Monthly Hours by Number of Projects
data$number_project_category <- cut(
  data$number_project, 
  breaks = c(0, 2, 4, 6, 8, Inf), 
  labels = c("1-2", "3-4", "5-6", "7-8", "9+")
)
anova_h3 <- aov(average_montly_hours ~ number_project_category, data = data)
cat("\nH3: One-Way ANOVA for Average Monthly Hours by Number of Projects\n")
summary(anova_h3)

# H4: One-Way ANOVA for Satisfaction Level by Promotion History
data$promotion_category <- as.factor(data$promotion_last_5years)
anova_h4 <- aov(satisfaction_level ~ promotion_category, data = data)
cat("\nH4: One-Way ANOVA for Satisfaction Level by Promotion History\n")
summary(anova_h4)

# H5: One-Way ANOVA for Time Spent by Salary
anova_h5 <- aov(time_spend_company ~ salary, data = data)
cat("\nH5: One-Way ANOVA for Time Spent by Salary\n")
summary(anova_h5)

