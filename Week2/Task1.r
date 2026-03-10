#Q1 a
# Histogram and boxplot of the blood pressure and serum cholesterol levels

# Loaad dataset
data <- read.csv("heart.csv", header = TRUE)
head(data)
# Histogram of blood pressure
hist(data$trestbps, main = "Histogram of Blood Pressure",
     xlab = "Blood Pressure (mm Hg)", col = "lightblue", border = "black")
# Boxplot of blood pressure
boxplot(data$trestbps, main = "Boxplot of Blood Pressure",
        ylab = "Blood Pressure (mm Hg)", col = "lightblue", border = "black")

# Observations:
# The histogram of blood pressure shows a right-skewed distribution,
# with most values clustered around 120 mm Hg,
# and a few higher values extending up to around 200 mm Hg.
# The boxplot confirms this skewness, with a longer upper whisker and
# several outliers above the upper quartile, indicating that there are
# some individuals with significantly higher blood pressure levels in
# the dataset.
# Histogram of serum cholesterol levels
hist(data$chol, main = "Histogram of Serum Cholesterol Levels",
     xlab = "Serum Cholesterol (mg/dl)", col = "lightgreen", border = "black")
# Boxplot of serum cholesterol levels
boxplot(data$chol, main = "Boxplot of Serum Cholesterol Levels",
        ylab = "Serum Cholesterol(mg/dl)", col = "lightgreen", border = "black")
#Observations:
# The histogram of serum cholesterol levels also shows a right-skewed distribution, with most values
# clustered around 200 mg/dl, and a few higher values extending up to around 400 mg/dl.
# The boxplot confirms this skewness, with a longer upper whisker and several outliers
# above the upper quartile, indicating that there are some individuals with significantly higher serum 
# cholesterol levels in the dataset.

# Q1 b
# 5 number summary of the blood pressure
summary(data$trestbps)

#Q1 c
"What summary statistics will you use to summarise the center and the spread of
the distribution of blood pressure? Why?"

# I would use the median to summarize the center of the distribution of 
# blood pressure,because the median is less affected by outliers 
# and skewed data compared to the mean.
# For the spread of the distribution, I would use the interquartile range (IQR), 
# which is the difference between the third quartile (Q3) and the first quartile (Q1). 
# The IQR is also less affected by outliers and provides a
# better measure of variability for skewed distributions compared to the standard deviation.

# Q2 a
# Scatter plot of age and blood pressure
plot(data$age, data$trestbps, main = "Scatter Plot of Age vs Blood Pressure",
     xlab = "Age (years)", ylab = "Blood Pressure (mm Hg)",
     col = "blue", pch = 16)
# Association between age and blood pressure
# The scatter plot of age and blood pressure shows a positive association,
# where blood pressure tends to increase with age.
# As age increases, there is a general upward trend in blood pressure values,
# indicating that older individuals in the dataset tend to have higher
# blood pressure levels compared to younger individuals

# Q2 b
# Find the correlation coefficient and coefficient of determination between 
# age and blood pressure. Explain the correlation between those variables
correlation <- cor(data$age, data$trestbps) # Range of correlation coefficient is -1 to 1
r_squared <- correlation^2 # Range of R-squared is 0 to 1
correlation
r_squared
# The correlation coefficient between age and blood pressure is 0.27,
# which indicates a weak positive correlation between the two variables.
# The coefficient of determination (R-squared) is 0.07, which means
# that only 7% of the variability in blood pressure can be explained by age.
# This suggests that while there is a positive association between age and blood pressure,
# age is not a strong predictor of blood pressure, and other
# factors may play a more significant
# role in determining blood pressure levels in the dataset.

# Q2 c Fit a linear regression model for the above two variables 
# (taking age as the x, i.e.,independent variable). 
#Plot the regression line on the scatter plot.
# Fit linear regression model
model <- lm(trestbps ~ age, data = data)
summary(model)
# Add regression line to scatter plot
abline(model, col = "red", lwd = 2)
#' The linear regression model shows that the slope coefficient for age is 0.5,
#' which means that for each additional year of age, blood pressure
#' increases by an average of 0.5 mm Hg. However, the R-squared value of 0.07
#' indicates that the model explains only a small portion of the variability
#' in blood pressure, suggesting that other factors may be more important
#' in determining blood pressure levels in the dataset.

# Q3 a
# Classify bp and chol into 2 categories (high and low) 
# using the following cut-off points:
# bp: high if trestbps >= 120, else low
# chol: high if chol >= 200, else low
data$bp_category <- ifelse(data$trestbps > 120, "high", "low")
data$chol_category <- ifelse(data$chol > 200, "high", "low")
# View the first few rows of the updated dataset
head(data)
# Contigency table of bp and chol categories
table(data$bp_category, data$chol_category,
    dnn = c("Blood Pressure", "Cholesterol"))

# Q3 b
# 1)The probability that the person has both conditions high bp and high chol
prob_both_high <- sum(data$bp_category == "high" & 
                        data$chol_category == "high") / nrow(data)
prob_both_high

# 2)The probability that the person has high bp
prob_high_bp <- sum(data$bp_category == "high") / nrow(data)
prob_high_bp
# 3)The probability that the person with high bp has high cholesterol
prob_high_chol_given_high_bp <- sum(data$bp_category == "high" &
                                      data$chol_category == "high") /
  sum(data$bp_category == "high")
prob_high_chol_given_high_bp
# 4)The probability that the person has high blood pressure if it is known
# that he/she has high cholesterol
prob_high_bp_given_high_chol <- sum(data$bp_category == "high" & 
                                       data$chol_category == "high") /
  sum(data$chol_category == "high")
prob_high_bp_given_high_chol

# Summary of Results
cat("\n=== Q3 b: Probability Results ===\n")
cat("1) P(High BP AND High Chol):", round(prob_both_high, 4), "\n")
cat("2) P(High BP):", round(prob_high_bp, 4), "\n")
cat("3) P(High Chol | High BP):", round(prob_high_chol_given_high_bp, 4), "\n")
cat("4) P(High BP | High Chol):", round(prob_high_bp_given_high_chol, 4), "\n")

# Q3 c
# Are high blood pressure and high cholesterol disjoint? Explain
"
# Two events are disjoint if they cannot occur at the same time.
# In this case, it is possible for a person to have both high blood 
# pressure and high cholesterol,so the events are not disjoint.
"
# d) Are high blood pressure and high cholesterol independent? Explain
"
Two events A and B are independent if the occurrence of one event
does not affect the probability of the other event occurring.

In this case, we can check for independence by comparing the
probability of having high blood pressure given high cholesterol
(P(High BP | High Chol)) with the overall probability of having
high blood pressure (P(High BP)).

If P(High BP | High Chol) is equal to P(High BP)*P(High Chol), then the events
are independent.

In our calculations, we found that P(High BP | High Chol) is not
equal to P(High BP), which suggests that high blood pressure and
high cholesterol are not independent events. This means that the
presence of high cholesterol may increase the likelihood of having
high blood pressure, indicating a potential association between the
two conditions in the dataset.
"

# Q3 e
"
Consider that two people are chosen at random in a sequence to form a group.
i. what is the probability that they both have high blood pressure?
ii. what is the probability that only the second one has high blood pressure?
iii. what is the probability that at least one of them has high blood pressure?
"
# i. Probability that both have high blood pressure
prob_both_high_bp <- prob_high_bp * prob_high_bp
prob_both_high_bp
# ii. Probability that only the second one has high blood pressure
prob_only_second_high_bp <- (1 - prob_high_bp) * prob_high_bp
# First one does not have high bp and second one has high bp
prob_only_second_high_bp
# iii. Probability that at least one of them has high blood pressure
prob_at_least_one_high_bp <- 1 - (1 - prob_high_bp) * (1 - prob_high_bp)
# This is equivalent to 1 - P(neither has high blood pressure)
prob_at_least_one_high_bp
