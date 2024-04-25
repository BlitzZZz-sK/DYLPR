  print_specified_text <- function() {
    cat("---
title: \"EDA_LAB3_20MIA1008\"
output: html_document
date: \"2024-01-18\"
---


X <- c(2, 3, 4, 5, 6)
Y <- c(12, 17, 23, 28, 32)

# Calculate the means of X and Y
mean_X <- mean(X)
mean_Y <- mean(Y)

# Calculate the slope (a) and intercept (b)
a <- sum((X - mean_X) * (Y - mean_Y)) / sum((X - mean_X)^2)
b <- mean_Y - a * mean_X

# Print the coefficients
cat(\"The best linear fit is Y =\", a, \"X +\", b)



# Plot the data points
plot(X, Y, main = \"Linear Regression\", xlab = \"X\", ylab = \"Y\")

# Add the best fit line
abline(a = a, b = b, col = \"red\")




# Calculate the predicted values
predicted_Y <- a * X + b

# Calculate the residuals
residuals <- Y - predicted_Y

# Calculate the RSS
RSS <- sum(residuals^2)

# Print the RSS
cat(\"The RSS is\", RSS)




# Calculate the standard errors
se_a <- sqrt(RSS / (length(X) - 2) / sum((X - mean_X)^2))
se_b <- se_a * sqrt(1/length(X) + mean_X^2 / sum((X - mean_X)^2))

# Print the standard errors
cat(\"The standard error for a is\", se_a, \"and for b is\", se_b)





# Calculate the total sum of squares (TSS)
TSS <- sum((Y - mean(Y))^2)

# Calculate the explained sum of squares (ESS)
ESS <- sum((predicted_Y - mean(Y))^2)

# Calculate the R-squared value
RSquared <- ESS / TSS

# Calculate the adjusted R-squared value
n <- length(X)
p <- 2 # Number of predictors (a and b)
Adjusted_RSquared <- 1 - (1 - RSquared) * (n - 1) / (n - p - 1)



# Print the R-squared and adjusted R-squared values
cat(\"The R-squared value is\", RSquared, \"and the adjusted R-squared value is\", Adjusted_RSquared)

# Compute F-Statistic
f_statistic <- (RSquared / (1 - RSquared)) * ((n - 2) / 1)

cat(\" and F-Statistic:\", round(f_statistic, 3), \"\\n\")




# Perform hypothesis test
p_value <- 2 * (1 - pt(abs(a / se_a), df = length(X) - 2))

# Print the p-value
cat(\"The p-value for the hypothesis test is\", p_value, \"\\n\")

# Check significance level (commonly 0.05)
alpha <- 0.05

# Draw inferences
if (p_value < alpha) {
 cat(\"Reject the null hypothesis. There is a significant linear relationship between X and Y.\\n\")
} else {
 cat(\"Fail to reject the null hypothesis. There is no significant linear relationship between X and Y.\\n\")
}





#2. Apply linear regression analysis on any prominent dataset and state the inferences.
# importing dataset with the name HousingData
library(readr)
dataset <- read_csv(\"C:/Users/skp68/OneDrive/Documents/Datasets/HousingData.csv\")
View(dataset)







head(dataset)
summary(dataset)



missing_values <- sum(is.na(dataset))
cat(\"Missing values: \", missing_values)

colSums(is.na(dataset))



for (variable in colnames(dataset))
{
dataset[[variable]][is.na(dataset[[variable]])] <- median(dataset[[variable]],
                                                        na.rm = TRUE)
}

new_missing_values <- sum(is.na(dataset))
cat(\"Missing values after imputation: \", new_missing_values)



options(repos = \"https://cloud.r-project.org/\")



install.packages(\"ggplot2\")
install.packages(\"caret\")
# Plotting Data
library(ggplot2)
# Data splitting into training and test set
library(caret)



ggplot(dataset, aes(x = AGE, y = MEDV)) +
geom_point(color = \"blue\") +
labs(x = \"AGE\", y = \"MEDV (Median Home Value)\") +
ggtitle(\"Scatterplot of AGE vs. MEDV\")




library(ggcorrplot)
#Using ggcorrplot to get correlation between features
ggcorrplot(cor(dataset),hc.order = TRUE, lab = TRUE)




set.seed(123)

train_percentage <- 0.8

trainIndex <- createDataPartition(dataset$MEDV, p = train_percentage)

# Convert trainIndex to a numeric vector
trainIndex <- unlist(trainIndex)

training_data <- dataset[trainIndex, ]
testing_data <- dataset[-trainIndex, ]

\"Training data:\\n\"
head(training_data)
\"Test data:\\n\"
head(testing_data)





lm_model <- lm(MEDV ~ LSTAT + RM, data = training_data)




# Predicting on the test set
predictions <- predict(lm_model, newdata = testing_data)

# getting true value of test data
actual_values <- testing_data$MEDV # Actual values from the test set

# Calculating Mean Absolute Error (MAE)
mae <- mean(abs(predictions - actual_values))
cat(\"Mean Absolute Error (MAE):\", mae, \"\\n\")

# Calculating Mean Squared Error (MSE)
mse <- mean((predictions - actual_values)^2)
cat(\"Mean Squared Error (MSE):\", mse, \"\\n\")

# Calculating Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
cat(\"Root Mean Squared Error (RMSE):\", rmse, \"\\n\")

summary(lm_model)
")
    cat("# Define the function F(X)\n",
    "F <- function(x1, x2) {\n",
    "  return((4*x1)^2 + 3*x1*x2 + 2.5*(x2)^2 - 5.5*x1 - 4*x2)\n",
    "}\n\n",
    "# Define the gradient of F(X) with respect to x1 and x2\n",
    "grad_F <- function(x1, x2) {\n",
    "  df_dx1 <- 2 * (4*x1) * (4) + 3*x2 - 5.5\n",
    "  df_dx2 <- 3*x1 + 2.5 * 2 * x2 - 4\n",
    "  return(c(df_dx1, df_dx2))\n",
    "}\n\n",
    "# Gradient descent function\n",
    "gradient_descent <- function(gradient, initial_x, learning_rate, iterations) {\n",
    "  x <- initial_x\n",
    "\n",
    "  for (i in 1:iterations) {\n",
    "    grad <- gradient(x[1], x[2])\n",
    "    x <- x - learning_rate * grad\n",
    "  }\n",
    "\n",
    "  return(x)\n",
    "}\n\n",
    "# Initial guess for x1 and x2\n",
    "initial_guess <- c(0, 0)\n\n",
    "# Learning rate\n",
    "learning_rate <- 0.01\n\n",
    "# Number of iterations\n",
    "iterations <- 1000\n\n",
    "# Perform gradient descent\n",
    "minimum <- gradient_descent(grad_F, initial_guess, learning_rate, iterations)\n\n",
    "# Output the minimum point\n",
    "cat('Minimum point (x1, x2):', minimum, '\\n')\n",
    "cat('Minimum value of F(X):', F(minimum[1], minimum[2]), '\\n')\n")
}

#' Perform Analysis
#'
#' This function calculates RMSE and MAPE for SMA and WMA, and forecasts using an ARIMA model.
#'
#' @param sales A numeric vector of sales data.
#' @export
gredient <- function() {
cat("# Define the function F(X)\n",
    "F <- function(x1, x2) {\n",
    "  return((4*x1)^2 + 3*x1*x2 + 2.5*(x2)^2 - 5.5*x1 - 4*x2)\n",
    "}\n\n",
    "# Define the gradient of F(X) with respect to x1 and x2\n",
    "grad_F <- function(x1, x2) {\n",
    "  df_dx1 <- 2 * (4*x1) * (4) + 3*x2 - 5.5\n",
    "  df_dx2 <- 3*x1 + 2.5 * 2 * x2 - 4\n",
    "  return(c(df_dx1, df_dx2))\n",
    "}\n\n",
    "# Gradient descent function\n",
    "gradient_descent <- function(gradient, initial_x, learning_rate, iterations) {\n",
    "  x <- initial_x\n",
    "\n",
    "  for (i in 1:iterations) {\n",
    "    grad <- gradient(x[1], x[2])\n",
    "    x <- x - learning_rate * grad\n",
    "  }\n",
    "\n",
    "  return(x)\n",
    "}\n\n",
    "# Initial guess for x1 and x2\n",
    "initial_guess <- c(0, 0)\n\n",
    "# Learning rate\n",
    "learning_rate <- 0.01\n\n",
    "# Number of iterations\n",
    "iterations <- 1000\n\n",
    "# Perform gradient descent\n",
    "minimum <- gradient_descent(grad_F, initial_guess, learning_rate, iterations)\n\n",
    "# Output the minimum point\n",
    "cat('Minimum point (x1, x2):', minimum, '\\n')\n",
    "cat('Minimum value of F(X):', F(minimum[1], minimum[2]), '\\n')\n")

#' Perform Naive Bayes Analysis
#'
#' This function performs Naive Bayes analysis on the specified dataset.
#'
#' @param dataset A data frame containing the dataset to analyze.
#' @param target_col The name of the target column in the dataset.
#' @export
naive_bayes <- function() {
 # Load required libraries
 library(caret)
 library(readr)
  
 # Split the data
 set.seed(123)
 splitIndex <- createDataPartition(dataset[[target_col]], p = 0.7, list = FALSE)
 train_data <- dataset[splitIndex, ]
 test_data <- dataset[-splitIndex, ]
  
 # Train the model
 model <- train(target_col ~ ., data = train_data, method = "naive_bayes")
  
 # Predict the values
 predictions <- predict(model, newdata = test_data)
  
 # Convert predictions and test data to character
 predictions <- as.character(predictions)
 test_data[[target_col]] <- as.character(test_data[[target_col]])
  
 # Manually set factor levels to be the same
 levels_to_use <- union(predictions, test_data[[target_col]])
 predictions <- factor(predictions, levels = levels_to_use)
 test_data[[target_col]] <- factor(test_data[[target_col]], levels = levels_to_use)
  
 # Confusion Matrix
 conf_matrix <- confusionMatrix(predictions, test_data[[target_col]])
  
 # Accuracy Assessment
 accuracy <- conf_matrix$overall["Accuracy"]
 print(conf_matrix)
 print(paste("Accuracy:", accuracy))
}


#' Perform Naive Bayes Analysis with Hardcoded Parameters
#'
#' This function performs Naive Bayes analysis on the specified dataset with hardcoded parameters.
#'
#' @param dataset_path The path to the dataset CSV file.
#' @param target_col The name of the target column in the dataset.
#' @export
naive_bayes_hardcoded <- function() {
 # Load required libraries
 library(caret)
 library(e1071)
  
 # Reading dataset
 data <- read.csv(dataset_path) 
  
 data[[target_col]] <- factor(data[[target_col]])
  
 # Split the data into training and testing sets
 set.seed(123) # Setting seed for reproducibility
 trainIndex <- createDataPartition(data[[target_col]], p = 0.8, 
                                    list = FALSE, 
                                    times = 1)
 train_data <- data[trainIndex, ]
 test_data <- data[-trainIndex, ]
  
 # Train the model using Naive Bayes classifier on the training data
 naive_bayes_model <- naiveBayes(target_col ~ ., data = train_data)
  
 # Predict the values on the test set with explicitly set levels
 test_predictions <- predict(naive_bayes_model, test_data)
 test_predictions <- factor(test_predictions, levels = levels(test_data[[target_col]]))
  
 # Confusion Matrix for test data
 test_confusion_matrix <- confusionMatrix(test_predictions, test_data[[target_col]])
  
 # Accuracy Assessment for test data
 test_accuracy <- test_confusion_matrix$overall["Accuracy"]
 print(paste("Test Accuracy: ", round(test_accuracy, 2) * 100, "%"))
}

#' Perform Decision Tree Analysis
#'
#' This function performs Decision Tree analysis on the specified dataset.
#'
#' @param dataset_path The path to the dataset CSV file.
#' @param target_col The name of the target column in the dataset.
#' @export
decision_tree <- function(dataset_path, target_col) {
 # Load required libraries
 library(rpart)
 library(rpart.plot)
 library(caret)
 library(readr)
  
 # Reading dataset
 data <- read_csv(dataset_path) 
  
 # Convert categorical variables to factors
 data[[target_col]] <- as.factor(data[[target_col]])
  
 # Split the data
 set.seed(123) # Setting seed for reproducibility
 splitIndex <- createDataPartition(data[[target_col]], p = 0.8, list = FALSE)
 train_data <- data[splitIndex, ]
 test_data <- data[-splitIndex, ]
  
 # Train the Decision Tree model
 decision_tree_model <- rpart(target_col ~ ., data = train_data, method = "class")
  
 # Plot the tree
 rpart.plot(decision_tree_model)
  
 # Predict values
 predictions <- predict(decision_tree_model, newdata = test_data, type = "class")
  
 # Accuracy assessment
 accuracy <- sum(predictions == test_data[[target_col]]) / nrow(test_data)
 print(paste("Accuracy:", round(accuracy, 2)))
}

