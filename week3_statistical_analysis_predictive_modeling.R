# Week 3: Statistical Analysis and Predictive Modeling using R
# Dataset: built-in mtcars
# Model: Multiple Linear Regression
# Uses base R only; no additional packages required.

set.seed(42)
data(mtcars)

# 1. Explore the dataset
cat("Dataset structure:\n")
str(mtcars)

cat("\nSummary statistics:\n")
print(summary(mtcars))

cat("\nMissing values by column:\n")
print(colSums(is.na(mtcars)))

# Visualize the outcome distribution
png("w3_mpg_histogram.png", width = 1000, height = 700, res = 130)
hist(mtcars$mpg, breaks = 8, col = "lightblue",
     main = "Distribution of Fuel Efficiency",
     xlab = "Miles per US gallon")
dev.off()

png("w3_mpg_boxplot.png", width = 900, height = 650, res = 130)
boxplot(mtcars$mpg, col = "wheat",
        main = "Fuel Efficiency Boxplot", ylab = "MPG")
dev.off()

# 2. Correlation analysis and hypothesis testing
cat("\nCorrelation matrix:\n")
print(cor(mtcars[, c("mpg", "wt", "hp", "cyl")]))

cat("\nPearson correlation test: vehicle weight vs MPG\n")
pearson_test <- cor.test(mtcars$wt, mtcars$mpg, method = "pearson")
print(pearson_test)

cat("\nSpearman correlation test: vehicle weight vs MPG\n")
spearman_test <- cor.test(mtcars$wt, mtcars$mpg, method = "spearman")
print(spearman_test)

# 3. Split data into training (75%) and testing (25%)
set.seed(42)
train_rows <- sample(seq_len(nrow(mtcars)),
                     size = floor(0.75 * nrow(mtcars)))
train_data <- mtcars[train_rows, ]
test_data <- mtcars[-train_rows, ]

cat("\nTraining rows:", nrow(train_data), "\n")
cat("Testing rows:", nrow(test_data), "\n")

# 4. Fit a multiple linear regression model
model <- lm(mpg ~ wt + hp + cyl, data = train_data)
cat("\nRegression model summary:\n")
print(summary(model))

# 5. Evaluate on held-out test set
test_predictions <- predict(model, newdata = test_data)
actual <- test_data$mpg

rmse <- sqrt(mean((actual - test_predictions)^2))
mae <- mean(abs(actual - test_predictions))
sst <- sum((actual - mean(actual))^2)
test_r_squared <- if (sst == 0) NA else
  1 - sum((actual - test_predictions)^2) / sst

cat("\nHeld-out test performance:\n")
cat("RMSE:", round(rmse, 3), "\n")
cat("MAE:", round(mae, 3), "\n")
cat("Test R-squared:", round(test_r_squared, 3), "\n")

png("w3_actual_vs_predicted.png", width = 900, height = 700, res = 130)
plot(actual, test_predictions, pch = 19, col = "steelblue",
     main = "Actual vs Predicted MPG",
     xlab = "Actual MPG", ylab = "Predicted MPG")
abline(0, 1, col = "red", lwd = 2)
dev.off()

# 6. Repeated 5-fold cross-validation on the training data
# This estimates how model performance varies across training folds.
set.seed(42)
k <- 5
repeats <- 10
cv_rmse <- numeric(0)
cv_mae <- numeric(0)

for (rep_id in seq_len(repeats)) {
  fold_id <- sample(rep(seq_len(k), length.out = nrow(train_data)))

  for (fold in seq_len(k)) {
    cv_train <- train_data[fold_id != fold, ]
    cv_valid <- train_data[fold_id == fold, ]

    cv_model <- lm(mpg ~ wt + hp + cyl, data = cv_train)
    cv_predictions <- predict(cv_model, newdata = cv_valid)
    cv_actual <- cv_valid$mpg

    cv_rmse <- c(cv_rmse,
                 sqrt(mean((cv_actual - cv_predictions)^2)))
    cv_mae <- c(cv_mae,
                mean(abs(cv_actual - cv_predictions)))
  }
}

cat("\nRepeated 5-fold cross-validation results:\n")
cat("Mean CV RMSE:", round(mean(cv_rmse), 3), "\n")
cat("Mean CV MAE:", round(mean(cv_mae), 3), "\n")
cat("CV RMSE standard deviation:", round(sd(cv_rmse), 3), "\n")

# 7. Model diagnostics
png("w3_model_diagnostic_plots.png", width = 1200, height = 900, res = 130)
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))
dev.off()

cat("\nShapiro-Wilk test on model residuals:\n")
print(shapiro.test(residuals(model)))

cat("\nCook's distance values:\n")
print(cooks.distance(model))

# 8. Export results for documentation
write.csv(data.frame(
  Actual_MPG = actual,
  Predicted_MPG = as.numeric(test_predictions),
  Residual = actual - as.numeric(test_predictions)
), "w3_test_predictions.csv", row.names = FALSE)

write.csv(data.frame(
  Metric = c("RMSE", "MAE", "Test_R_squared",
             "Mean_CV_RMSE", "Mean_CV_MAE", "CV_RMSE_SD"),
  Value = c(rmse, mae, test_r_squared,
            mean(cv_rmse), mean(cv_mae), sd(cv_rmse))
), "w3_model_metrics.csv", row.names = FALSE)

cat("\nGenerated charts and CSV files in the working directory:\n")
print(getwd())
cat("\nNote: Interpret results cautiously because mtcars has only 32 observations.\n")
