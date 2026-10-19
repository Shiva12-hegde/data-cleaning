# Week 4: Comprehensive Data Analysis Reporting and Presentation
# Integrated examples from Weeks 1-3 using R built-in datasets.
# This script recreates key cleaning, visualization, and modeling outputs.
# Base R only; no extra packages are required.

# -----------------------------
# PART A: WEEK 1 & 2 - AIRQUALITY
# -----------------------------
data(airquality)
cat("AIRQUALITY DATASET\n")
str(airquality)
print(summary(airquality))
cat("\nMissing values by variable:\n")
print(colSums(is.na(airquality)))

# Create a cleaned copy using median imputation for demonstration.
# Keep the original dataset unchanged and document that imputation was applied.
clean_airquality <- airquality
clean_airquality$Ozone[is.na(clean_airquality$Ozone)] <-
  median(clean_airquality$Ozone, na.rm = TRUE)
clean_airquality$Solar.R[is.na(clean_airquality$Solar.R)] <-
  median(clean_airquality$Solar.R, na.rm = TRUE)

cat("\nMissing values after median imputation:\n")
print(colSums(is.na(clean_airquality)))

# Export cleaned dataset
write.csv(clean_airquality, "w4_airquality_cleaned.csv", row.names = FALSE)

# 1. Bar chart: mean ozone by month using observed values
monthly_ozone <- aggregate(Ozone ~ Month, data = airquality,
                           FUN = mean, na.rm = TRUE)
png("w4_ozone_by_month.png", width = 1200, height = 700, res = 140)
barplot(monthly_ozone$Ozone,
        names.arg = month.abb[monthly_ozone$Month],
        col = "steelblue", border = NA,
        main = "Average Ozone by Month",
        xlab = "Month", ylab = "Mean Ozone Concentration")
dev.off()

# 2. Line charts: monthly mean temperature and wind in separate panels
monthly_temp <- aggregate(Temp ~ Month, data = airquality, FUN = mean)
monthly_wind <- aggregate(Wind ~ Month, data = airquality, FUN = mean)
png("w4_monthly_temperature_wind.png", width = 1200, height = 600, res = 140)
par(mfrow = c(1, 2))
plot(month.abb[monthly_temp$Month], monthly_temp$Temp,
     type = "b", pch = 19, col = "firebrick",
     main = "Monthly Mean Temperature",
     xlab = "Month", ylab = "Temperature (°F)")
plot(month.abb[monthly_wind$Month], monthly_wind$Wind,
     type = "b", pch = 17, col = "darkgreen",
     main = "Monthly Mean Wind",
     xlab = "Month", ylab = "Wind Speed")
par(mfrow = c(1, 1))
dev.off()

# 3. Scatter plot: ozone vs temperature, using complete pairs only
ok <- complete.cases(airquality$Temp, airquality$Ozone)
png("w4_ozone_vs_temperature.png", width = 1000, height = 700, res = 140)
plot(airquality$Temp[ok], airquality$Ozone[ok],
     pch = 19, col = "steelblue",
     main = "Ozone vs Temperature",
     xlab = "Temperature (°F)", ylab = "Ozone Concentration")
abline(lm(Ozone ~ Temp, data = airquality[ok, ]),
       col = "red", lwd = 2)
dev.off()

# 4. Histogram and boxplot for ozone
png("w4_ozone_histogram.png", width = 1000, height = 700, res = 140)
hist(airquality$Ozone, breaks = 12, col = "lightblue",
     border = "white", main = "Distribution of Ozone Readings",
     xlab = "Ozone Concentration", ylab = "Frequency")
dev.off()

png("w4_ozone_boxplot_by_month.png", width = 1000, height = 700, res = 140)
boxplot(Ozone ~ factor(Month), data = airquality,
        col = "wheat", names = month.abb[5:9],
        main = "Ozone Distribution by Month",
        xlab = "Month", ylab = "Ozone Concentration")
dev.off()

# Correlation among environmental variables (pairwise complete observations)
air_cor <- cor(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")],
               use = "pairwise.complete.obs")
print(air_cor)
write.csv(air_cor, "w4_airquality_correlation_matrix.csv")

# -----------------------------
# PART B: WEEK 3 - MTCARS STATISTICS & MODEL
# -----------------------------
data(mtcars)
cat("\nMTCARS DATASET\n")
str(mtcars)
print(summary(mtcars))
cat("\nMissing values by variable:\n")
print(colSums(is.na(mtcars)))

# Hypothesis tests: association between weight and fuel efficiency
cat("\nPearson correlation test (wt vs mpg):\n")
pearson_test <- cor.test(mtcars$wt, mtcars$mpg, method = "pearson")
print(pearson_test)

cat("\nSpearman correlation test (wt vs mpg):\n")
spearman_test <- cor.test(mtcars$wt, mtcars$mpg, method = "spearman")
print(spearman_test)

# Scatter plot: weight vs mpg
png("w4_weight_vs_mpg.png", width = 1000, height = 700, res = 140)
plot(mtcars$wt, mtcars$mpg, pch = 19, col = "steelblue",
     main = "Vehicle Weight vs Fuel Efficiency",
     xlab = "Weight (1000 lb)", ylab = "Miles per US gallon")
abline(lm(mpg ~ wt, data = mtcars), col = "red", lwd = 2)
dev.off()

# Train-test split (75% training, 25% test)
set.seed(42)
train_rows <- sample(seq_len(nrow(mtcars)),
                     size = floor(0.75 * nrow(mtcars)))
train_data <- mtcars[train_rows, ]
test_data <- mtcars[-train_rows, ]

# Multiple linear regression
model <- lm(mpg ~ wt + hp + cyl, data = train_data)
cat("\nRegression model summary:\n")
print(summary(model))

# Test metrics
predictions <- predict(model, newdata = test_data)
actual <- test_data$mpg
rmse <- sqrt(mean((actual - predictions)^2))
mae <- mean(abs(actual - predictions))
sst <- sum((actual - mean(actual))^2)
test_r_squared <- if (sst == 0) NA else
  1 - sum((actual - predictions)^2) / sst

cat("\nHeld-out test metrics:\n")
cat("RMSE:", round(rmse, 3), "\n")
cat("MAE:", round(mae, 3), "\n")
cat("Test R-squared:", round(test_r_squared, 3), "\n")

# Actual vs predicted plot
png("w4_actual_vs_predicted.png", width = 1000, height = 700, res = 140)
plot(actual, predictions, pch = 19, col = "steelblue",
     main = "Actual vs Predicted MPG",
     xlab = "Actual MPG", ylab = "Predicted MPG")
abline(0, 1, col = "red", lwd = 2)
dev.off()

# Repeated 5-fold cross-validation on the training data
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
    cv_pred <- predict(cv_model, newdata = cv_valid)
    cv_actual <- cv_valid$mpg
    cv_rmse <- c(cv_rmse, sqrt(mean((cv_actual - cv_pred)^2)))
    cv_mae <- c(cv_mae, mean(abs(cv_actual - cv_pred)))
  }
}

cat("\nRepeated 5-fold CV on training data:\n")
cat("Mean CV RMSE:", round(mean(cv_rmse), 3), "\n")
cat("Mean CV MAE:", round(mean(cv_mae), 3), "\n")
cat("CV RMSE SD:", round(sd(cv_rmse), 3), "\n")

# Diagnostic plots and residual normality test
png("w4_model_diagnostics.png", width = 1200, height = 900, res = 140)
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))
dev.off()

cat("\nShapiro-Wilk test on model residuals:\n")
print(shapiro.test(residuals(model)))

# Export model results
write.csv(data.frame(
  Actual_MPG = actual,
  Predicted_MPG = as.numeric(predictions),
  Residual = actual - as.numeric(predictions)
), "w4_test_predictions.csv", row.names = FALSE)

write.csv(data.frame(
  Metric = c("RMSE", "MAE", "Test_R_squared",
             "Mean_CV_RMSE", "Mean_CV_MAE", "CV_RMSE_SD"),
  Value = c(rmse, mae, test_r_squared,
            mean(cv_rmse), mean(cv_mae), sd(cv_rmse))
), "w4_model_metrics.csv", row.names = FALSE)

cat("\nAll generated files are saved in:\n")
print(getwd())
cat("\nImportant: Interpret results cautiously; mtcars contains only 32 observations.\n")
