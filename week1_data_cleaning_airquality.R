# Week 1 Task: Data Cleaning and Preliminary Analysis with R
# Dataset: airquality (included in R's datasets package)
# Author: M G Shivaprasad
#
# Run this script in R or RStudio.
# Outputs are saved in the current working directory:
#   airquality_cleaned.csv
#   airquality_missing_values.png
#   airquality_boxplots.png
#   airquality_histograms.png
#   airquality_ozone_vs_temperature.png
#   airquality_correlation_matrix.csv
#   airquality_outlier_summary.csv

# -------------------------------
# 1. Load and inspect the dataset
# -------------------------------
data("airquality", package = "datasets")
df <- airquality

cat("Dataset dimensions:\n")
print(dim(df))

cat("\nDataset structure:\n")
str(df)

cat("\nFirst six observations:\n")
print(head(df))

cat("\nSummary statistics for original data:\n")
print(summary(df))

# ---------------------------------
# 2. Missing-value profile
# ---------------------------------
missing_count <- colSums(is.na(df))
missing_percent <- round(missing_count / nrow(df) * 100, 2)

missing_table <- data.frame(
  Variable = names(missing_count),
  Missing = as.integer(missing_count),
  Percent = as.numeric(missing_percent)
)

cat("\nMissing-value profile:\n")
print(missing_table)

cat("\nNumber of duplicate rows in original dataset:\n")
print(sum(duplicated(df)))

# Save missing-value chart
png("airquality_missing_values.png", width = 1100, height = 650, res = 130)
barplot(
  missing_count,
  main = "Missing Values by Variable — airquality",
  ylab = "Number of Missing Values",
  xlab = "Variable",
  col = "lightblue",
  border = "white"
)
dev.off()

# ---------------------------------
# 3. Clean data: median imputation
# ---------------------------------
# Keep df unchanged so the original data remains available for comparison.
clean_df <- df

clean_df$Ozone[is.na(clean_df$Ozone)] <-
  median(clean_df$Ozone, na.rm = TRUE)

clean_df$Solar.R[is.na(clean_df$Solar.R)] <-
  median(clean_df$Solar.R, na.rm = TRUE)

cat("\nMissing values after median imputation:\n")
print(colSums(is.na(clean_df)))

# ---------------------------------
# 4. Outlier detection using IQR
# ---------------------------------
numeric_vars <- c("Ozone", "Solar.R", "Wind", "Temp")

outlier_summary <- do.call(rbind, lapply(numeric_vars, function(v) {
  x <- clean_df[[v]]
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- IQR(x, na.rm = TRUE)
  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value

  data.frame(
    Variable = v,
    Lower_Bound = as.numeric(lower),
    Upper_Bound = as.numeric(upper),
    Potential_Outliers = sum(x < lower | x > upper, na.rm = TRUE)
  )
}))

cat("\nIQR outlier summary:\n")
print(outlier_summary)

write.csv(outlier_summary, "airquality_outlier_summary.csv", row.names = FALSE)

# Save boxplots for outlier review
png("airquality_boxplots.png", width = 1100, height = 800, res = 130)
par(mfrow = c(2, 2))
for (v in numeric_vars) {
  boxplot(
    clean_df[[v]],
    main = paste("Boxplot:", v),
    ylab = v,
    col = "lightblue"
  )
}
par(mfrow = c(1, 1))
dev.off()

# Note: potential outliers are flagged for investigation, not automatically removed.

# ---------------------------------
# 5. Min-max normalization
# ---------------------------------
min_max <- function(x) {
  rng <- range(x, na.rm = TRUE)

  if (diff(rng) == 0) {
    return(rep(0, length(x)))
  }

  (x - rng[1]) / (rng[2] - rng[1])
}

for (v in numeric_vars) {
  clean_df[[paste0(v, "_norm")]] <- min_max(clean_df[[v]])
}

cat("\nSummary of normalized variables:\n")
print(summary(clean_df[, paste0(numeric_vars, "_norm")]))

# ---------------------------------
# 6. Encode Month as a categorical variable
# ---------------------------------
clean_df$Month <- factor(
  clean_df$Month,
  levels = 5:9,
  labels = c("May", "June", "July", "August", "September")
)

# One-hot encoding for use in models requiring numeric inputs
month_one_hot <- model.matrix(~ Month - 1, data = clean_df)

cat("\nFirst rows of one-hot encoded Month:\n")
print(head(month_one_hot))

cat("\nNumber of observations by month:\n")
print(table(clean_df$Month))

# ---------------------------------
# 7. Exploratory data analysis
# ---------------------------------
cat("\nSummary statistics after cleaning:\n")
print(summary(clean_df[, c("Ozone", "Solar.R", "Wind", "Temp", "Month", "Day")]))

cat("\nMean measurements by month:\n")
monthly_summary <- aggregate(
  cbind(Ozone, Solar.R, Wind, Temp) ~ Month,
  data = clean_df,
  FUN = mean
)
print(monthly_summary)

# Histograms
png("airquality_histograms.png", width = 1100, height = 800, res = 130)
par(mfrow = c(2, 2))
hist(clean_df$Ozone, main = "Ozone Distribution", xlab = "Ozone", col = "lightblue")
hist(clean_df$Solar.R, main = "Solar Radiation Distribution", xlab = "Solar.R", col = "lightgreen")
hist(clean_df$Wind, main = "Wind Speed Distribution", xlab = "Wind", col = "lightyellow")
hist(clean_df$Temp, main = "Temperature Distribution", xlab = "Temp", col = "mistyrose")
par(mfrow = c(1, 1))
dev.off()

# Scatterplot: Ozone versus Temperature
png("airquality_ozone_vs_temperature.png", width = 900, height = 650, res = 130)
plot(
  clean_df$Temp,
  clean_df$Ozone,
  xlab = "Temperature",
  ylab = "Ozone",
  main = "Ozone versus Temperature",
  pch = 19,
  col = "steelblue"
)
abline(lm(Ozone ~ Temp, data = clean_df), lwd = 2)
dev.off()

# Pairwise plots for numeric variables
png("airquality_pairs_plot.png", width = 1000, height = 1000, res = 130)
pairs(clean_df[, numeric_vars], main = "Pairwise Plots After Cleaning")
dev.off()

# ---------------------------------
# 8. Correlation analysis
# ---------------------------------
numeric_original <- df[, numeric_vars]
cor_original <- cor(numeric_original, use = "pairwise.complete.obs")

numeric_clean <- clean_df[, numeric_vars]
cor_clean <- cor(numeric_clean, use = "complete.obs")

cat("\nPearson correlations using original data (pairwise complete observations):\n")
print(round(cor_original, 2))

cat("\nPearson correlations after imputation:\n")
print(round(cor_clean, 2))

write.csv(
  round(cor_original, 3),
  "airquality_correlation_matrix.csv",
  row.names = TRUE
)

# ---------------------------------
# 9. Quality checks
# ---------------------------------
stopifnot(nrow(clean_df) == nrow(df))
stopifnot(all(colSums(is.na(clean_df[, numeric_vars])) == 0))

for (v in numeric_vars) {
  normalized <- clean_df[[paste0(v, "_norm")]]
  stopifnot(all(normalized >= 0 & normalized <= 1))
}

cat("\nQuality checks passed:\n")
cat("- Row count preserved.\n")
cat("- No missing values remain in the selected numeric variables after imputation.\n")
cat("- Normalized values fall within [0, 1].\n")

# ---------------------------------
# 10. Export cleaned data and results
# ---------------------------------
write.csv(clean_df, "airquality_cleaned.csv", row.names = FALSE)
write.csv(monthly_summary, "airquality_monthly_summary.csv", row.names = FALSE)

cat("\nAnalysis complete. Output files were saved in:\n")
print(getwd())
