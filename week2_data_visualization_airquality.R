# Week 2: Data Visualization and Insight Communication using R
# Dataset: R built-in airquality dataset
# No external packages are required.

# Load and inspect the dataset
data(airquality)
cat("Dataset dimensions:", nrow(airquality), "rows x", ncol(airquality), "columns\n")
str(airquality)
summary(airquality)
cat("\nMissing values by column:\n")
print(colSums(is.na(airquality)))

# 1. Bar chart: average ozone by month
monthly_ozone <- aggregate(Ozone ~ Month, data = airquality,
                           FUN = mean, na.rm = TRUE)

png("w2_average_ozone_by_month.png", width = 1200, height = 700, res = 140)
barplot(monthly_ozone$Ozone,
        names.arg = month.abb[monthly_ozone$Month],
        col = "steelblue", border = NA,
        main = "Average Ozone Level by Month",
        xlab = "Month", ylab = "Mean Ozone Concentration")
dev.off()

# 2. Line charts: monthly average temperature and wind in separate panels
monthly_temp <- aggregate(Temp ~ Month, data = airquality, FUN = mean)
monthly_wind <- aggregate(Wind ~ Month, data = airquality, FUN = mean)

png("w2_monthly_temperature_wind.png", width = 1200, height = 600, res = 140)
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

# 3. Scatter plot: ozone vs temperature
# Keep only rows where both variables are available.
complete_rows <- complete.cases(airquality$Temp, airquality$Ozone)

png("w2_ozone_vs_temperature.png", width = 1000, height = 700, res = 140)
plot(airquality$Temp[complete_rows], airquality$Ozone[complete_rows],
     pch = 19, col = "steelblue",
     main = "Ozone vs Temperature",
     xlab = "Temperature (°F)", ylab = "Ozone Concentration")
abline(lm(Ozone ~ Temp, data = airquality[complete_rows, ]),
       col = "red", lwd = 2)
dev.off()

# 4. Histogram: distribution of ozone readings
png("w2_ozone_histogram.png", width = 1000, height = 700, res = 140)
hist(airquality$Ozone,
     breaks = 12, col = "lightblue", border = "white",
     main = "Distribution of Ozone Readings",
     xlab = "Ozone Concentration", ylab = "Frequency")
dev.off()

# 5. Boxplot: ozone distribution by month
png("w2_ozone_boxplot_by_month.png", width = 1000, height = 700, res = 140)
boxplot(Ozone ~ factor(Month), data = airquality,
        col = "wheat", names = month.abb[5:9],
        main = "Ozone Distribution by Month",
        xlab = "Month", ylab = "Ozone Concentration")
dev.off()

# 6. Optional correlation summary
correlation_data <- airquality[, c("Ozone", "Solar.R", "Wind", "Temp")]
correlation_matrix <- cor(correlation_data, use = "pairwise.complete.obs")
print(correlation_matrix)
write.csv(correlation_matrix, "w2_correlation_matrix.csv")

# 7. Export monthly summary table
monthly_summary <- aggregate(cbind(Temp, Wind) ~ Month,
                             data = airquality, FUN = mean)
monthly_summary$MonthName <- month.abb[monthly_summary$Month]
write.csv(monthly_summary, "w2_monthly_summary.csv", row.names = FALSE)

cat("\nGenerated visualization files and summary CSVs in:\n")
print(getwd())
