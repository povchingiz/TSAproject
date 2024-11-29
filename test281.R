# Sofya Nurekenova, Altynay Saken, Chingiz Yertay
library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(zoo)
library(prophet)

# Step 1: Load the Data
file_path <- "C:/Users/Chingiz/OneDrive/Рабочий стол/TSAPROj/meteo.xlsx"
data <- read_excel(file_path, sheet = 1)

# Step 2: Clean and Preprocess the Data
cleaned_data <- data %>%
  select(
    date, 
    `18`, `21`, `00`, `03`, `06`, `09`, `12`, `15`, 
    daily_avg_temp = average
  ) %>%
  filter(!is.na(date), !is.na(daily_avg_temp))

cleaned_data$date <- as.Date(cleaned_data$date)

# Step 3: Aggregate to Monthly Data
monthly_data <- cleaned_data %>%
  mutate(year_month = format(date, "%Y-%m")) %>%
  group_by(year_month) %>%
  summarise(daily_avg_temp = mean(daily_avg_temp, na.rm = TRUE)) %>%
  ungroup()

# Convert to monthly time series
temperature_ts <- ts(
  monthly_data$daily_avg_temp, 
  frequency = 12,              # Monthly data
  start = c(as.numeric(substr(min(monthly_data$year_month), 1, 4)), 
            as.numeric(substr(min(monthly_data$year_month), 6, 7)))
)

# Plot the daily average temperature over the entire dataset
ggplot(cleaned_data, aes(x = date, y = daily_avg_temp)) +
  geom_line(color = "blue", size = 0.7) +
  labs(
    title = "Daily Average Temperature Over 5 Years",
    x = "Date",
    y = "Temperature (\u00b0C)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Step 4: Plot all data to observe behavior
ggplot(monthly_data, aes(x = as.Date(paste0(year_month, "-01")), y = daily_avg_temp)) +
  geom_line(color = "blue") +
  labs(
    title = "Monthly Average Temperature",
    x = "Date",
    y = "Temperature (\u00b0C)"
  ) +
  theme_minimal()

# Step 5: Analyze Model Components
# Check stationarity
adf_test <- adf.test(temperature_ts)
cat("ADF Test p-value:", adf_test$p.value, "\n")

if (adf_test$p.value > 0.05) {
  temperature_ts <- diff(temperature_ts, differences = 1)
  cat("Differencing applied. ADF Test p-value after differencing:", adf_test$p.value, "\n")
}

# ACF and PACF
acf(temperature_ts, main = "ACF of Differenced Data")
pacf(temperature_ts, main = "PACF of Differenced Data")

# Step 6: Fit Models
# ARIMA Model
arima_model <- auto.arima(temperature_ts, seasonal = TRUE)
cat("ARIMA Model Summary:\n")
print(summary(arima_model))

# SARIMA Model
sarima_model <- Arima(
  temperature_ts, 
  order = c(1, 1, 1),         #p,d,q
  seasonal = c(1, 1, 1)       #P,D,Q
)
cat("SARIMA Model Summary:\n")
print(summary(sarima_model))

# Residual diagnostics
checkresiduals(sarima_model)

# Step 7: Plot the last 6 months with actual and predicted last 3 months

# Extract the last 6 months from the dataset
last_6_months <- monthly_data %>% 
  tail(6)

# Predict the last 3 months from the dataset
forecast_horizon_3 <- 3  # Predict last 3 months in the dataset
sarima_forecast_3 <- forecast(sarima_model, h = forecast_horizon_3)

# Prepare comparison data for last 6 months, including actual and predicted last 3 months
comparison_last_6 <- data.frame(
  Date = as.Date(paste0(last_6_months$year_month, "-01")),
  Actual = last_6_months$daily_avg_temp,
  Predicted = c(rep(NA, 3), sarima_forecast_3$mean)  # Predictions for the last 3 months
)

# Plot the last 6 months with actual and predicted last 3 months
ggplot(comparison_last_6, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1, linetype = "dashed") +
  labs(
    title = "Last 6 Months: Actual vs Predicted (SARIMA)",
    x = "Date",
    y = "Temperature (\u00b0C)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Numerical comparison for the last 3 months in the dataset
cat("Numerical Comparison for Last 3 Months in Dataset:\n")
numerical_comparison <- data.frame(
  Date = as.Date(paste0(last_6_months$year_month[4:6], "-01")),
  Actual = last_6_months$daily_avg_temp[4:6],
  Predicted = sarima_forecast_3$mean
)
print(numerical_comparison)

# Prepare extended dataset to include predictions for the last 3 months
extended_monthly_data <- monthly_data %>%
  mutate(Predicted = NA) %>% 
  bind_rows(
    data.frame(
      year_month = format(seq(
        as.Date(paste0(tail(monthly_data$year_month, 1), "-01")), 
        by = "month", 
        length.out = forecast_horizon_3
      ), "%Y-%m"),
      daily_avg_temp = rep(NA, forecast_horizon_3),
      Predicted = sarima_forecast_3$mean
    )
  )

# Plot all data with predictions
ggplot(extended_monthly_data, aes(x = as.Date(paste0(year_month, "-01")))) +
  geom_line(aes(y = daily_avg_temp, color = "Actual"), na.rm = TRUE) +
  geom_line(aes(y = Predicted, color = "Predicted"), na.rm = TRUE) +
  labs(
    title = "All Data with Last 3 Months Predictions",
    x = "Date",
    y = "Temperature (\u00b0C)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Step 8: Predict Future 6 Months
forecast_horizon_6 <- 6  # Future 6 months
sarima_forecast_6 <- forecast(sarima_model, h = forecast_horizon_6)

# Prophet Model for Future 6 Months
prophet_data <- monthly_data %>%
  mutate(ds = as.Date(paste0(year_month, "-01")), y = daily_avg_temp) %>%
  select(ds, y)
prophet_model <- prophet(prophet_data)
future <- make_future_dataframe(prophet_model, periods = forecast_horizon_6, freq = "month")
prophet_forecast <- predict(prophet_model, future)

# Prepare comparison data
comparison_6_future <- data.frame(
  Date = seq(as.Date(paste0(monthly_data$year_month[nrow(monthly_data)], "-01")) + months(1), 
             by = "month", length.out = forecast_horizon_6),
  SARIMA = sarima_forecast_6$mean,
  Prophet = tail(prophet_forecast$yhat, forecast_horizon_6)
)

# Plot future forecasts
comparison_6_future <- data.frame(
  Date = seq(as.Date(paste0(monthly_data$year_month[nrow(monthly_data)], "-01")) + months(1), 
             by = "month", length.out = forecast_horizon_6),
  SARIMA = sarima_forecast_6$mean,
  Prophet = tail(prophet_forecast$yhat, forecast_horizon_6)
)

# Plot SARIMA vs Prophet for future 6 months
ggplot(comparison_6_future, aes(x = Date)) +
  geom_line(aes(y = SARIMA, color = "SARIMA"), size = 1) +
  geom_line(aes(y = Prophet, color = "Prophet"), size = 1, linetype = "dashed") +
  labs(
    title = "Future 6-Month Predictions: SARIMA vs Prophet",
    x = "Date",
    y = "Temperature (°C)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("SARIMA" = "red", "Prophet" = "purple"))

# Step 9: Evaluate Model Performance
# Calculate RMSE for SARIMA and Prophet
sarima_rmse <- sqrt(mean((comparison_6_future$SARIMA - comparison_6_future$Prophet)^2, na.rm = TRUE))
cat("SARIMA RMSE for Future 6 Months:", sarima_rmse, "\n")

# Display comparison of future predictions
cat("Comparison of Future Predictions:\n")
print(comparison_6_future)

# Step 10: Save Outputs for Analysis
# Save comparison data to a CSV
#write.csv(comparison_6_future, "comparison_6_future.csv", row.names = FALSE)

#cat("Future prediction comparison saved to 'comparison_6_future.csv'\n")
