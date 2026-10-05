# 1. Load Required Libraries
#install.packages(c("forecast", "tseries", "ggplot2", "dplyr", "lubridate"))
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(lubridate)

# 2. Read Dataset & Structure Inspection
data <- read.csv("~/Downloads/bputra2.csv", check.names = FALSE)

# Convert Date column (Format: DD-MMM-YY, e.g., '1-Jan-98')
data$Date <- as.Date(data$Date, format = "%d-%b-%y")

# Sort observations chronologically
data <- data %>% arrange(Date)

# Dataset structure and summary information
str(data)
summary(data)

# 3. Data Preprocessing
# Check missing values
cat("Missing Values:\n")
colSums(is.na(data))

# Impute missing Discharge values using linear interpolation (standard hydrological practice)
data$Discharge <- zoo::na.approx(data$Discharge, na.rm = FALSE)
data$Discharge[is.na(data$Discharge)] <- median(data$Discharge, na.rm = TRUE)

# Remove duplicates if any
data <- distinct(data)

# Create useful date features
data$DayOfWeek <- weekdays(data$Date)
data$Month <- month(data$Date, label = TRUE)
data$Year <- year(data$Date)

# 4. Exploratory Data Analysis (EDA)
# 4.1 Overall River Discharge Timeline Plot
ggplot(data, aes(x = Date, y = Discharge)) +
  geom_line(color = "steelblue") +
  labs(title = "Daily River Water Discharge",
       x = "Time",
       y = "Discharge") +
  theme_minimal()

# 4.2 Distribution of River Discharge
ggplot(data, aes(x = Discharge)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "black") +
  labs(title = "Distribution of River Water Discharge",
       x = "Discharge",
       y = "Count") +
  theme_minimal()

# 4.3 Average Discharge by Month
monthly_avg <- data %>%
  group_by(Month) %>%
  summarise(Average_Discharge = mean(Discharge, na.rm = TRUE))

ggplot(monthly_avg, aes(x = Month, y = Average_Discharge, group = 1)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(size = 2, color = "darkgreen") +
  labs(title = "Average Monthly River Water Discharge",
       x = "Month",
       y = "Average Discharge") +
  theme_minimal()

# 5. Convert into Time Series Object
# Weekly seasonality frequency (7 days)
discharge_ts <- ts(data$Discharge, frequency = 7)

# Plot time series object
autoplot(discharge_ts) +
  labs(title = "Weekly Seasonality Time Series of River Discharge",
       x = "Time Index",
       y = "Discharge") +
  theme_minimal()

# 6. Stationarity Test
adf_result <- adf.test(discharge_ts)
print(adf_result)
# Seasonal differencing (lag = 7) to achieve stationarity
discharge_diff <- diff(discharge_ts, lag = 7)
autoplot(discharge_diff) +
  labs(title = "Seasonally Differenced River Discharge Series",
       x = "Time Index",
       y = "Differenced Discharge") +
  theme_minimal()

adf_diff <- adf.test(na.omit(discharge_diff))
print(adf_diff)

# 7. ACF and PACF Plots
par(mfrow = c(1, 2))
Acf(discharge_diff, main = "ACF Plot - Differenced Series")
Pacf(discharge_diff, main = "PACF Plot - Differenced Series")
par(mfrow = c(1, 1))

# 8. Build Candidate SARIMA Models for Discharge
model1 <- Arima(discharge_ts, order = c(1, 0, 1), seasonal = list(order = c(1, 1, 1), period = 7))
model2 <- Arima(discharge_ts, order = c(1, 0, 1), seasonal = list(order = c(2, 1, 1), period = 7))
model3 <- Arima(discharge_ts, order = c(2, 0, 1), seasonal = list(order = c(1, 1, 1), period = 7))
model4 <- Arima(discharge_ts, order = c(2, 0, 1), seasonal = list(order = c(2, 1, 1), period = 7))

# 9. Compare Models Using AIC & BIC
comparison <- data.frame(
  Model = c("SARIMA(1,0,1)(1,1,1)[7]",
            "SARIMA(1,0,1)(2,1,1)[7]",
            "SARIMA(2,0,1)(1,1,1)[7]",
            "SARIMA(2,0,1)(2,1,1)[7]"),
  AIC = c(AIC(model1), AIC(model2), AIC(model3), AIC(model4)),
  BIC = c(BIC(model1), BIC(model2), BIC(model3), BIC(model4))
)
print(comparison)

# Select best model based on lowest AIC
best_index <- which.min(comparison$AIC)
cat("\nBest Model:\n")
print(comparison[best_index, ])

best_model <- list(model1, model2, model3, model4)[[best_index]]
summary(best_model)

# 10. Residual Diagnostics
checkresiduals(best_model)

# Box-Ljung Test for Residual Autocorrelation
Box.test(residuals(best_model), lag = 20, type = "Ljung")

# 11. Forecast River Water Discharge (30 Days Ahead)
forecast_discharge <- forecast(best_model, h = 30)
print(forecast_discharge)

autoplot(forecast_discharge) +
  labs(title = "30-Day River Water Discharge Forecast",
       x = "Time",
       y = "Discharge") +
  theme_minimal()

# Save forecast values with dates
forecast_values <- data.frame(
  Date = seq(max(data$Date) + 1, by = "day", length.out = 30),
  Forecast_Discharge = as.numeric(forecast_discharge$mean),
  Lower_95 = forecast_discharge$lower[, 2],
  Upper_95 = forecast_discharge$upper[, 2]
)

print(forecast_values)

# 12. Peak Flood Load & Risk Analysis
# Aggregate Daily Peak Discharge
daily_peak <- data %>%
  group_by(Date) %>%
  summarise(Peak_Discharge = max(Discharge, na.rm = TRUE),
            Flood_Event = max(Flood, na.rm = TRUE))

# Plot Daily Peak Discharge & Actual Flood Markers
ggplot(daily_peak, aes(x = Date, y = Peak_Discharge)) +
  geom_line(color = "red") +
  geom_point(data = subset(daily_peak, Flood_Event == 1), aes(y = Peak_Discharge), color = "black", size = 1.5) +
  labs(title = "Daily Peak River Discharge & Flood Occurrences (Black Points)",
       x = "Date",
       y = "Peak Discharge") +
  theme_minimal()

# 13. Peak Flood Forecasting using Auto ARIMA
peak_ts <- ts(daily_peak$Peak_Discharge, frequency = 7)
peak_model <- auto.arima(peak_ts, seasonal = TRUE)

summary(peak_model)

peak_forecast <- forecast(peak_model, h = 30)

autoplot(peak_forecast) +
  labs(title = "30-Day Peak Flood Discharge Forecast",
       x = "Days",
       y = "Peak Discharge") +
  theme_minimal()

# 14. Weekly Flood Risk Pattern Analysis
weekly_flood <- data %>%
  group_by(DayOfWeek) %>%
  summarise(Average_Discharge = mean(Discharge, na.rm = TRUE),
            Flood_Count = sum(Flood, na.rm = TRUE))

ggplot(weekly_flood, aes(x = reorder(DayOfWeek, Average_Discharge), y = Average_Discharge, fill = DayOfWeek)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Average River Discharge by Day of Week",
       x = "Day of Week",
       y = "Average Discharge") +
  theme_minimal()

# 15. Monthly Flood & Discharge Seasonality Pattern
monthly_pattern <- data %>%
  group_by(Month) %>%
  summarise(
    Average_Discharge = mean(Discharge, na.rm = TRUE),
    Total_Floods = sum(Flood, na.rm = TRUE)
  )

ggplot(monthly_pattern, aes(x = Month, y = Average_Discharge, group = 1)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(size = 2.5, color = "darkred") +
  labs(title = "Average Monthly River Water Discharge Seasonality",
       x = "Month",
       y = "Average Discharge") +
  theme_minimal()