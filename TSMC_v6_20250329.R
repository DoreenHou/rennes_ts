######### Team Members #########
# ETU20240090 Kuan-Yu Hou      #
# ETU20240092 Ruei-Chieh Hung  #
# ETU20240228 Ping-Yao Wang    #
# ETU20240126 Yi-Ling, Li      #
################################

########################### Choice of Project ###########################
# Taiwan Semiconductor Manufacturing Company (TSMC), listed as 2330.TW, # 
# is the world's largest and most advanced semiconductor foundry. It    #
# plays a critical role in Taiwan's economy, technology industry, and   #
# global supply chains. As a leader in semiconductor manufacturing,     #
# TSMC produces chips for top technology companies, including Apple,    #
# NVIDIA, and AMD. Given the increasing reliance on semiconductor       #
# technology across industries, TSMC's financial performance directly   #
# influences Taiwan's economy, global tech markets, and geopolitical    #
# stability. Understanding its stock price movements is crucial for     #
# investors, financial analysts, and policymakers. Our project aims to  #
# analyze historical stock price data to identify trends, seasonality,  #
# and forecast future price movements using time series models.         #
#########################################################################

############################## Objective ################################
# The primary objectives of this study are:                             #
# 1. Explore the historical stock price trends of 2330.TW over the past #
#    10 years.                                                          #
# 2. Apply various time series forecasting methods, including SMA, ETS, #
#    ARIMA, and prophet to predict future stock prices.                 #
# 3. Evaluate model performance using accuracy metrics: RMSE & MAPE.    #
#########################################################################

#################### Dataset Information ####################
# Source: Yahoo Finance or other financial data providers.  #
# Time Frame: Monthly stock prices from the past 10 years.  #
# Key Variables: Date (time index), Closing Price (Cl).     #
#############################################################

# Load required packages
rm(list = ls()) # clean the environment
library(quantmod)
library(tseries)
library(forecast)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(Metrics)
library(TTR)
library(dplyr)
library(prophet)
library(reshape2)

# Download TSMC (2330.TW) stock data from Yahoo Finance
getSymbols("2330.TW", src = "yahoo", from = "2015-01-01", to = '2024-12-31', periodicity = 'monthly')

# Remove missing values
data <- na.omit(`2330.TW`) 

# Extract and clean closing prices
tsmc <- na.omit(Cl(`2330.TW`))

# Convert xts object to data.frame and keep the date
tsmc_df <- data.frame(
  date = index(tsmc),                # Extract date from xts index
  price = as.numeric(tsmc)           # Extract closing prices
)

# Format the date column as "YYYY/MM/DD"
tsmc_df$date <- as.Date(tsmc_df$date)

# View the transformed data
head(tsmc_df)

# Plot time series
autoplot(tsmc) + ggtitle("TSMC Closing Prices") + xlab("Date") + ylab("Price")

#################################### plot analysis ################################
# The chart shows the historical closing prices of TSMC (2330.TW) from 2015 to    #
# early 2025. The price shows a strong long-term upward trend. Starting below     #
# 100 TWD in 2015, the price steadily increased and approached 1,000 TWD by 2025. #
# There was a steep rise from late 2019 to mid-2021, likely driven by the global  #
# semiconductor boom and the surging demand for advanced chip manufacturing, where# 
# TSMC plays a leading role. After peaking around mid-2021, the stock experienced #
# a correction throughout 2022, which may have been due to global economic        #
# uncertainty, rising interest rates, and a broader tech sector adjustment. From  #
# mid-2023 onward, the price began climbing again, reaching new all-time highs by #
# early 2025. This rebound could be attributed to growing demand for AI and       #
# high-performance computing, both of which rely heavily on TSMC’s cutting-edge   #
# processes. Overall, the chart reflects consistent long-term growth with periods #
# of volatility, highlighting TSMC’s resilience and key role in the global tech   #
# supply chain.                                                                   #
###################################################################################

# Stationarity Test (KPSS Test) 
kpss.test(tsmc)  # p-value < 0.05, series is non-stationary

####################### KPSS test intrepretation ##################################
# The KPSS test evaluates whether a time series is stationary around a constant   #
# level. In this case, the p-value is 0.01, which is less than the 0.05           #
# significance level, indicating that we reject the null hypothesis of            #
# stationarity.Therefore, the TSMC closing price series is non-stationary, likely # 
# due to a strong upward trend over time.                                         #
###################################################################################

# Apply first difference to make data stationary
diff_tsmc <- diff(tsmc)
kpss.test(na.omit(diff_tsmc))  # p-value > 0.05 series is stationary
# The KPSS test on the differenced TSMC series yields a p-value of 0.07894
#(which is greater than the 0.05 significance level.) This means we fail to reject the null hypothesis of stationarity. 
# Therefore, the first-differenced TSMC closing price series can be considered stationary.

# seasonality test
acf(tsmc_df$price)

######################### seasonality test intrepretation #########################
# The ACF plot shows very high and steadily decreasing autocorrelation values     #
# through many lags. This pattern suggests that the initial stock price series is # 
# not stationary. In a non-stationary time series, the past values have a strong  #
# and lasting influence on future values, which is the reason why the             #
# autocorrelations decline slowly but are still important through many lags.      #
# In contrast, a stationary series would have rapidly declining autocorrelations  #
# that fall within the blue bands of confidence after a few lags. This result     #
# explains the need to differentiate the data to achieve stationarity before      #
# applying time series models like ARIMA. Based on the ACF analysis, the data is  #
# non-stationary. To confirm this, we applied the KPSS test to the differenced    #
# TSMC stock price series.                                                        #
###################################################################################

#### split the data into train and test ####
# Training set: 1 to 90 (month)
# Testing set: 91 to 120 >>> 30 (weeks)month

length(tsmc_df$price) # 120
h = 30
ts.train = tsmc_df$price[1:90]
length(ts.train) # 90
ts.test = tsmc_df$price[-(1:90)] 
length(ts.test) # 30
length(ts.train)+length(ts.test) == length(tsmc_df$price) # true

# Add time attributes for proper date display in autoplot 
# Convert date to Date type and create a weekly time series
tsmc_df$date <- as.Date(tsmc_df$date)
tsmc_ts <- ts(tsmc_df$price, start = c(2015, 1), frequency = 12)

#### Forecasting Models(MA:SMA/WMA, ARIMA, ETS:SES/HOLT/HW) ####

########################### MA #################################
# 90-day Simple Moving Average
sma_result <- SMA(tsmc[1:90], n = 10)
print(sma_result)
plot(tsmc[1:90], type = "o", col = "blue", main = "Simple Moving Average (SMA)")
lines(sma_result, col = "red", lwd = 2)

# 90-day Weighted Moving Average
WMA(tsmc[1:90], n = 3, wts = c(0.1, 0.3, 0.6))
wma_result <- WMA(tsmc[1:90], n = 10)
plot(tsmc[1:90], type = "o", col = "blue", main = "Weighted Moving Average (WMA)")
lines(wma_result, col = "red", lwd = 2)

############################ ARIMA ###############################

##### ARIMA MODELING #####
# fit the best ARIMA model
fit_arima <- auto.arima(ts.train) # ARIMA(0,1,0) 
fit_arima
summary(fit_arima)

########################### ARIMA Model fitting analysis ########################
# The ARIMA(0,1,0) with drift model shows that TSMC’s stock price has a steady  #
# upward trend after differencing, with prices going up by about 3.76 TWD each  #
# period on average. The prediction errors are low, and the residuals look      #
# good,so the model fits pretty well.                                           #
#################################################################################

# Check residuals of ARIMA
checkresiduals(fit_arima)
# Top chart (residuals against time):
# - The residuals mostly hover around zero, which is a good sign. 
# - Toward the end, we see some larger spikes, suggesting slight changes in volatility, but overall, there’s no clear trend or pattern.

# Bottom left (ACF of residuals):
# - Most bars fall within the blue dotted lines, indicating that there’s no strong autocorrelation left in the residuals. 
# - This means the model has captured the main structure of the data well.

# Bottom right (histogram of residuals):
# - The residuals are fairly bell-shaped, resembling a normal distribution.
# - This suggests that the model errors are reasonably well-behaved and balanced.


###### ARIMA FORECAST ######
pred.arima = forecast(fit_arima, h=30)
pred.arima

# plot forecast with the confidence interval
autoplot(pred.arima) +
  ggtitle("Forecast from ARIMA(0,1,0)") +
  xlab("Date") + ylab("TSMC Price") +
  theme_minimal()

# print forecast by mean
fore.arima = pred.arima$mean
length(fore.arima) # 30

# calculate the rmse
rmse.arima = rmse(ts.test, fore.arima) 
print(rmse.arima) # 216.3465

############################ ETS ##################################

fit_ets <- ets(ts.train)

##### ses #####
ses.test = ses(ts.train, h = 30) 
length(ses.test$mean)
plot(ses.test, main = "Simple Exponential Smoothing Forecast",
     xlab = "Time", ylab = "Value", col = "blue", fcol = "red")

##### holt #####
holt.test = holt(ts.train, h = 30)
length(holt.test$mean)
plot(holt.test, main = "HOLT Forecast",
     xlab = "Time", ylab = "Value", col = "blue", fcol = "red")

##### hw addictive & hw multiplicative #####
# Since there is no seasonality, hw addictive & hw multiplicative isn't suitable for this data



################################ Prophet ##################################

# Train Prophet model on training set with monthly data
prophet.train <- tsmc_df[1:90, ] %>%
  rename(ds = date, y = price)  # Rename columns for Prophet

# Train the Prophet model
prophet.model <- prophet(prophet.train)

# Create a future dataframe for the next 30 months
prophet.future <- make_future_dataframe(prophet.model, periods = 30, freq = "month")

# Forecast
prophet.forecast <- predict(prophet.model, prophet.future)

# Extract forecasted values for the next 30 months
fore.prophet <- prophet.forecast$yhat[(nrow(prophet.train) + 1):(nrow(prophet.train) + 30)]

# Plot the forecast
plot(prophet.model, prophet.forecast)

# To compare the forecast with the actual test data (the last 30 months), you can also plot it:
test_data <- tsmc_df[91:120, ] %>% rename(ds = date, y = price)

# Plot actual vs forecasted
plot(test_data$ds, test_data$y, type = "o", col = "blue", 
     main = "Prophet Forecast vs Actual Data", xlab = "Date", ylab = "Price")
lines(test_data$ds, fore.prophet, type = "o", col = "red")
legend("topleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)

##### Model Performance Comparison ######

# Compare model performance between arima and ets using AIC & BIC
aic_arima = AIC(fit_arima)
aic_ets = AIC(fit_ets)
ifelse(aic_arima < aic_ets, 'The best model is the arima', 'The best model is ets')

bic_arima = BIC(fit_arima)
bic_ets = BIC(fit_ets)
ifelse(bic_arima < bic_ets, 'The best model is the arima', 'The best model is ets')

##### Forecast evaluation #####
rmse.arima = rmse(ts.test, fore.arima)
rmse.ses = rmse(ts.test, ses.test$mean)
rmse.holt = rmse(ts.test, holt.test$mean)
rmse.prophet = rmse(ts.test, fore.prophet)
table = cbind(c("ARIMA","SES","HOLT","PROPHET"),c(rmse.arima, rmse.ses, rmse.holt, rmse.prophet))
colnames(table) = c("Method", "RMSE")
table #find the lowest
print('The best forecasting method is')
table[which.min(table[,2]),1]
# The best forecasting method is Prophet, as it has the lowest RMSE of 183.64 compared to others.


# RMSE (Root Mean Squared Error) evaluates the average magnitude of the forecast errors.
# MAE (Mean Absolute Error) measures the average absolute difference between predictions and actual values.
# MAPE (Mean Absolute Percentage Error) reflects the average percentage difference, useful for scale-independent comparison.


# ARIMA forecast accuracy: RMSE, MAE, and MAPE (%)

# Extract forecasted values from ARIMA model
fore.arima <- as.numeric(pred.arima$mean)
# Calculate forecast accuracy metrics for ARIMA
rmse.arima <- rmse(ts.test, fore.arima) # Root Mean Squared Error (measures magnitude of error)
mae.arima <- mae(ts.test, fore.arima) # Mean Absolute Error (average absolute difference)
mape.arima <- mape(ts.test, fore.arima) * 100 # Mean Absolute Percentage Error (as a percentage)

# ARIMA:
# - RMSE = 216.35, MAE = 145.95, MAPE = 17.72%
# - Offers stable and accurate forecasts overall, with the lowest MAPE among all models.

# SES forecast accuracy: RMSE, MAE, and MAPE (%)
fore.ses <- as.numeric(ses.test$mean)

rmse.ses <- rmse(ts.test, fore.ses)
mae.ses <- mae(ts.test, fore.ses)
mape.ses <- mape(ts.test, fore.ses) * 100

# SES (Simple Exponential Smoothing):
# - RMSE = 276.41, MAE = 198.81, MAPE = 24.73%
# - Performs moderately, but all errors are higher than ARIMA.

# Holt forecast accuracy: RMSE, MAE, and MAPE (%)
fore.holt <- as.numeric(holt.test$mean)

rmse.holt <- rmse(ts.test, fore.holt)
mae.holt <- mae(ts.test, fore.holt)
mape.holt <- mape(ts.test, fore.holt) * 100

# HOLT:
# - RMSE = 534.97, MAE = 423.81, MAPE = 55.40%
# - This model has the highest error metrics, indicating poor forecasting performance.

# Prophet forecast accuracy: RMSE, MAE, and MAPE (%)
rmse.prophet <- rmse(ts.test, fore.prophet)
mae.prophet <- mae(ts.test, fore.prophet)
mape.prophet <- mape(ts.test, fore.prophet) * 100

# Prophet:
# - RMSE = 183.64 (lowest), MAE = 162.67, MAPE = 29.57%
# - Achieves the lowest RMSE, but has a relatively high MAPE.
# - Indicates accurate overall forecasts, but may have larger errors in specific periods.

# Combine results into a summary table
performance_table <- data.frame(
  Model = c("ARIMA", "SES", "HOLT", "Prophet"),
  RMSE = c(rmse.arima, rmse.ses, rmse.holt, rmse.prophet),
  MAE = c(mae.arima, mae.ses, mae.holt, mae.prophet),
  MAPE = c(mape.arima, mape.ses, mape.holt, mape.prophet)
)

print(performance_table)

# Identify the best model
best_model <- performance_table[which.min(performance_table$RMSE), "Model"]
cat("The best forecasting model is:", best_model, "\n")

# Conclusion:
# - If prioritizing percentage accuracy and consistency → ARIMA is preferred.
# - If minimizing overall error magnitude (RMSE) → Prophet performs best.

##### Future Forecast (trained on whole data) for half year #####

# future dates (half year)
future_dates <- seq(as.Date("2025-01-01"), by = "month", length.out = 24)

# Create forecast using models trained on full data
full_arima <- auto.arima(tsmc_ts)
forecast_arima_future <- forecast(full_arima, h = 24)

# Plot the forecast result
autoplot(forecast_arima_future) +
  ggtitle("Final ARIMA Forecast for TSMC") +
  xlab("Date") + ylab("Price") +
  theme_minimal()

full_ses <- ses(tsmc_ts, h = 24)
full_holt <- holt(tsmc_ts, h = 24)

# Prophet model on full dataset
df_full_prophet <- tsmc_df %>% rename(ds = date, y = price)
m_full <- prophet(df_full_prophet, changepoint.prior.scale = 0.1)
future_full <- make_future_dataframe(m_full, periods = 24, freq = "month")
forecast_prophet_full <- predict(m_full, future_full)

# Combine forecasts
df_forecast_future <- data.frame(
  Date = future_dates,
  ARIMA = as.numeric(forecast_arima_future$mean),
  SES = as.numeric(full_ses$mean),
  HOLT = as.numeric(full_holt$mean),
  Prophet = as.numeric(tail(forecast_prophet_full$yhat, 24))
)

# Actual data
df_actual <- tsmc_df %>% rename(Date = date, Actual = price)

# Combine forecast and actual data
df_plot <- full_join(df_actual, df_forecast_future, by = "Date")

# Reshape for plotting
df_long <- melt(df_plot, id.vars = "Date")

# Plot
ggplot(na.omit(df_long), aes(x = Date, y = value, color = variable)) +
  geom_line(linewidth = 1.1) +
  labs(
    title = "TSMC Actual & Forecasted Stock Prices (2015–2026)",
    x = "Date", y = "Price", color = "Legend"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )

############################# Business Implications ##############################
# 1.Stable Growth Trend Reinforces Investor Confidence                           #   
# Based on historical data and forecasts using time series models such as ARIMA  #
# and Prophet, TSMC’s stock price has shown a strong and consistent upward trend # 
# since 2015.Despite temporary corrections,the overall growth trajectory remains #
# intact, which reinforces its appeal to long-term and risk-averse investors.    #
#                                                                                #
# 2.Rising Demand from AI and High-Performance Computing                         #
# The forecast indicates continued stock price growth through 2025–2026, driven  #
# by increasing demand for AI and high-performance chip manufacturing. As key    #
# clients like Apple and NVIDIA deepen their reliance on TSMC’s advanced nodes,  #
# the company is expected to maintain its leadership in the global semiconductor # 
# supply chain.                                                                  #
#                                                                                #
# 3.Short-Term Volatility Requires Risk Awareness                                #
# While Prophet achieved the lowest RMSE, it also showed higher MAPE, indicating # 
# potential forecast deviations in specific periods. This suggests that while    #
# long-term trends are reliable, short-term fluctuations may still occur due to  #
# external shocks such as geopolitical tensions or supply chain disruptions.     #
##################################################################################

################################ Recommendations #################################
# 1.Investment Strategy: Focus on Medium-to-Long-Term Positioning                #
# Given the clear long-term upward trend, investors are advised to adopt a       #
# medium to long-term strategy (6 to 24 months), rather than react to short-term #
# volatility.Dollar-cost averaging or buying on dips may be effective approaches #
# to manage entry risk.                                                          #
#                                                                                #
# 2.Corporate Strategy: Continue Investing in Advanced Process Technologies      #
# As forecasted growth is tied to cutting-edge chip production, TSMC should      #
# continue to invest heavily in next-generation to sustain its technological     #
# advantage and protect high profit margins.                                     #
#                                                                                #
# 3.Risk Management: Leverage Multi-Model Forecasting and Scenario Planning      #
# While Prophet performs well in terms of overall error, ARIMA offers superior   # 
# percentage accuracy (lowest MAPE). It is recommended to build a multi-model    #
# monitoring system combining both Prophet and ARIMA, and to regularly update    #
# model parameters while incorporating macroeconomic indicators for scenario     #
# -based planning.                                                               #
# external shocks such as geopolitical tensions or supply chain disruptions.     #
##################################################################################