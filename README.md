# 📌 About the Project 
This project was conducted as part of the **Time Series Analysis course** at **Rennes School of Business**. It focuses on analyzing the historical stock prices of **Taiwan Semiconductor Manufacturing Company (TSMC, 2330.TW)** — the world’s largest and most advanced semiconductor foundry.

Given TSMC’s key role in global technology supply chains and Taiwan’s economy, we aimed to explore long-term price trends and forecast future stock movements using multiple time series models. The insights drawn from this analysis have implications for investment strategies, economic forecasting, and risk management in the tech sector.

## Team member
*   **[Kuan-Yu HOU](https://github.com/DoreenHou)**
*   **[Ping-Yao WANG](https://github.com/Clementtnk)**
*   **[Annie HUNG](https://github.com/RUEI-CHIEH)**
*   **Yi-Ling Li**

## 🔍 Methodology

1. **Data Collection**: Monthly closing price data (2015–2024) from Yahoo Finance.
2. **Preprocessing**: Cleaned and transformed for time series analysis.
3. **Exploratory Analysis**: Visual trend inspection, KPSS and ACF tests to assess stationarity and seasonality.
4. **Modeling**:
   - **Simple/Weighted Moving Averages (SMA/WMA)**
   - **ARIMA**
   - **Exponential Smoothing (SES, Holt)**
   - **Facebook Prophet**
5. **Model Evaluation**: Forecast accuracy measured with **RMSE**, **MAE**, and **MAPE** using a hold-out test set of 30 months.
6. **Selection**: The best-performing model was identified based on lowest RMSE.

---

### ✅ Conclusions

- **TSMC’s stock price shows strong long-term growth**, supported by trends in AI and high-performance computing demand.
- **Prophet had the best RMSE**, while **ARIMA offered the best MAPE**, making both valuable depending on forecasting goals.
- **Volatility remains a short-term risk**, reinforcing the need for multi-model forecasting and scenario planning.
- Recommended investment horizon: **medium to long-term (6–24 months)**.

---

### 🛠️ Tools and Technologies

- **Language**: R
- **Libraries**: `quantmod`, `forecast`, `prophet`, `tseries`, `Metrics`, `ggplot2`, `lubridate`, `TTR`, `dplyr`, `reshape2`
- **Models**: ARIMA, SES, Holt, Prophet, SMA, WMA
- **Data Source**: Yahoo Finance
- **Forecast Horizon**: 30 months for evaluation, 6–12 months for future projection

