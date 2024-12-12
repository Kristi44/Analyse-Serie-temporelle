"Ajouter le fichier README.md"
# README for Time Series Analysis in R

This document explains the steps and methods used for analyzing and modeling the given time series data using R.

---

## 1. **Setup and Data Preprocessing**
- **Libraries Required**:
  - `forecast`: For ARIMA modeling and forecasting.
  - `tseries`: For statistical tests like ADF and PP.
  - `Kendall`: For trend analysis using the Mann-Kendall test.

- **Data Import**:
  - The time series data is read using `read.table()` with columns `t` (time) and `Xt` (observations).
  - A subset of the first 100 observations is extracted for analysis.

- **Visualization**:
  - Plots are created to visualize the data and identify patterns such as seasonality and trends.

---

## 2. **Stationarity Analysis**
- **ACF Analysis**:
  - The autocorrelation function of the series `Xt` is analyzed.
  - Observations indicate non-stationarity as ACF does not decay to zero.

- **Statistical Tests**:
  - Augmented Dickey-Fuller (ADF) and Phillips-Perron (PP) tests are used to assess stationarity.

---

## 3. **Trend Estimation**
- **Moving Average Smoothing**:
  - Function `moymob()` is implemented to calculate moving averages with a specified window size (`q`).
  - Results are saved in a file `trend_estim.txt` and plotted.

---

## 4. **Seasonality Estimation**
- **Seasonal Components**:
  - Function `Estimsaison()` calculates seasonal effects using residuals after trend removal.
  - Seasonal estimates are saved in `saison_estim.txt`.

---

## 5. **Deseasonalization and Stationarity Validation**
- **Deseasonalized Series**:
  - The series `Yt` is created by removing trend and seasonality from `Xt`.
  - Tests and plots confirm stationarity.

---

## 6. **ARMA Modeling**
- **Order Selection**:
  - PACF and ACF plots guide the selection of AR and MA orders.
  - Box-Jenkins algorithm (`auto.arima()`) suggests optimal orders.

- **Model Selection**:
  - Various AR and MA models are tested using BIC.
  - Best ARMA model is identified as `(3,5)`.

- **Prediction**:
  - Forecasts for 100 future observations are saved in `predictionXtArma.txt`.

---

## 7. **SARIMA Modeling**
- **Differencing for Seasonality**:
  - Seasonal differencing is applied to the series.

- **Model Identification**:
  - PACF and ACF plots help identify seasonal AR and MA components.
  - Best SARIMA model is `(0,0,1)(1,1,0)[12]` based on AIC.

- **Model Validation**:
  - Residuals are checked for white noise using the Ljung-Box test.

- **Prediction**:
  - Forecasts are saved in `predictionSarimaXt.txt`.

---

## 8. **Key Functions and Outputs**
- **Custom Functions**:
  - `moymob()`: Computes moving averages for trend estimation.
  - `Estimsaison()`: Calculates seasonal components.

- **Output Files**:
  - `trend_estim.txt`: Contains trend estimates.
  - `saison_estim.txt`: Contains seasonal estimates.
  - `predictionXtArma.txt`: Forecasts from ARMA modeling.
  - `predictionSarimaXt.txt`: Forecasts from SARIMA modeling.

---

## 9. **Conclusions**
- ARMA(3,5) and SARIMA(0,0,1)(1,1,0)[12] models were the most suitable for the given data.
- SARIMA outperformed ARMA in terms of RMSE and seasonal adjustment.
- Residual analysis confirmed the adequacy of the selected SARIMA model.

---

For further details or improvements, please refer to the R code provided in this project.

