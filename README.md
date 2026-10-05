River Water Discharge Forecasting & Flood Risk Analysis

Project Overview
This project applies advanced Time Series Analysis (TSA) to predict river water discharge levels and estimate peak flood risks. By modeling historical daily discharge rates and identifying seasonal patterns, the objective is to build a reliable SARIMA-based forecasting system to anticipate potential flood events 30 days in advance.   

Data Preprocessing & EDA
Imputation: 
Handled missing daily discharge values using linear interpolation, adhering to standard hydrological data practices, with median fallback for edge cases.   
Feature Engineering: 
Extracted day, month, and year features from chronological dates to evaluate weekly and monthly seasonality patterns.   
Visualizations: 
Plotted historical timelines, distribution histograms, and average monthly discharge trends using ggplot2 to establish baseline hydrological behavior.   

Methodology
Stationarity Testing: 
Conducted Augmented Dickey-Fuller (ADF) tests. Applied a 7-day seasonal differencing lag to achieve stationarity, verified via ACF and PACF plots.   
SARIMA Modeling: 
Designed and evaluated four candidate SARIMA models (e.g., SARIMA(1,0,1)(1,1,1)[7], SARIMA(2,0,1)(2,1,1)[7]).   
Model Selection & Diagnostics: 
Selected the optimal forecasting model based on the lowest AIC and BIC scores. Verified model reliability by checking residual autocorrelation using the Ljung-Box test.   
Peak Flood Forecasting: 
Isolated daily peak discharge metrics mapped against actual flood events, and deployed an auto.arima model with seasonality to forecast peak flood loads 30 days ahead.   

Technologies & Libraries
Language: R
Libraries: forecast, tseries, ggplot2, dplyr, lubridate
