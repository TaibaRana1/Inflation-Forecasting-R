📊 Inflation Forecasting Using Time Series & Regularized Regression (R)
📌 Overview

This project builds and compares multiple statistical and machine learning models to forecast inflation using historical macroeconomic data (1980–2024).
The goal is to evaluate traditional time series methods against regularized regression techniques and determine the most accurate forecasting approach.


🎯 Models Implemented

Multiple Linear Regression (Baseline Model)

ARIMAX (Auto ARIMA with Exogenous Variables)

LASSO Regression

Ridge Regression

Elastic Net

Time Split:

Training: 1980–2020

Testing: 2021–2024


🔎 Key Steps

Exploratory Data Analysis (boxplots, scatter plots, correlation matrix)

Stationarity testing using ADF test

Feature selection based on p-values and correlation

10-fold cross-validation for regularized models

Residual diagnostics and performance comparison

📊 Evaluation Metrics
Models were compared using:

MSE

RMSE

MAE

A final combined visualization compares predictions from all models against actual inflation values (2021–2024).


🛠 Tools Used

R, forecast, tseries, caret, glmnet, ggplot2


💡 Highlights

Applied both time series and machine learning approaches

Addressed multicollinearity using regularization

Performed full model diagnostics and comparison

Built an end-to-end forecasting pipeline in R
