# UCI Air Quality Analysis in Rome (March 2004 - April 2005)

## Project Overview

This project analyzes the [UCI Air Quality Dataset](https://archive.ics.uci.edu/dataset/360/air+quality), which contains hourly air pollution and weather data from Rome, Italy (March 2004–February 2005). The goal is to explore relationships between pollutants, sensor responses, and environmental factors, and to build predictive models for pollutant concentrations. Key tasks include exploratory data analysis (EDA), sensor calibration assessment, time-series forecasting, and machine learning model development.

------------------------------------------------------------------------

## Dataset Description

-   **Source**: Multisensor devices in a polluted urban area (road level) in Rome, Italy.
-   **Time Range**: March 2004 to February 2005 (9358 hourly records).
-   **Variables**:
    -   **Pollutants**: CO(GT), NO₂(GT), NOx(GT), C₆H₆(GT), NMHC(GT).
    -   **Sensor Responses**: PT08.S1 to PT08.S5 (metal oxide sensor readings).
    -   **Environmental Factors**: Temperature (T), Relative Humidity (RH), Absolute Humidity (AH).
    -   **Metadata**: Timestamp (Date/Time).
-   **Missing Values**: Denoted by `-200`.

------------------------------------------------------------------------

## Key Objectives

1.  Investigate sensor calibration accuracy and cross-sensitivities.
2.  Analyze air quality trends using the Air Quality Index (AQI).
3.  Predict CO(GT) levels using regression and time-series models.
4.  Forecast NO₂ concentrations using hybrid SARIMAX and domain knowledge models.
5.  Compare machine learning techniques for pollutant prediction.

------------------------------------------------------------------------

## Key Analysis Questions

### a. Sensor Calibration & Air Quality

-   How do sensor responses correlate with true pollutant concentrations?
-   Do environmental factors bias sensor readings?
-   What are seasonal trends in AQI, and which pollutants dominate poor air quality?

### b. CO Prediction

-   Can temperature, humidity, and time predict CO(GT)?
-   Does adding sensor data improve prediction accuracy?
-   How do linear (Linear Regression) and nonlinear (Random Forest) models perform?

### c. NO₂ Time-Series Analysis

-   Is the NO₂ series stationary?
-   How do environmental variables and SAPRC-derived biochemical outputs correlate with NO₂?
-   Does a hybrid SARIMAX + SAPRC model outperform SARIMAX alone?

### d. Machine Learning Comparison

-   Which ML technique (e.g., KNN, Random Forest) performs best for pollutant prediction?
-   Are time-series approaches superior to non-time-series methods?
-   Can unsupervised learning be applied to this dataset?

## Planned Modeling Approaches

| **Task** | **Approach** |
|------------------|-----------------------------------------------------|
| **Sensor Calibration** | Pearson/Spearman correlations, residual analysis. |
| **AQI Analysis** | Time-series categorization into health risk levels. |
| **CO Prediction** | Time encoding (hour of day), model comparison (Linear vs. Random Forest). |
| **NO₂ Hybrid Model** | SARIMAX with SAPRC-derived variables. |
| **ML Comparison** | ML techniques for regression and classification of pollutants. |

------------------------------------------------------------------------

## Results

For details, please consult the [final report](reports/Final%20Report%20UCI%20Air%20Quality%20Analysis%20in%20Rome.pdf).


## Authors

**Duc Do**

**Thuan Khang Dinh**

**Parth A. Pansara**

**Felix Vo**
