# Diabetes Risk Prediction

This project involves developing a predictive model to assess the risk of diabetes using health indicators like BMI, glucose levels, and blood pressure. A real-time risk assessment app was built using Shiny, allowing users to input their health data and get diabetes risk predictions.

## Table of Contents
- [Project Motivation](#project-motivation)
- [Technologies Used](#technologies-used)
- [Data Source](#data-source)
- [Methodology](#methodology)
- [Results](#results)
- [Shiny App](#shiny-app)

## Project Motivation
This project was inspired by a family member who developed diabetes. The goal was to create a tool that could provide a quick assessment of diabetes risk using logistic regression and Recursive Feature Elimination (RFE) based on user inputs like BMI, blood pressure, and glucose levels.

## Technologies Used
- **Python** (for data preprocessing, model development)
- **Logistic Regression** (predictive model)
- **Recursive Feature Elimination (RFE)** (feature selection)
- **Shiny (R)** (for web application)
- **Pandas** (data manipulation)
- **scikit-learn** (model training and evaluation)
- **Matplotlib** (visualizations)
- **Shiny** (for developing the web interface)

## Data Source
The dataset was sourced from [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/diabetes) and consists of various health metrics related to diabetes risk.

## Methodology
1. **Data Preprocessing**: Cleaning and scaling the dataset for model input.
2. **Feature Selection**: Recursive Feature Elimination (RFE) was used to identify the most important health indicators.
3. **Modeling**: A Logistic Regression model was trained on the processed data.
4. **Evaluation**: The model's performance was evaluated using various metrics, including the AUC (Area Under Curve), achieving an AUC of **0.8349**.
5. **Deployment**: A Shiny app was created to allow real-time diabetes risk prediction based on user input.

## Results
The Logistic Regression model achieved an AUC of **0.8349**, indicating good performance. The Shiny application allows users to interactively assess their diabetes risk.

## Shiny App
The Shiny app allows users to input health metrics such as BMI, blood pressure, and glucose levels. The app outputs a diabetes risk score in real-time based on the trained model.
