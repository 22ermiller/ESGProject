# Analyzing Retirement Plans with an ESG

This repository contains all the code and documents relating to my BYU master's project. The goal of the project was to analyze the risk associated with retirement plans under a stochastic framework via a cascade-style Economic Scenario Generator (ESG).

<p align="center">

<img src="documents/www/ESG_diagram.png" width="50%"/>

</p>

## Important Files and Structure

### To Run Simulations:

 - ESG_draws.R: function to get simulated draws from all pieces of ESG
 - portfolio_sim_functions.R: functions to simulate a portfolio

### Model Development Scripts (01-08)

These files contain all the exploratory work done to create the final models implemented in the ESG

### 09_ESG.R

This script implements all pieces of the ESG and applies simulated draws to hypothetical retirement plans consisting of variable annuitization and spending rates.

### /models

These are saved fitted model objects that make up the ESG

### /raw_data /data /data_cleaning

These folders contain all files and scripts related to the data used to fit the ESG. Interest rate and inflation data was obtained from the Federal Reserve Bank of St. Louis Website (<https://fred.stlouisfed.org/>) and Stock Market data was obtained from the `tidyfinance` package

### functions.R

This file contains all the helper functions for the ESG

### /documents

Contains important documents/presentation files related to the presentation of the project. 

- ESGPaper.pdf is the final paper describing the project 
- DefensePresentation.pdf is the presentation file used for project defense

## Updating the ESG

Steps to be taken to update ESG and models with up-to-date data:

1.  Read in New Data Files
    -   Put new inflation and interest rate raw files in /raw_data
        -   where to go to download necessary data is in the comments of the files in /data_cleaning
    -   New Mortality Table will need to be manually recorded in /data_cleaning/mortality.R
2.  Re-run the following scripts:
    -   04_inflation_final.R
    -   06_garch_model.R
    -   07_stock_returns.R
    -   08_yield_curve.R
