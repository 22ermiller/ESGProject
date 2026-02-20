# Analyzing Retirement Plans with an ESG

This repository contains all the code and documents relating to my BYU master's project. The goal of the project was to analyze the risk associated with retirement plans under a stochastic framework via an Economic Scenario Generator (ESG).

<p align="center">
  <img src="documents/www/ESG_diagram.png" width="50%">
</p>

## Important Files and Structure

### Model Development Scripts (01-08)

These files contain all the exploratory work done to create the final models implemented in the ESG

### 09_ESG.R

This script implements all pieces of the ESG and applies simulated draws to hypothetical retirement plans consisting of variable annuitization and spending rates.

### /models

These are saved fitted model objects that make up the ESG

### /raw_data /data /data_cleaning

These folders contain all files and scripts related to the data used to fit the ESG. Interest rate and inflation data was obtained from the Federal Reserve Bank of St. Louis Website (https://fred.stlouisfed.org/) and Stock Market data was obtained from the `tidyfinance` package

### functions.R

This file contains all the functions necessary to extract simulated draws from the ESG

### /documents

Contains important documents/presentation files related to the presentation of the project.
 - ESGPaper.pdf is the final paper describing the project
 - SRC_Presentation-pdf.pdf is the presentation file used for project defense
