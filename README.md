# Final Project Shiny Application - Unemployment Immigration Visualization
This is a shiny app analyzing and visualizing how immigration and unemployment is correlated across the US. The app allows users to filter, explore, and compare datasets interactively.

## Repository Structure

```text
Code and documentation (in main branch)

main/
├── shiny.R                         # Main Shiny application file
├── merge_data_employment.Rmd       # Merge unemployment datasets from different states
├── merge_data_migration            # Updated migration merging script
├── merge_irs_data.Rmd              # Rename merge_irs_data.html → merge_irs_data.Rmd
└── README.md                       # Documentation

Datasets (in master branch)

master/
└── finalproject/
    ├── nat-datacleaning.Rmd                      # Cleaned unemployment dataset (2011–2022), removed NAs
    ├── unemployment_cleaned.csv                  # Unemployment dataset without duplicate columns or NAs
    ├── unemployment_yearly_2011_2022.csv         # Cleaned dataset aggregated by year
    └── state-employment-data/
        └── migration_flow_combined_2011_2022.csv # Merged migration dataset (2011–2022)

## Requirements

R version 4.4 or higher.

Required R packages:
- shiny
- readr
- dplyr
- tidyr
- ggplot2
- maps

Install them with:

install.packages(c(
  "shiny",
  "readr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "maps"
))

## How to Run The App Locally
1. Clone the repository:
   git clone https://github.com/myralyu-lgtm/datasci306-final-project.git
   
2. Open RStudio.

3. Set the working directory to the project folder:
   setwd("path/to/project")

4. Run the application:
   shiny::runApp()

## Deployment
library(rsconnect)

rsconnect::deployApp()

## Features
- Interactive state-level immigration-unemployment map
- Year and states filters
- Dynamic comparison across states
- Downloadable CSV summaries

## Data Source
- IRS SOI Immigration Data: https://www.irs.gov/statistics/soi-tax-stats-migration-data
- FRED unemployment dataset: https://fred.stlouisfed.org/release?rid=112
