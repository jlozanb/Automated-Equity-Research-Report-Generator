# Automated Equity Research Report Generator

## Overview

End-to-end equity research reporting system developed to automate the generation of financial research reports.

The project retrieves financial data from external APIs using R, processes and analyses market information, and generates structured equity research reports using R Markdown and LaTeX.

Additionally, **Beta BullishReports**, an interactive R Shiny application, was developed to provide users with a reporting interface including user registration, database integration, and automated report generation capabilities.

## Features

- Automated financial data extraction from APIs
- Data processing and financial analysis using R
- Reproducible report generation using R Markdown
- Professional report formatting with LaTeX
- Interactive R Shiny application (**Beta BullishReports**)
- User registration and database integration
- Automated equity research report generation

## Technologies

- R
- R Markdown
- R Shiny
- LaTeX
- SQL Database
- Financial APIs

## Workflow

1. Financial market data is retrieved from external APIs.
2. Data is processed and analysed using R.
3. Financial metrics and insights are generated.
4. The equity research report is created using R Markdown and compiled into PDF format through LaTeX.
5. Users can interact with the reporting system through the Beta BullishReports R Shiny application.

## Live Demo

The deployed R Shiny application **Beta BullishReports** is available here:

https://freebeta.shinyapps.io/Beta_BullishReports/

## Example Report

A generated equity research report example is included:

- `BAC_BankOfAmerica_Report.pdf`

This report demonstrates the automated generation of a structured equity research document based on financial data analysis.

## Project Purpose

The objective of this project was to develop an automated and reproducible workflow capable of transforming raw financial data into professional equity research reports, reducing manual reporting tasks and improving consistency.

## Repository Contents

```
## Repository Contents
├── App
│ ├── App_Rshiny.R
│ └── Readme.md (App Link)
│
├── Reports
│ ├── BAC_BankOfAmerica_Equity_Report.pdf
│ └── KO_CocaCola_Equity_Report.pdf
│ └── SBUX_Starbucks_Report.pdf
│
├── RStudio
│ └── #1_Report_Generator.R
│ └── #2_Report_Generator.Rmd
│ └── Readme.md
│
└── README.md
```

## Author

Jorge Lozano
