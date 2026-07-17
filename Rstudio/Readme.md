# Equity Report Generation Workflow

This folder contains the R workflow used to extract, process and generate automated equity research reports.

The process is divided into two main stages:

---

## 1. Data Extraction & Processing

**File:** `data_extraction.R`

This script is responsible for extracting financial data from APIs and processing the information required for the equity reports.

Main tasks:

- API data extraction
- Data processing and preparation
- Financial data structuring for report generation

---

## 2. Equity Report Generation

**File:** `equity_report_generator.Rmd`

This R Markdown file uses the processed data to generate structured equity research reports in PDF format.

The R Markdown workflow is responsible for organizing, formatting and presenting the extracted financial information into a final report.

---

# Report Features

The generated equity reports include:

- Data Overview & Company Profile

- Revenue by Product & Regions

- Income Statement, Balance Sheet, Cash Flow & Annual Report (SEC 10-k)

- Annual Ratios & Metrics

- Dividends

---

## Workflow
```
Financial APIs
      ↓
Data Extraction & Processing (R)
      ↓
Equity Report Generation (R Markdown)
      ↓
PDF Equity Research Report
```





