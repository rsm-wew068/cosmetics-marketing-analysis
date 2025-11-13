# Cosmetics Marketing Analysis

A comprehensive R-based customer segmentation and lifetime value analysis project for cosmetics marketing data.

## Overview

This project performs advanced customer analytics including:
- **RFM Analysis**: Recency, Frequency, and Monetary value segmentation
- **Customer Segmentation**: Both statistical clustering and rule-based segmentation
- **Customer Lifetime Value (CLV)**: Predictive modeling for customer value estimation
- **Marketing Strategy**: Data-driven approach to customer retention and acquisition

## Features

### 1. Data Preparation
- Transaction data processing and cleaning
- Customer-level aggregation (Recency, Frequency, Monetary, Seniority)
- Data visualization and summary statistics

### 2. Customer Segmentation

#### Statistical Clustering (K-means)
- 10-group segmentation based on RFM metrics
- Multi-dimensional visualization (5D bubble charts)
- Group attribute analysis

#### Rule-Based Segmentation
- **N1**: New Customers
- **N2**: New Potential Customers
- **R1**: Key Customers
- **R2**: Core Customers
- **S1**: Drowsy Customers
- **S2**: Half-Asleep Customers
- **S3**: Deep-Sleep Customers

### 3. Predictive Modeling

#### Retention Model
- Logistic regression to predict customer retention
- Accuracy: 85.19%
- AUC: 87.92%

#### Revenue Model
- Linear regression to predict purchase amounts
- R²: 0.713

### 4. Customer Lifetime Value (CLV)
- Discounted cash flow approach
- Group-level CLV comparison

### 5. Marketing Strategy
- Expected return calculation for marketing campaigns
- Target customer selection
- ROI optimization

## Requirements

### R Packages
```r
install.packages(c("pacman", "magrittr", "readr", "caTools", "ggplot2", 
                  "dplyr", "vcd", "ROCR", "plotly", "chorddiag"))
```

Or use pacman to load all packages:
```r
library(pacman)
pacman::p_load(magrittr, readr, caTools, ggplot2, dplyr, vcd, ROCR, plotly, chorddiag)
```

### Data Format
The script expects a CSV file with the following columns:
- `date`: Transaction date
- `type`: Transaction type (should include "purchase")
- `prod`: Product identifier
- `cat`: Category
- `code`: Product code
- `brand`: Brand name
- `price`: Transaction price
- `cid`: Customer ID
- `session`: Session identifier

## Usage

1. **Prepare your data**: Place your transaction data CSV file in the appropriate directory (default: `C:/data/2022-Jan.csv`)

2. **Update file paths**: Modify the file path in the script to match your data location:
   ```r
   X = read_csv("path/to/your/data.csv")
   ```

3. **Run the analysis**: Execute the R script section by section:
   - Section 1: Data preparation
   - Section 2: Hierarchical cluster analysis
   - Section 3: Rule-based segmentation
   - Section 4: Model building
   - Section 5: CLV estimation
   - Section 6-7: Marketing strategy

## Key Metrics Explained

### RFM Metrics
- **Recency (R)**: Days since last purchase (Activity indicator)
- **Frequency (F)**: Number of purchases (Loyalty indicator)
- **Monetary (M)**: Average purchase amount (Contribution indicator)
- **Seniority (S)**: Days since first purchase (Tenure indicator)

### Customer Segments
- **New Customers (N1, N2)**: Recently acquired customers
- **Regular Customers (R1, R2)**: Active, valuable customers
- **Sleeping Customers (S1, S2, S3)**: Inactive customers at different stages

### CLV Formula
```
CLV = g × Predicted Revenue × Σ(Retention Rate / (1 + Discount Rate))^t
```
Where:
- `g` = Profit margin (default: 0.5)
- `t` = Time period (0 to N, default: 5 years)
- Discount rate = 10%

## Outputs

1. **Visualizations**:
   - Daily transaction histograms
   - RFM distribution plots
   - Customer segment bubble charts
   - Group size trend bar charts
   - CLV distribution histograms
   - Interactive chord diagrams for customer flow

2. **Data Files**:
   - Saved R data files with processed customer and transaction data
   - Customer segment summaries

3. **Model Results**:
   - Retention probability predictions
   - Revenue predictions
   - Customer lifetime value estimates

## Notes

- **Platform Compatibility**: 
  - On Windows: Use `windows()` to open new plot windows
  - On Mac: Replace `windows()` with `quartz()`

- **Data Requirements**: 
  - Minimum 2 transactions per customer needed to calculate purchase cycle
  - Historical data spanning multiple periods recommended for trend analysis

## Analysis Workflow

1. **Data Cleaning** → Filter purchases, convert dates
2. **Customer Aggregation** → Calculate RFM metrics
3. **Segmentation** → Statistical and rule-based grouping
4. **Modeling** → Build retention and revenue models
5. **Prediction** → Estimate CLV and future behavior
6. **Strategy** → Identify targets and calculate ROI

## Applications

This analysis enables:
- **Customer Retention**: Identify at-risk customers (S1, S2, S3)
- **Revenue Optimization**: Focus on high-value segments (R1, R2)
- **Marketing ROI**: Calculate expected returns for campaigns
- **Resource Allocation**: Prioritize customer segments based on CLV
- **Strategic Planning**: Understand customer lifecycle and migration patterns

## References

- dplyr package documentation
- ggplot2 visualization guide
- RFM analysis methodology
- Customer Lifetime Value calculation methods

