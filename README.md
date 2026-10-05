# Gender Gaps in PPP Lending

Replication code for **"Gender Gaps in PPP Lending"** 

## About
This project examines gender disparities in the SBA Paycheck Protection Program (PPP) across loan size, approval timing, and lender type using the full public loan-level dataset of approximately 11.3 million loans from 2020–2021.

## Data
**Source:** SBA PPP Public Data — https://data.sba.gov/dataset/ppp-foia

Download all 13 CSV files to a local folder and update the `setwd()` path in Scripts 1, 4, and 5 before running. The raw CSV files are not included in this repository due to size constraints (each file exceeds GitHub's 100 MB limit).

**Additional data required for Script 4:**
- `Number_of_firms_2020.xlsx` and `Number_of_firms_2021.xlsx` — Census Bureau data on the number of female- and male-owned firms by state (used to compute representation and access gaps).

## Scripts
| File | Description |
|------|-------------|
| `1_Importing_data.R` | Loads and stacks 13 SBA CSV files; constructs 2-digit NAICS codes; creates year variable |
| `2_Data_Boundaries.R` | Missing data audit; small business profiling; industry-level descriptives; gender gap by industry |
| `3_Granular_Analysis.R` | State-level gender gap metrics; choropleth maps; year-on-year gap changes; heatmaps |
| `4_Indices.R` | Computes four GGI components by state and year: Early Access Gap, Loan Size Gap, Representation Gap, Access Gap |
| `5_Regression_Analysis_1.R` | Loan-level OLS and LPM regressions (loan amount gap, early approval gap, gender observability); robustness checks |
| `6_Regression_Analysis_2.R` | State-panel regressions of GGI indices on unknown-gender share and unemployment rate with state and year fixed effects |

## Authors
 Lillian Kamal & Shreya Malhotra
