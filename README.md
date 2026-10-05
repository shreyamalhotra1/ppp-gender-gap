# Gender Gaps in PPP Lending

Replication code for **"Gender Gaps in PPP Lending"** by Shreya Malhotra and Lillian Kamal.

## About
This project examines gender disparities in the SBA Paycheck Protection Program (PPP) 
across loan size, approval timing, and lender type using the full public loan-level dataset 
of 11.3 million loans from 2020–2021.

## Data
**Source:** SBA PPP Public Data — https://data.sba.gov/dataset/ppp-foia  
Download all CSV files to a local folder and update the `setwd()` path in Script 1 
before running.

## Scripts

| `1_Importing_data.R` | Loads and stacks 13 SBA CSV files, constructs 2-digit NAICS industry codes, creates year variable |
| `2_Data_Boundaries.R` | Missing data audit, small business profiling, industry-level descriptives, gender breakdown |
| `3_Granular_Analysis.R` | State-level gender gap indices, choropleth maps, year-on-year gap changes |

## Authors
 Lillian Kamal & Shreya Malhotra
