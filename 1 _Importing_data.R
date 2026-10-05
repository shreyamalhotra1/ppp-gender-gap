# ==============================================================================
# PPP Gender Gap — Data Preparation
# Kamal & Shreya (2026)
# Loading, Data cleaning, variable construction, and sample selection
####Loads and stacks 13 SBA PPP CSV files, constructs NAICS 2-digit 
#### industry codes, and creates year subsets (2020, 2021).
# ==============================================================================
# Data source: SBA PPP public loan-level dataset
# Downloaded from: https://www.sba.gov/funding-programs/loans/covid-19-relief-options/
#                paycheck-protection-program/ppp-data
# ==============================================================================
# Install and load readr package
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("stringr")
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

#set working directory
getwd()
setwd("C:\\Users\\shrey\\Downloads")


#data loading
df1 <- read_csv("Data_Gender Gaps\\public_up_to_150k_1_240930.csv")
df2 <- read_csv("Data_Gender Gaps\\public_up_to_150k_2_240930.csv")
df3 <- read_csv("Data_Gender Gaps\\public_up_to_150k_3_240930.csv")
df4 <- read_csv("Data_Gender Gaps\\public_up_to_150k_4_240930.csv")
df5 <- read_csv("Data_Gender Gaps\\public_up_to_150k_5_240930.csv")
df6 <- read_csv("Data_Gender Gaps\\public_up_to_150k_6_240930.csv")
df7 <- read_csv("Data_Gender Gaps\\public_up_to_150k_7_240930.csv")
df8 <- read_csv("Data_Gender Gaps\\public_up_to_150k_8_240930.csv")
df9 <- read_csv("Data_Gender Gaps\\public_up_to_150k_9_240930.csv")
df10 <- read_csv("Data_Gender Gaps\\public_up_to_150k_10_240930.csv")
df11 <- read_csv("Data_Gender Gaps\\public_up_to_150k_11_240930.csv")
df12 <- read_csv("Data_Gender Gaps\\public_up_to_150k_12_240930.csv")
df13 <- read_csv("Data_Gender Gaps\\public_150k_plus_240930.csv")

# data stacking
df_stacked <- rbind(df13,df1, df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

#creating data frame with NAICS description
naics_descriptions <- data.frame(
  naics_2digit = c("11", "21", "22", "23", "31", "32", "33", "42", "44", "45", 
                   "48", "49", "51", "52", "53", "54", "55", "56", "61", "62", 
                   "71", "72", "81", "92"),
  industry_name = c("Agriculture", "Mining", "Utilities", "Construction", 
                    "Manufacturing", "Manufacturing", "Manufacturing", 
                    "Wholesale Trade", "Retail Trade", "Retail Trade",
                    "Transportation", "Transportation", "Information", 
                    "Finance/Insurance", "Real Estate", "Professional Services",
                    "Management", "Administrative", "Educational Services",
                    "Healthcare", "Arts/Entertainment", "Accommodation/Food",
                    "Other Services", "Public Administration")
) 


#creating NAICS_2 digit variable
df_stacked <- df_stacked %>%
  mutate(
    NAICSCode = as.character(NAICSCode),
    naics_2digit = case_when(
      is.na(NAICSCode) ~ NA_character_,
      NAICSCode == "" ~ NA_character_,
      nchar(NAICSCode) < 2 ~ str_pad(NAICSCode, width = 2, pad = "0"),
      TRUE ~ str_pad(substr(NAICSCode, 1, 2), width = 2, pad = "0")
    )
  )

#merging df_stacked and naics description
df_stacked <- df_stacked %>%
  left_join(naics_descriptions, by = "naics_2digit")

#creating year variable
df_stacked$year <- format(as.Date(df_stacked$DateApproved, format="%m/%d/%Y"),"%Y")

#creating subsets
df_stacked_year2020 <- subset(df_stacked, year == year[2020])
df_stacked_year2020
df_stacked_year2021 <- subset(df_stacked, year == year[2021])
df_stacked_year2021

#to check all variable names
names(df_stacked)

# Save cleaned datasets
saveRDS(df_stacked_year2020,  "df_stacked_year2020.rds")
saveRDS(df_stacked_year2021,  "df_stacked_year2021.rds")


