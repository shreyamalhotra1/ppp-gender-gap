#Step 1 (loading dataset)
rm(list = ls())

install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages(c("tidyverse", "fixest", "modelsummary", "haven", "janitor"))

library(tidyverse)      # data wrangling
library(fixest)         # fast fixed effects regression (feols)
library(modelsummary)   # publication-quality tables
library(janitor)        # clean column names
library(lubridate)      # date handling
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)

getwd()
setwd("C:\\Users\\shrey\\OneDrive\\Desktop\\Data_Gender Gaps\\Excel files")


#data loading
df1 <- read_csv("public_up_to_150k_1_240930.csv")
df2 <- read_csv("public_up_to_150k_2_240930.csv")
df3 <- read_csv("public_up_to_150k_3_240930.csv")
df4 <- read_csv("public_up_to_150k_4_240930.csv")
df5 <- read_csv("public_up_to_150k_5_240930.csv")
df6 <- read_csv("public_up_to_150k_6_240930.csv")
df7 <- read_csv("public_up_to_150k_7_240930.csv")
df8 <- read_csv("public_up_to_150k_8_240930.csv")
df9 <- read_csv("public_up_to_150k_9_240930.csv")
df10 <- read_csv("public_up_to_150k_10_240930.csv")
df11 <- read_csv("public_up_to_150k_11_240930.csv")
df12 <- read_csv("public_up_to_150k_12_240930.csv")
df13 <- read_csv("public_150k_plus_240930.csv")

# data stacking
df_stacked <- rbind(df13,df1, df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

#clean
#get unique territories

unique_territories <- unique(df_stacked$BorrowerState)  
print(unique_territories)

us_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
               "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
               "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
               "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
               "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC")

ppp_states <- df_stacked[df_stacked$BorrowerState %in% us_states, ]


#STEP 2: BASE DATA PREP
# ============================================================

ppp_clean <- ppp_states %>%
  
  # --- Drop only zero/negative loan amounts ---
  filter(CurrentApprovalAmount > 0) %>%
  
  # --- Dependent variable ---
  mutate(ln_loan = log(CurrentApprovalAmount)) %>%
  
  # --- Gender dummies (Male = baseline) ---
  mutate(Gender_clean = case_when(
    Gender == "Female Owned" ~ "Female",
    Gender == "Male Owned"   ~ "Male",
    Gender == "Unanswered"   ~ "Unknown"
  )) %>%
  mutate(Gender_clean = factor(Gender_clean,
                               levels = c("Male", "Female", "Unknown"))) %>%
  
  # --- Parse dates, extract year and time FE ---
  mutate(DateApproved = mdy(DateApproved),
         loan_year    = year(DateApproved),
         time_fe      = format(DateApproved, "%Y-%m")) %>%
  
  # --- Jobs: set missing/negative to 0 ---
  mutate(jobs = pmax(as.numeric(JobsReported), 0, na.rm = TRUE)) %>%
  
  # --- 2-digit NAICS code ---
  # NAICSCode is numeric and always 6 digits so divide by 10000
  mutate(naics2 = if_else(!is.na(NAICSCode),
                          as.character(floor(NAICSCode / 10000)),
                          "Missing")) %>%
  
  # --- Normalize zip to 5 digits for second draw matching ---
  mutate(zip5 = str_pad(substr(str_replace(BorrowerZip, "-.*", ""),
                               1, 5), 5, pad = "0")) %>%
  
  # --- Normalize borrower name for second draw matching ---
  mutate(name_clean = tolower(str_replace_all(BorrowerName,
                                              "[^a-zA-Z0-9 ]", ""))) %>%
  
  # --- Fix encoding for lender name ---
  mutate(OriginatingLender = iconv(OriginatingLender,
                                   from = "latin1",
                                   to   = "UTF-8",
                                   sub  = "")) %>%
  
  # --- Fix corrupted credit union name ---
  mutate(OriginatingLender = if_else(
    str_detect(OriginatingLender,
               regex("tru.*fi\\s*cu", ignore_case = TRUE)),
    "TruFi CU",
    OriginatingLender
  )) %>%
  
  # --- Drop rows with missing key variables ---
  filter(!is.na(ln_loan),
         !is.na(BorrowerState),
         !is.na(time_fe),
         !is.na(Gender_clean),
         !is.na(zip5))

# ============================================================
# STEP 3: SECOND DRAW INDICATOR
# Definition: same firm received loans in BOTH 2020 and 2021
# Firm identified by normalized name + 5-digit zip
# ============================================================

# --- Deduplicate within each year ---
firms_per_year <- ppp_clean %>%
  distinct(name_clean, zip5, loan_year)

# --- Firms in 2020 and 2021 separately ---
firms_2020 <- firms_per_year %>%
  filter(loan_year == 2020) %>%
  select(name_clean, zip5)

firms_2021 <- firms_per_year %>%
  filter(loan_year == 2021) %>%
  select(name_clean, zip5)

# --- Inner join: firms appearing in BOTH years ---
firms_both <- inner_join(firms_2020, firms_2021,
                         by = c("name_clean", "zip5")) %>%
  mutate(got_both = 1L)

cat("Firms that received loans in both 2020 and 2021:",
    nrow(firms_both), "\n")

# --- Merge back and flag 2021 loan as second draw ---
ppp_clean <- ppp_clean %>%
  left_join(firms_both, by = c("name_clean", "zip5")) %>%
  mutate(second_draw = case_when(
    got_both == 1L & loan_year == 2021 ~ 1L,
    TRUE                               ~ 0L
  )) %>%
  select(-got_both)


# ============================================================
# STEP 4: LENDER TYPE CLASSIFICATION (7 categories)
# ============================================================

ppp_clean <- ppp_clean %>%
  mutate(
    lender_name_lower = tolower(OriginatingLender),
    
    lender_type = case_when(
      
      # --- Credit Unions ---
      str_detect(lender_name_lower,
                 "credit union|\\bfcu\\b") ~ "Credit Union",
      
      # --- Explicit Fintech exceptions before CDFI rule ---
      str_detect(lender_name_lower,
                 "harvest small business finance|
                  newtek small business finance|
                  crf small business loan") ~ "Fintech",
      
      # --- CDFIs ---
      str_detect(lender_name_lower,
                 "\\bcdfi\\b|community development|
                  development corporation|development company|
                  development fund|development center|
                  microenterprise|microloan|micro loan|
                  small business loan company|small business finance|
                  opportunity fund|opportunity finance|
                  impact fund|impact capital|
                  neighborhood lending|neighborhood fund|
                  economic development|
                  black business|african american.*capital|
                  hispanic.*capital|latino.*capital|
                  native.*capital|tribal.*lending|
                  women.*capital|minority.*capital|
                  community advantage|\\bcdc\\b|
                  certified development|community reinvestment|
                  dreamspring|lendistry|prestamos|
                  accompany capital|b:side") ~ "CDFI",
      
      # --- Fintechs ---
      str_detect(lender_name_lower,
                 "kabbage|bluevine|fundbox|funding circle|
                  readycap|itria|celtic bank|cross river|
                  webbank|square capital|intuit financing|
                  lending club|lendingclub|fountainhead|
                  tab bank|bayfirst|a10capital|liberty sbf|
                  amur equipment|american lending center|
                  live oak|coastal community bank|
                  northeast bank|on deck|ondeck|
                  credibly|smartbiz|lendio|
                  biz2credit|rapid finance") ~ "Fintech",
      
      # --- Big Banks ---
      str_detect(lender_name_lower,
                 "bank of america|jpmorgan|jp morgan|
                  wells fargo|citibank|u\\.s\\. bank|us bank|
                  td bank|pnc bank|truist|
                  citizens bank.*national|huntington national|
                  regions bank|keybank|key bank|bmo bank|
                  fifth third|manufacturers and traders|
                  zions bank|capital one.*national|
                  comerica|santander bank|
                  hsbc bank|american express.*bank") ~ "Big Bank",
      
      # --- Savings Banks ---
      str_detect(lender_name_lower,
                 "savings bank|savings association|
                  savings institution|federal savings|
                  savings & loan|savings and loan|
                  \\bfsb\\b|\\bfsa\\b") ~ "Savings Bank",
      
      # --- Farm/Ag Lenders ---
      str_detect(lender_name_lower,
                 "farm credit|agricultural credit|
                  agcredit|farm bureau|\\baca\\b|compeer") ~ "Farm/Ag Lender",
      
      # --- Everything else ---
      TRUE ~ "Community/Regional Bank"
    )
  ) %>%
  select(-lender_name_lower)

ppp_clean <- ppp_clean %>%
  mutate(BusinessType = iconv(BusinessType, 
                              from = "latin1", 
                              to   = "UTF-8", 
                              sub  = "")) %>%
  mutate(BusinessType = case_when(
    str_detect(BusinessType, "501\\(c\\)3") ~ "501(c)3 Non Profit",
    str_detect(BusinessType, "501\\(c\\)6") ~ "501(c)6 Non Profit Membership",
    str_detect(BusinessType, "501\\(c\\) .*except") ~ "501(c) Non Profit except 3,4,6,19",
    str_detect(BusinessType, "501\\(c\\)19") ~ "501(c)19 Non Profit Veterans",
    TRUE ~ BusinessType
  ))

saveRDS(ppp_clean, "ppp_clean.rds")
file.exists("ppp_clean.rds")
#next time you open R you just need:
ppp_clean <- readRDS("C:/Users/shrey/OneDrive/Desktop/Data_Gender Gaps/Excel files/ppp_clean.rds")

# ============================================================
# STEP 5: SANITY CHECKS
# ============================================================

# Gender breakdown
ppp_clean %>% count(Gender_clean)

# Second draw breakdown
ppp_clean %>% count(second_draw)

# Cross check second draw with year
ppp_clean %>%
  group_by(loan_year, second_draw) %>%
  summarise(n = n(), .groups = "drop")

# Lender type breakdown
ppp_clean %>%
  count(lender_type) %>%
  mutate(share = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

# NAICS 2-digit breakdown
ppp_clean %>%
  count(naics2) %>%
  arrange(naics2) %>%
  print(n = 30)

# Time periods
ppp_clean %>% count(time_fe) %>% arrange(time_fe)

# Loan amount distribution
summary(ppp_clean$ln_loan)


# ============================================================
# STEP 6: REGRESSIONS
# ============================================================

# --- Model 1: Gender only + state & time FE ---
m1 <- feols(ln_loan ~ Gender_clean | BorrowerState + time_fe,
            data    = ppp_clean,
            cluster = ~BorrowerState)

# --- Model 2: Add jobs and second draw ---
m2 <- feols(ln_loan ~ Gender_clean + log1p(jobs) + second_draw |
              BorrowerState + time_fe,
            data    = ppp_clean,
            cluster = ~BorrowerState)

# --- Model 3: Add business type and 2-digit NAICS as FE ---
m3 <- feols(ln_loan ~ Gender_clean + log1p(jobs) + second_draw |
              BorrowerState + time_fe + naics2 + BusinessType,
            data    = ppp_clean,
            cluster = ~BorrowerState)

# --- Model 4: Full model — add lender type FE ---
m4 <- feols(ln_loan ~ Gender_clean + log1p(jobs) + second_draw |
              BorrowerState + time_fe + naics2 + BusinessType + lender_type,
            data    = ppp_clean,
            cluster = ~BorrowerState)

# --- Loan Amount Regression with lender_type as explicit control ---

# Model 5 with lender_type as explicit control
m4_explicit <- feols(ln_loan ~ Gender_clean + log1p(jobs) + 
                       second_draw + lender_type |
                       BorrowerState + time_fe + naics2 + BusinessType,
                     data    = ppp_clean,
                     cluster = ~BorrowerState)



# STEP 7: RESULTS TABLE

etable(m1,m2,m3,m4,m4_explicit,
       digits   = 4,
       se.below = TRUE,
       fitstat  = c("n", "r2", "ar2"),
       title    = "Gender Gap in PPP Loan Amounts (Male = Baseline)")


etable(m1, m2, m3, m4,m4_explicit,
       keep     = "Gender_clean",
       digits   = 4,
       se.below = TRUE,
       fitstat  = c("n", "r2", "ar2"),
       title    = "Gender Gap in PPP Loan Amounts (Male = Baseline)",
       file     = "ppp_gender_results.html")


#regression 6.2 (y = early approval)
rm(list = setdiff(ls(), c("ppp_clean", "ppp_early")))

# Check earliest 2021 date in ppp_clean
cat("Earliest 2021 approval date in ppp_clean:",
    format(min(ppp_clean$DateApproved[year(ppp_clean$DateApproved) == 2021],
               na.rm = TRUE)), "\n")

# Check date distribution in early windows
ppp_clean %>%
  filter((DateApproved >= as.Date("2020-04-03") & 
            DateApproved <= as.Date("2020-04-16")) |
           (DateApproved >= as.Date("2021-01-13") & 
              DateApproved <= as.Date("2021-01-26"))) %>%
  count(time_fe) %>%
  arrange(time_fe)

# ============================================================
# STEP: CREATE EARLY APPROVAL INDICATOR
# ============================================================

# --- Define early access windows ---
early_start_2020 <- as.Date("2020-04-03")
early_end_2020   <- as.Date("2020-04-16")
early_start_2021 <- as.Date("2021-01-13")
early_end_2021   <- as.Date("2021-01-26")

ppp_clean <- ppp_clean %>%
  mutate(
    
    # --- Early approval flag per year ---
    early_2020 = case_when(
      loan_year == 2020 & DateApproved >= early_start_2020 &
        DateApproved <= early_end_2020 ~ 1L,
      loan_year == 2020 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    early_2021 = case_when(
      loan_year == 2021 & DateApproved >= early_start_2021 &
        DateApproved <= early_end_2021 ~ 1L,
      loan_year == 2021 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # --- Combined early approval indicator ---
    # 1 if approved in early window of either year
    # 0 if approved outside early window in either year
    # NA if neither year applies
    early_approval = case_when(
      loan_year == 2020 ~ early_2020,
      loan_year == 2021 ~ early_2021,
      TRUE              ~ NA_integer_
    )
  )

# --- Verify ---
ppp_clean %>%
  group_by(loan_year, early_approval) %>%
  summarise(n = n(), .groups = "drop")


# ============================================================
# EARLY APPROVAL REGRESSIONS
# ============================================================

# --- Subset to loans where early_approval is defined ---
ppp_early <- ppp_clean %>%
  filter(!is.na(early_approval))

#saving ppp_early
saveRDS(ppp_early, "ppp_early.rds")
file.exists("ppp_early.rds")
#next time you open R you just need:
ppp_early <- readRDS("C:/Users/shrey/OneDrive/Desktop/Data_Gender Gaps/Excel files/ppp_early.rds")


# ------------------------------------------------------------
# APPROACH 1: LINEAR PROBABILITY MODEL (LPM)
# Coefficients = percentage point differences in probability
# ------------------------------------------------------------

# --- LPM Model 1: Gender only + state & time FE ---
lpm1 <- feols(early_approval ~ Gender_clean | BorrowerState + time_fe,
              data    = ppp_early,
              cluster = ~BorrowerState)

# --- LPM Model 2: Add jobs and second draw ---
lpm2 <- feols(early_approval ~ Gender_clean + log1p(jobs) + second_draw |
                BorrowerState + time_fe,
              data    = ppp_early,
              cluster = ~BorrowerState)

# --- LPM Model 3: Add business type and NAICS FE ---
lpm3 <- feols(early_approval ~ Gender_clean + log1p(jobs) + second_draw |
                BorrowerState + time_fe + naics2 + BusinessType,
              data    = ppp_early,
              cluster = ~BorrowerState)

# --- LPM Model 4: Full model ---
lpm4 <- feols(early_approval ~ Gender_clean + log1p(jobs) + second_draw |
                BorrowerState + time_fe + naics2 + BusinessType + lender_type,
              data    = ppp_early,
              cluster = ~BorrowerState)

# --- LPM Results Table ---
etable( lpm4,
       digits   = 4,
       se.below = TRUE,
       fitstat  = c("n", "r2", "ar2"),
       title    = "Early Approval Gap (LPM) — Male = Baseline")


#regression 6.3
#question: LPM 
## ============================================================
# STEP: CREATE OBSERVED GENDER INDICATOR
# ============================================================

ppp_clean <- ppp_clean %>%
  mutate(observed_gender = case_when(
    Gender == "Female Owned" ~ 1L,
    Gender == "Male Owned"   ~ 1L,
    Gender == "Unanswered"   ~ 0L
  ))

# --- Verify ---
ppp_clean %>% count(observed_gender)


# ============================================================
# GENDER OBSERVABILITY REGRESSIONS
# W_ist includes all controls as explicit variables
# ============================================================

# ------------------------------------------------------------
# APPROACH 1: LINEAR PROBABILITY MODEL (LPM)
# ------------------------------------------------------------

# --- LPM Model 1: Loan amount only + state & time FE ---
obs1 <- feols(observed_gender ~ ln_loan |
                BorrowerState + time_fe,
              data    = ppp_clean,
              cluster = ~BorrowerState)

# --- LPM Model 2: Add jobs and second draw ---
obs2 <- feols(observed_gender ~ ln_loan + log1p(jobs) + second_draw |
                BorrowerState + time_fe,
              data    = ppp_clean,
              cluster = ~BorrowerState)

# --- LPM Model 3: Add business type and industry ---
obs3 <- feols(observed_gender ~ ln_loan + log1p(jobs) + second_draw +
                BusinessType |
                BorrowerState + time_fe + naics2,
              data    = ppp_clean,
              cluster = ~BorrowerState)
gc()

# obs4 — richest model- reported
obs4 <- feols(observed_gender ~ ln_loan + log1p(jobs) + second_draw |
                BorrowerState + time_fe + naics2 + lender_type + BusinessType,
              data    = ppp_clean,
              cluster = ~BorrowerState)
gc()

#LpM MODEL 5 (TReating naics codes as explicit control 
obs5 <- feols(observed_gender ~ ln_loan + log1p(jobs) + second_draw +
                     + lender_type   |
                    BorrowerState + time_fe + BusinessType + naics2,
                  data    = ppp_clean,
                  cluster = ~BorrowerState)

Sys.setlocale("LC_ALL", "en_US.UTF-8")

# --- LPM Results Table ---
etable( obs4, 
       digits   = 4,
       se.below = TRUE,
       fitstat  = c("n", "r2", "ar2"),
       title    = "Gender Observability Regression (LPM)")

etable(obs1)
etable(obs2)
etable(obs3)
etable(obs4)
etable(obs5)

# Check BusinessType for non-ASCII characters
ppp_clean %>%
  distinct(BusinessType) %>%
  print(n = 50)

# Check specifically for invalid characters
ppp_clean %>%
  distinct(BusinessType) %>%
  filter(!validUTF8(BusinessType))

# Check naics2 for issues
ppp_clean %>%
  distinct(naics2) %>%
  arrange(naics2)
etable(obs4)
etable(obs5)
# --- LPM Results showing lender type coefficients ---
etable(obs4,
       digits   = 4,
       se.below = TRUE,
       fitstat  = c("n", "r2", "ar2"),
       title    = "Gender Observability — Full Model Coefficients")

#Robustness check 

#ROBUSTNESS CHECK: Observed gender only
# Drop all Unknown/Unanswered observations
# Re-run Regression 1 (loan amount) and Regression 2 (early approval)
# Sample restricted to Male-owned and Female-owned firms only
# ==============================================================================

# --- Create observed-gender-only subset ---
ppp_observed <- ppp_clean %>%
  filter(Gender_clean != "Unknown") %>%
  mutate(Gender_clean = droplevels(Gender_clean))  # drop unused Unknown level

ppp_early_observed <- ppp_early %>%
  filter(Gender_clean != "Unknown") %>%
  mutate(Gender_clean = droplevels(Gender_clean))

# Check sample size
cat("Full sample:", nrow(ppp_clean), "\n")
cat("Observed gender only:", nrow(ppp_observed), "\n")
cat("Dropped:", nrow(ppp_clean) - nrow(ppp_observed), "Unknown observations\n")
cat("\nGender breakdown in restricted sample:\n")
print(ppp_observed %>% count(Gender_clean))


# ==============================================================================
# REGRESSION 1 (ROBUSTNESS): LOAN AMOUNT — Observed gender only
# ==============================================================================

cat("\nRunning loan amount regressions on observed gender only...\n")

# Model 1: Gender + state & time FE
m1_obs <- feols(ln_loan ~ Gender_clean | BorrowerState + time_fe,
                data    = ppp_observed,
                cluster = ~BorrowerState)

# Model 2: Add jobs and second draw
m2_obs <- feols(ln_loan ~ Gender_clean + log1p(jobs) + second_draw |
                  BorrowerState + time_fe,
                data    = ppp_observed,
                cluster = ~BorrowerState)

# Model 3: Add business type and NAICS FE
m3_obs <- feols(ln_loan ~ Gender_clean + log1p(jobs) + second_draw |
                  BorrowerState + time_fe + naics2 + BusinessType,
                data    = ppp_observed,
                cluster = ~BorrowerState)

# Model 4: Full model with lender type FE
m4_obs <- feols(ln_loan ~ Gender_clean + log1p(jobs) + second_draw |
                  BorrowerState + time_fe + naics2 + BusinessType + lender_type,
                data    = ppp_observed,
                cluster = ~BorrowerState)

# Model 5: Lender type as explicit control
m5_obs <- feols(ln_loan ~ Gender_clean + log1p(jobs) + second_draw + lender_type |
                  BorrowerState + time_fe + naics2 + BusinessType,
                data    = ppp_observed,
                cluster = ~BorrowerState)

cat("\nLoan Amount — Observed Gender Only:\n")
etable(m1_obs, m2_obs, m3_obs, m4_obs, m5_obs,
       keep     = "Gender_clean",
       digits   = 4,
       se.below = TRUE,
       fitstat  = c("n", "r2", "ar2"),
       title    = "Robustness: Loan Amount — Observed Gender Only (Male = Baseline)")

 # ROBUSTNESS CHECK: Observed gender only
# Drop all Unknown/Unanswered observations
# Re-run Regression 1 (loan amount) and Regression 2 (early approval)
# Sample restricted to Male-owned and Female-owned firms only
# ==============================================================================

# --- Create observed-gender-only subset ---
ppp_observed <- ppp_clean %>%
  filter(Gender_clean != "Unknown") %>%
  mutate(Gender_clean = droplevels(Gender_clean))  # drop unused Unknown level

ppp_early_observed <- ppp_early %>%
  filter(Gender_clean != "Unknown") %>%
  mutate(Gender_clean = droplevels(Gender_clean))

# Check sample size
cat("Full sample:", nrow(ppp_clean), "\n")
cat("Observed gender only:", nrow(ppp_observed), "\n")
cat("Dropped:", nrow(ppp_clean) - nrow(ppp_observed), "Unknown observations\n")
cat("\nGender breakdown in restricted sample:\n")
print(ppp_observed %>% count(Gender_clean))


# ==============================================================================
# REGRESSION 1 (ROBUSTNESS): LOAN AMOUNT — Observed gender only
# ==============================================================================

cat("\nRunning loan amount regressions on observed gender only...\n")

# Model 1: Gender + state & time FE
m1_obs <- feols(ln_loan ~ Gender_clean | BorrowerState + time_fe,
                data    = ppp_observed,
                cluster = ~BorrowerState)

# Model 2: Add jobs and second draw
m2_obs <- feols(ln_loan ~ Gender_clean + log1p(jobs) + second_draw |
                  BorrowerState + time_fe,
                data    = ppp_observed,
                cluster = ~BorrowerState)

# Model 3: Add business type and NAICS FE
m3_obs <- feols(ln_loan ~ Gender_clean + log1p(jobs) + second_draw |
                  BorrowerState + time_fe + naics2 + BusinessType,
                data    = ppp_observed,
                cluster = ~BorrowerState)

# Model 4: Full model with lender type FE
m4_obs <- feols(ln_loan ~ Gender_clean + log1p(jobs) + second_draw |
                  BorrowerState + time_fe + naics2 + BusinessType + lender_type,
                data    = ppp_observed,
                cluster = ~BorrowerState)

# Model 5: Lender type as explicit control
m5_obs <- feols(ln_loan ~ Gender_clean + log1p(jobs) + second_draw + lender_type |
                  BorrowerState + time_fe + naics2 + BusinessType,
                data    = ppp_observed,
                cluster = ~BorrowerState)

cat("\nLoan Amount — Observed Gender Only:\n")
etable(m1_obs, m2_obs, m3_obs, m4_obs, m5_obs,
       keep     = "Gender_clean",
       digits   = 4,
       se.below = TRUE,
       fitstat  = c("n", "r2", "ar2"),
       title    = "Robustness: Loan Amount — Observed Gender Only (Male = Baseline)")


# ==============================================================================
# REGRESSION 2 (ROBUSTNESS): EARLY APPROVAL — Observed gender only
# ==============================================================================

cat("\nRunning early approval regressions on observed gender only...\n")

# LPM Model 1
lpm1_obs <- feols(early_approval ~ Gender_clean | BorrowerState + time_fe,
                  data    = ppp_early_observed,
                  cluster = ~BorrowerState)

# LPM Model 2
lpm2_obs <- feols(early_approval ~ Gender_clean + log1p(jobs) + second_draw |
                    BorrowerState + time_fe,
                  data    = ppp_early_observed,
                  cluster = ~BorrowerState)

# LPM Model 3
lpm3_obs <- feols(early_approval ~ Gender_clean + log1p(jobs) + second_draw |
                    BorrowerState + time_fe + naics2 + BusinessType,
                  data    = ppp_early_observed,
                  cluster = ~BorrowerState)

# LPM Model 4
lpm4_obs <- feols(early_approval ~ Gender_clean + log1p(jobs) + second_draw |
                    BorrowerState + time_fe + naics2 + BusinessType + lender_type,
                  data    = ppp_early_observed,
                  cluster = ~BorrowerState)

cat("\nEarly Approval — Observed Gender Only:\n")
etable(lpm1_obs, lpm2_obs, lpm3_obs, lpm4_obs,
       keep     = "Gender_clean",
       digits   = 4,
       se.below = TRUE,
       fitstat  = c("n", "r2", "ar2"),
       title    = "Robustness: Early Approval — Observed Gender Only (Male = Baseline)")

# ==============================================================================
# ROBUSTNESS CHECK: Extended early approval window (30 days)
# Original: first 14 days of each round
# Robustness: first 30 days of each round
# ==============================================================================

# --- Define 30-day windows ---
early_start_2020 <- as.Date("2020-04-03")
early_end_2020_30   <- as.Date("2020-05-02")   # 30 days from April 3

early_start_2021 <- as.Date("2021-01-13")
early_end_2021_30   <- as.Date("2021-02-11")   # 30 days from January 13

# --- Create 30-day early approval indicator ---
ppp_clean1 <- ppp_clean %>%
  mutate(
    early_2020_30 = case_when(
      loan_year == 2020 & DateApproved >= early_start_2020 &
        DateApproved <= early_end_2020_30 ~ 1L,
      loan_year == 2020 ~ 0L,
      TRUE ~ NA_integer_
    ),
    early_2021_30 = case_when(
      loan_year == 2021 & DateApproved >= early_start_2021 &
        DateApproved <= early_end_2021_30 ~ 1L,
      loan_year == 2021 ~ 0L,
      TRUE ~ NA_integer_
    ),
    early_approval_30 = case_when(
      loan_year == 2020 ~ early_2020_30,
      loan_year == 2021 ~ early_2021_30,
      TRUE              ~ NA_integer_
    )
  )

# --- Create 30-day ppp_early subset ---
ppp_early_30 <- ppp_clean1 %>%
  filter(!is.na(early_approval_30))

# --- Check how many loans fall in 30-day vs 14-day window ---
cat("14-day early approval share:\n")
print(ppp_early %>% count(early_approval) %>%
        mutate(share = round(n / sum(n) * 100, 1)))

cat("\n30-day early approval share:\n")
print(ppp_early_30 %>% count(early_approval_30) %>%
        mutate(share = round(n / sum(n) * 100, 1)))


# ==============================================================================
# EARLY APPROVAL REGRESSIONS — 30-day window
# ==============================================================================

cat("\nRunning early approval regressions with 30-day window...\n")

# LPM Model 1
lpm1_30 <- feols(early_approval_30 ~ Gender_clean | BorrowerState + time_fe,
                 data    = ppp_early_30,
                 cluster = ~BorrowerState)

# LPM Model 2
lpm2_30 <- feols(early_approval_30 ~ Gender_clean + log1p(jobs) + second_draw |
                   BorrowerState + time_fe,
                 data    = ppp_early_30,
                 cluster = ~BorrowerState)

# LPM Model 3
lpm3_30 <- feols(early_approval_30 ~ Gender_clean + log1p(jobs) + second_draw |
                   BorrowerState + time_fe + naics2 + BusinessType,
                 data    = ppp_early_30,
                 cluster = ~BorrowerState)

# LPM Model 4: Full model
lpm4_30 <- feols(early_approval_30 ~ Gender_clean + log1p(jobs) + second_draw |
                   BorrowerState + time_fe + naics2 + BusinessType + lender_type,
                 data    = ppp_early_30,
                 cluster = ~BorrowerState)

cat("\nEarly Approval (30-day window) Results:\n")
etable(lpm4_30,
       keep     = "Gender_clean",
       digits   = 4,
       se.below = TRUE,
       fitstat  = c("n", "r2", "ar2"),
       title    = "Robustness: Early Approval — 30-day Window (Male = Baseline)")

# ==============================================================================
# HETEROGENEITY BY YEAR: Split sample + Interaction term
# Tests whether the gender gap changed between PPP Round 1 (2020)
# and PPP Round 2 (2021)
# ==============================================================================

# --- Create year indicator and interaction terms ---
ppp_clean2 <- ppp_clean %>%
  mutate(
    year2021       = if_else(loan_year == 2021, 1L, 0L),
    female         = if_else(Gender_clean == "Female", 1L, 0L),
    unknown        = if_else(Gender_clean == "Unknown", 1L, 0L),
    female_2021    = female  * year2021,   # interaction: Female × Year2021
    unknown_2021   = unknown * year2021    # interaction: Unknown × Year2021
  )
ppp_early2 <- ppp_early %>%
  mutate(
    year2021       = if_else(loan_year == 2021, 1L, 0L),
    female         = if_else(Gender_clean == "Female", 1L, 0L),
    unknown        = if_else(Gender_clean == "Unknown", 1L, 0L),
    female_2021    = female  * year2021,   # interaction: Female × Year2021
    unknown_2021   = unknown * year2021    # interaction: Unknown × Year2021
  )






ppp_2020 <- ppp_clean %>% filter(loan_year == 2020)
ppp_2021 <- ppp_clean %>% filter(loan_year == 2021)

cat("2020 sample size:", nrow(ppp_2020), "\n")
cat("2021 sample size:", nrow(ppp_2021), "\n")

# --- 2020: Loan Amount ---
m_2020_1 <- feols(ln_loan ~ Gender_clean | BorrowerState + time_fe,
                  data    = ppp_2020,
                  cluster = ~BorrowerState)

m_2020_4 <- feols(ln_loan ~ Gender_clean + log1p(jobs) + second_draw |
                    BorrowerState + time_fe + naics2 + BusinessType + lender_type,
                  data    = ppp_2020,
                  cluster = ~BorrowerState)

# --- 2021: Loan Amount ---
m_2021_1 <- feols(ln_loan ~ Gender_clean | BorrowerState + time_fe,
                  data    = ppp_2021,
                  cluster = ~BorrowerState)

m_2021_4 <- feols(ln_loan ~ Gender_clean + log1p(jobs) + second_draw |
                    BorrowerState + time_fe + naics2 + BusinessType + lender_type,
                  data    = ppp_2021,
                  cluster = ~BorrowerState)

cat("\nLoan Amount — Split Sample Results:\n")
etable(m_2020_1, m_2021_1, m_2020_4, m_2021_4,
       keep     = "Gender_clean",
       digits   = 4,
       se.below = TRUE,
       fitstat  = c("n", "r2"),
       title    = "Loan Amount by Year — Split Sample (Male = Baseline)")

# Model using factor indicators with interaction
# Baseline: Male-owned firm in 2020
# female coefficient = female gap in 2020
# year2021 coefficient = year effect for male-owned firms
# female_2021 coefficient = CHANGE in female gap from 2020 to 2021

# Baseline model with interaction
m_int_1 <- feols(ln_loan ~ female + unknown + year2021 +
                   female_2021 + unknown_2021 |
                   BorrowerState,
                 data    = ppp_clean2,
                 cluster = ~BorrowerState)

# Full model with interaction
m_int_4 <- feols(ln_loan ~ female + unknown + year2021 +
                   female_2021 + unknown_2021 +
                   log1p(jobs) + second_draw |
                   BorrowerState + naics2 + BusinessType + lender_type,
                 data    = ppp_clean2,
                 cluster = ~BorrowerState)

etable(m_int_1, m_int_4,
       keep     = c("female", "unknown", "year2021",
                    "female_2021", "unknown_2021"),
       digits   = 4,
       se.below = TRUE,
       fitstat  = c("n", "r2"),
       title    = "Loan Amount — Female × Year2021 Interaction (Male 2020 = Baseline)")

# ==============================================================================
# PART 3: SPLIT SAMPLE — Early Approval
# ==============================================================================

cat("\n\nPART 2A: Early Approval — Split by Year\n")

ppp_early_2020 <- ppp_early %>% filter(loan_year == 2020)
ppp_early_2021 <- ppp_early %>% filter(loan_year == 2021)

cat("2020 early approval sample:", nrow(ppp_early_2020), "\n")
cat("2021 early approval sample:", nrow(ppp_early_2021), "\n")

# --- 2020: Early Approval ---
lpm_2020_1 <- feols(early_approval ~ Gender_clean | BorrowerState + time_fe,
                    data    = ppp_early_2020,
                    cluster = ~BorrowerState)

lpm_2020_4 <- feols(early_approval ~ Gender_clean + log1p(jobs) + second_draw |
                      BorrowerState + time_fe + naics2 + BusinessType + lender_type,
                    data    = ppp_early_2020,
                    cluster = ~BorrowerState)

# --- 2021: Early Approval ---
lpm_2021_1 <- feols(early_approval ~ Gender_clean | BorrowerState + time_fe,
                    data    = ppp_early_2021,
                    cluster = ~BorrowerState)

lpm_2021_4 <- feols(early_approval ~ Gender_clean + log1p(jobs) + second_draw |
                      BorrowerState + time_fe + naics2 + BusinessType + lender_type,
                    data    = ppp_early_2021,
                    cluster = ~BorrowerState)

cat("\nEarly Approval — Split Sample Results:\n")
etable(lpm_2020_1, lpm_2021_1, lpm_2020_4, lpm_2021_4,
       keep     = "Gender_clean",
       digits   = 4,
       se.below = TRUE,
       fitstat  = c("n", "r2"),
       title    = "Early Approval by Year — Split Sample (Male = Baseline)")

#PART 4: INTERACTION TERM — Early Approval

cat("\n\nPART 2B: Early Approval — Interaction Term\n")

lpm_int_1 <- feols(early_approval ~ female + unknown + year2021 +
                     female_2021 + unknown_2021 |
                     BorrowerState,
                   data    = ppp_early2,
                   cluster = ~BorrowerState)

lpm_int_4 <- feols(early_approval ~ female + unknown + year2021 +
                     female_2021 + unknown_2021 +
                     log1p(jobs) + second_draw |
                     BorrowerState  + naics2 + BusinessType + lender_type,
                   data    = ppp_early2,
                   cluster = ~BorrowerState)

cat("\nEarly Approval — Interaction Results:\n")
etable(lpm_int_1, lpm_int_4,
       keep     = c("female", "unknown", "year2021",
                    "female_2021", "unknown_2021"),
       digits   = 4,
       se.below = TRUE,
       fitstat  = c("n", "r2"),
       title    = "Early Approval — Female × Year2021 Interaction (Male 2020 = Baseline)")