# ============================================================
# Load libraries and Working Directory
# ============================================================
install.packages(c("tidyverse","readxl","fixest"))
library(tidyverse)
library(readxl)
library(fixest)
library(tidyverse)
library(readxl)

getwd()
setwd("C:\\Users\\shrey\\OneDrive\\Desktop\\Data_Gender Gaps\\Excel files")


# ============================================================
# Read both sheets from the Excel file
# ============================================================

indices_2020 <- read_excel("four_indices.xlsx", sheet = "2020") %>%
  mutate(year = 2020)

indices_2021 <- read_excel("four_indices.xlsx", sheet = "2021") %>%
  mutate(year = 2021)

# ============================================================
# Stack into one panel (102 rows: 51 states × 2 years)
# ============================================================

indices_panel <- bind_rows(indices_2020, indices_2021) %>%
  rename(
    state                = StateCode,
    representation_gap   = representation_gap,
    access_gap           = access_gap,
    loan_size_gap        = Loan_size_gap,
    early_access_gap     = ggi_early_access_gap
  ) %>%
  select(state, year, representation_gap, access_gap, 
         loan_size_gap, early_access_gap)

# ============================================================
# Quick checks
# ============================================================

# Should show 102 rows, 6 columns
glimpse(indices_panel)

# Should be 51 states × 2 years with no missing values
indices_panel %>%
  summarise(
    n_rows        = n(),
    n_states      = n_distinct(state),
    n_years       = n_distinct(year),
    missing_rep   = sum(is.na(representation_gap)),
    missing_acc   = sum(is.na(access_gap)),
    missing_loan  = sum(is.na(loan_size_gap)),
    missing_early = sum(is.na(early_access_gap))
  )

# Preview
print(indices_panel, n = 10)

# STEP 5: Read unknown share from your Excel file


ug_2020 <- read_excel("Unknown_Share_Diagonostic.xlsx", sheet = "2020")
ug_2021 <- read_excel("Unknown_Share_Diagonostic.xlsx", sheet = "2021")

ug_panel <- bind_rows(ug_2020, ug_2021) %>%
  select(state = BorrowerState, year, ug_share = unknown_share)

# Check — should be 102 rows
glimpse(ug_panel)


# ============================================================
#  Get ppp_clean and ppp_early excel files
# ============================================================

ppp_clean <- readRDS("C:/Users/shrey/OneDrive/Desktop/Data_Gender Gaps/Excel files/ppp_clean.rds")
ppp_early <- readRDS("C:/Users/shrey/OneDrive/Desktop/Data_Gender Gaps/Excel files/ppp_early.rds")


# Unemployment data (state control)
unemployment_panel <- tribble(
  ~state, ~year, ~unemp_rate,
  "AL", 2020, 5.9,   "AL", 2021, 3.4,
  "AK", 2020, 7.8,   "AK", 2021, 6.4,
  "AZ", 2020, 7.9,   "AZ", 2021, 4.9,
  "AR", 2020, 6.1,   "AR", 2021, 4.0,
  "CA", 2020, 10.1,  "CA", 2021, 7.3,
  "CO", 2020, 7.3,   "CO", 2021, 5.4,
  "CT", 2020, 7.9,   "CT", 2021, 6.3,
  "DE", 2020, 7.8,   "DE", 2021, 5.3,
  "DC", 2020, 8.0,   "DC", 2021, 6.6,
  "FL", 2020, 7.7,   "FL", 2021, 4.6,
  "GA", 2020, 6.5,   "GA", 2021, 3.9,
  "HI", 2020, 11.6,  "HI", 2021, 5.7,
  "ID", 2020, 5.4,   "ID", 2021, 3.6,
  "IL", 2020, 9.5,   "IL", 2021, 6.1,
  "IN", 2020, 7.1,   "IN", 2021, 3.6,
  "IA", 2020, 5.3,   "IA", 2021, 4.2,
  "KS", 2020, 5.9,   "KS", 2021, 3.2,
  "KY", 2020, 6.6,   "KY", 2021, 4.7,
  "LA", 2020, 8.3,   "LA", 2021, 5.5,
  "ME", 2020, 5.4,   "ME", 2021, 4.6,
  "MD", 2020, 6.8,   "MD", 2021, 5.8,
  "MA", 2020, 8.9,   "MA", 2021, 5.7,
  "MI", 2020, 9.9,   "MI", 2021, 5.9,
  "MN", 2020, 6.2,   "MN", 2021, 3.4,
  "MS", 2020, 8.1,   "MS", 2021, 5.6,
  "MO", 2020, 6.1,   "MO", 2021, 4.4,
  "MT", 2020, 5.9,   "MT", 2021, 3.4,
  "NE", 2020, 4.2,   "NE", 2021, 2.5,
  "NV", 2020, 12.8,  "NV", 2021, 7.2,
  "NH", 2020, 6.7,   "NH", 2021, 3.5,
  "NJ", 2020, 9.8,   "NJ", 2021, 6.3,
  "NM", 2020, 8.4,   "NM", 2021, 6.8,
  "NY", 2020, 10.0,  "NY", 2021, 6.9,
  "NC", 2020, 7.3,   "NC", 2021, 4.8,
  "ND", 2020, 5.1,   "ND", 2021, 3.7,
  "OH", 2020, 8.1,   "OH", 2021, 5.1,
  "OK", 2020, 6.1,   "OK", 2021, 3.8,
  "OR", 2020, 7.6,   "OR", 2021, 5.2,
  "PA", 2020, 9.1,   "PA", 2021, 6.3,
  "RI", 2020, 9.4,   "RI", 2021, 5.6,
  "SC", 2020, 6.2,   "SC", 2021, 4.0,
  "SD", 2020, 4.6,   "SD", 2021, 3.1,
  "TN", 2020, 7.5,   "TN", 2021, 4.3,
  "TX", 2020, 7.6,   "TX", 2021, 5.7,
  "UT", 2020, 4.7,   "UT", 2021, 2.7,
  "VT", 2020, 5.6,   "VT", 2021, 3.4,
  "VA", 2020, 6.2,   "VA", 2021, 3.9,
  "WA", 2020, 8.4,   "WA", 2021, 5.2,
  "WV", 2020, 8.3,   "WV", 2021, 5.0,
  "WI", 2020, 6.3,   "WI", 2021, 3.8,
  "WY", 2020, 5.8,   "WY", 2021, 4.5
)

# Verify — should be 102 rows, 51 states
glimpse(unemployment_panel)
  
ug_panel <- ug_panel %>%
  mutate(year = as.integer(year))

indices_panel <- indices_panel %>%
  mutate(year = as.integer(year))

unemployment_panel <- unemployment_panel %>%
  mutate(year = as.integer(year))

# Verify all are now integer
cat("ug_panel year type:         ", class(ug_panel$year), "\n")
cat("indices_panel year type:    ", class(indices_panel$year), "\n")
cat("unemployment_panel year type:", class(unemployment_panel$year), "\n")

regression_data <- ug_panel %>%
  left_join(indices_panel,    by = c("state", "year")) %>%
  left_join(unemployment_panel, by = c("state", "year"))


# Quick check
cat("Rows:", nrow(regression_data), "\n")        # expect 102
cat("Columns:", ncol(regression_data), "\n")     # expect 10
cat("Missing values:\n")
colSums(is.na(regression_data))

print(regression_data, n = 10)
names(regression_data)


# ============================================================
#  Run regression
# Model: Gap_st = δ0 + δ1*UG_st + δ2*unemp_rate_st + μ_s + λ_t + e_st
# ============================================================


m1 <- feols(representation_gap ~ ug_share + unemp_rate | state + year,
            data = regression_data, vcov = ~state)

m2 <- feols(access_gap ~ ug_share + unemp_rate | state + year,
            data = regression_data, vcov = ~state)

m3 <- feols(loan_size_gap ~ ug_share + unemp_rate | state + year,
            data = regression_data, vcov = ~state)

m4 <- feols(early_access_gap ~ ug_share + unemp_rate | state + year,
            data = regression_data, vcov = ~state)

# View results
etable(m1, m2, m3, m4,
       headers = c("Representation", "Access", "Loan Size", "Early Access"))

# ============================================================
#  Individual summaries if needed
# ============================================================

summary(m1)
summary(m2)
summary(m3)
summary(m4)