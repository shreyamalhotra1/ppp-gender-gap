#Step 1 (loading dataset for calculating ggi)
rm(list = ls())


install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

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


#creating year variable
df_stacked$year <- format(as.Date(df_stacked$DateApproved, format="%m/%d/%Y"),"%Y")


#Step 2 filter for male and female
gender_data <- df_stacked %>%
  filter(Gender %in% c("Male Owned", "Female Owned")) %>%
  mutate(
    year = as.factor(year),
    Gender = as.factor(Gender)
  )

#get unique territories

unique_territories <- unique(gender_data$BorrowerState)  
print(unique_territories)

us_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
               "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
               "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
               "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
               "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC")

gender_data_states <- gender_data[gender_data$BorrowerState %in% us_states, ]

#step 2 (preparing for hidden timing access gap)
# hidden timing access gap

library(lubridate)


gender_data_states <- gender_data_states %>%
  mutate(DateApproved = as.Date(DateApproved, format = "%m/%d/%Y"))

# Check if conversion worked
cat("\nAfter conversion - class:", class(gender_data_states$DateApproved), "\n")
cat("First few dates after conversion:", head(gender_data_states$DateApproved), "\n")

# Check for any conversion failures (NAs)
na_count <- sum(is.na(gender_data_states$DateApproved))
if(na_count > 0) {
  cat("\nWARNING:", na_count, "dates failed to convert. Check your date format.\n")
}
earliest_2021 <- min(gender_data_states$DateApproved[year(gender_data_states$DateApproved) == 2021], 
                     na.rm = TRUE)
cat("\nEarliest 2021 approval date in data:", earliest_2021, "\n")
# ----------------------------------------------------------------------------
# STEP 1: Define the "Early" date ranges for 2020 and 2021
# ----------------------------------------------------------------------------

# PPP 2020 launched on April 3, 2020 
# First 14 days: April 3 - April 16, 2020
# 
# PPP 2021: For consistency, using the first 14 days of the program's 
# active period in 2021. The SBA announced the 2021 round opening in January.
# Based on SBA data, the 2021 program was actively accepting applications
# from early January. We use January 11 - January 24, 2021 as the consistent
# "first 14 days" window. 
# ----------------------------------------------------------------------------

early_start_2020 <- as.Date("2020-04-03")
early_end_2020 <- as.Date("2020-04-16")

# Recommended: Check your data's earliest 2021 date
cat("Earliest 2021 approval date in data:", 
    min(gender_data_states$DateApproved[year(gender_data_states$DateApproved) == 2021], 
        na.rm = TRUE), "\n")

early_start_2021 <- as.Date("2021-01-13")  
early_end_2021 <- as.Date("2021-01-26")

# ----------------------------------------------------------------------------
# Create a flag for early approval status (like creating dummy)
# ----------------------------------------------------------------------------
gender_data_states <- gender_data_states %>%
  mutate(
  
    Year = year(DateApproved),
    
    # Flag for early approval in 2020
    early_2020 = case_when(
      Year == 2020 & DateApproved >= early_start_2020 & DateApproved <= early_end_2020 ~ 1,
      Year == 2020 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Flag for early approval in 2021
    early_2021 = case_when(
      Year == 2021 & DateApproved >= early_start_2021 & DateApproved <= early_end_2021 ~ 1,
      Year == 2021 ~ 0,
      TRUE ~ NA_real_
    )
  )
#Early Access Gap
# ----------------------------------------------------------------------------
# STEP 3: Calculate GGI Timing Gap by State for 2020
# ----------------------------------------------------------------------------
Tggi_2020 <- gender_data_states %>%
  filter(Year == 2020, !is.na(Gender), Gender %in% c("Male Owned", "Female Owned")) %>%
  group_by(BorrowerState, Gender) %>%
  summarise(
    total_loans = n(),
    early_loans = sum(early_2020, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate early rate per gender within each state
  group_by(BorrowerState) %>%
  summarise(
    female_rate = early_loans[Gender == "Female Owned"] / total_loans[Gender == "Female Owned"],
    male_rate = early_loans[Gender == "Male Owned"] / total_loans[Gender == "Male Owned"],
    female_total = total_loans[Gender == "Female Owned"],
    male_total = total_loans[Gender == "Male Owned"],
    .groups = "drop"
  ) %>%
  mutate(
    ggi_early_access_gap = female_rate - male_rate,
    year = 2020,
    # Add interpretation based on the GGI gap value
    interpretation = case_when(
      ggi_early_access_gap == 0 ~ "Women and men had EQUAL early access rate",
      ggi_early_access_gap < 0 ~ paste0("Women were ", abs(round(ggi_early_access_gap * 100, 2)), 
                                  " percentage points LESS likely to get early loan approvals"),
      ggi_early_access_gap > 0 ~ paste0("Women were ", round(ggi_early_access_gap * 100, 2), 
                                  " percentage points MORE likely to get early loan approvals")
    ),
    # Add a simplified category for easy filtering
    gap_category = case_when(
      ggi_early_access_gap == 0 ~ "Equal Access",
      ggi_early_access_gap < 0 ~ "Women Disadvantaged",
      ggi_early_access_gap > 0 ~ "Women Advantaged"
    ),
    # Add a severity indicator (optional but helpful)
    severity = case_when(
      abs(ggi_early_access_gap) < 0.02 ~ "Minimal gap",
      abs(ggi_early_access_gap) < 0.05 ~ "Small gap",
      abs(ggi_early_access_gap) < 0.10 ~ "Moderate gap",
      abs(ggi_early_access_gap) >= 0.10 ~ "Large gap"
    )
  ) %>%
  select(state = BorrowerState, year, ggi_early_access_gap, female_rate, male_rate, 
         female_total, male_total, interpretation, gap_category, severity)
# ----------------------------------------------------------------------------
# STEP 4: Calculate GGI Timing Gap by State for 2021
# ----------------------------------------------------------------------------
Tggi_2021 <- gender_data_states %>%
  filter(Year == 2021, !is.na(Gender), Gender %in% c("Male Owned", "Female Owned")) %>%
  group_by(BorrowerState, Gender) %>%
  summarise(
    total_loans = n(),
    early_loans = sum(early_2021, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(BorrowerState) %>%
  summarise(
    female_rate = early_loans[Gender == "Female Owned"] / total_loans[Gender == "Female Owned"],
    male_rate = early_loans[Gender == "Male Owned"] / total_loans[Gender == "Male Owned"],
    female_total = total_loans[Gender == "Female Owned"],
    male_total = total_loans[Gender == "Male Owned"],
    .groups = "drop"
  ) %>%
  mutate(
    ggi_early_access_gap = female_rate - male_rate,
    year = 2021,
    # Add interpretation based on the GGI gap value
    interpretation = case_when(
      ggi_early_access_gap == 0 ~ "Women and men had EQUAL early access rate",
      ggi_early_access_gap < 0 ~ paste0("Women were ", abs(round(ggi_early_access_gap * 100, 2)), 
                                  " percentage points LESS likely to get early loan approvals"),
      ggi_early_access_gap > 0 ~ paste0("Women were ", round(ggi_early_access_gap * 100, 2), 
                                  " percentage points MORE likely to get early loan approvals")
    ),
    # Add a simplified category for easy filtering
    gap_category = case_when(
      ggi_early_access_gap == 0 ~ "Equal Access",
      ggi_early_access_gap < 0 ~ "Women Disadvantaged",
      ggi_early_access_gap > 0 ~ "Women Advantaged"
    ),
    # Add severity indicator
    severity = case_when(
      abs(ggi_early_access_gap) < 0.02 ~ "Minimal gap",
      abs(ggi_early_access_gap) < 0.05 ~ "Small gap",
      abs(ggi_early_access_gap) < 0.10 ~ "Moderate gap",
      abs(ggi_early_access_gap) >= 0.10 ~ "Large gap"
    )
  ) %>%
  select(state = BorrowerState, year, ggi_early_access_gap, female_rate, male_rate,
         female_total, male_total, interpretation, gap_category, severity)

#national results
national_2020 <- gender_data_states %>%
  filter(Year == 2020, Gender %in% c("Male Owned", "Female Owned")) %>%
  summarise(
    female_rate = mean(early_2020[Gender == "Female Owned"], na.rm = TRUE),
    male_rate = mean(early_2020[Gender == "Male Owned"], na.rm = TRUE),
    female_total = sum(Gender == "Female Owned"),
    male_total = sum(Gender == "Male Owned"),
    ggi_national = female_rate - male_rate,
    year = 2020
  )

national_2021 <- gender_data_states %>%
  filter(Year == 2021, Gender %in% c("Male Owned", "Female Owned")) %>%
  summarise(
    female_rate = mean(early_2021[Gender == "Female Owned"], na.rm = TRUE),
    male_rate = mean(early_2021[Gender == "Male Owned"], na.rm = TRUE),
    female_total = sum(Gender == "Female Owned"),
    male_total = sum(Gender == "Male Owned"),
    ggi_national = female_rate - male_rate,
    year = 2021
  )
#interpretation
cat("\n\n=== NATIONAL GGI TIMING GAP ===\n")
cat("\n2020 Results:\n")
cat("  Female early rate:", round(national_2020$female_rate, 4), 
    "(", national_2020$female_total, "loans)\n")
cat("  Male early rate:", round(national_2020$male_rate, 4),
    "(", national_2020$male_total, "loans)\n")
cat("  GGI Gap:", round(national_2020$ggi_national, 4), "\n")

cat("\n2021 Results:\n")
cat("  Female early rate:", round(national_2021$female_rate, 4),
    "(", national_2021$female_total, "loans)\n")
cat("  Male early rate:", round(national_2021$female_rate, 4),
    "(", national_2021$male_total, "loans)\n")
cat("  GGI Gap:", round(national_2021$ggi_national, 4), "\n")

# Interpretation
cat("\n=== INTERPRETATION ===\n")
if(national_2020$ggi_national < 0) {
  cat("2020: Women were", abs(round(national_2020$ggi_national * 100, 2)), 
      "percentage points LESS likely to get early approvals\n")
} else if(national_2020$ggi_national > 0) {
  cat("2020: Women were", round(national_2020$ggi_national * 100, 2), 
      "percentage points MORE likely to get early approvals\n")
} else {
  cat("2020: Women and men had equal early access\n")
}

if(national_2021$ggi_national < 0) {
  cat("2021: Women were", abs(round(national_2021$ggi_national * 100, 2)), 
      "percentage points LESS likely to get early approvals\n")
} else if(national_2021$ggi_national > 0) {
  cat("2021: Women were", round(national_2021$ggi_national * 100, 2), 
      "percentage points MORE likely to get early approvals\n")
} else {
  cat("2021: Women and men had equal early access\n")
}

# Change over time

gap_change <- national_2021$ggi_national - national_2020$ggi_national
cat("\nChange in GGI from 2020 to 2021:", round(gap_change, 4), "\n")
if(gap_change > 0) {
  cat("The timing gap IMPROVED for women from 2020 to 2021\n")
} else if(gap_change < 0) {
  cat("The timing gap WORSENED for women from 2020 to 2021\n")
} else {
  cat("No change in the timing gap\n")
}
#state level highlights
cat("\n\n=== STATE-LEVEL RESULTS ===\n")
cat("\nStates where women had LEAST early access in 2020 (most negative gap):\n")
ggi_2020 %>%
  arrange(ggi_early_access_gap) %>%
  head(10) %>%
  mutate(
    female_rate = round(female_rate, 4),
    male_rate = round(male_rate, 4),
    ggi_early_access_gap = round(ggi_early_access_gap, 4)
  ) %>%
  as.data.frame() %>%
  print()

cat("\nStates where women had MOST early access in 2021 (most positive gap):\n")
ggi_2021 %>%
  arrange(desc(ggi_early_access_gap)) %>%
  head(10) %>%
  mutate(
    female_rate = round(female_rate, 4),
    male_rate = round(male_rate, 4),
    ggi_early_access_gap = round(ggi_early_access_gap, 4)
  ) %>%
  as.data.frame() %>%
  print()

write_xlsx(Tggi_2020,"2020.xlsx")
write_xlsx(Tggi_2021,"2021.xlsx")
write_xlsx(national_2020,"national_2020.xlsx")
write_xlsx(national_2021,"national_2021.xlsx")

#3.Index 3: Loan Size Gap

#look at missing loan amount data

total_missing <- sum(is.na(gender_data_states$CurrentApprovalAmount))
total_rows <- nrow(gender_data_states)
# there is no missing data for current loan approval amount

ggi3 <- gender_data_states %>%
  group_by(BorrowerState, year, Gender) %>%
  summarise(
    avg_loan_amount = mean(CurrentApprovalAmount, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Convert "Male Owned" to "male" and "Female Owned" to "female"
  mutate(
    Gender = case_when(
      Gender == "Male Owned" ~ "avg_male_loan_amount",
      Gender == "Female Owned" ~ "avg_female_loan_amount",
      TRUE ~ tolower(Gender)
    )
  ) %>%
  select(BorrowerState, year, Gender, avg_loan_amount) %>%
  tidyr::pivot_wider(
    names_from = Gender,
    values_from = avg_loan_amount,
    values_fill = 0
  ) %>%
  mutate(
    # Calculate GGI Size Gap = ln(female loan amount / male loan amount)
    Loan_size_gap = log(avg_female_loan_amount / avg_male_loan_amount),  # Natural log of the ratio
    # Alternative: log(female) - log(male) gives same result
    # GGI_size_gap = log(female) - log(male),
    
    # Add interpretation
    interpretation = case_when(
      Loan_size_gap == 0 ~ "Equal loan sizes",
      Loan_size_gap < 0 ~ paste0("Females received ", abs(round(Loan_size_gap * 100, 2)), "% smaller loans on average"),
      Loan_size_gap > 0 ~ paste0("Females received ", round(Loan_size_gap * 100, 2), "% larger loans on average")
    )
  ) %>%
  arrange(year, BorrowerState)

print(ggi3)

ggi3_2020 <- ggi3 %>%
  filter(year == 2020)

# Filter and save 2021 data
ggi3_2021 <- ggi3 %>%
  filter(year == 2021)

write_xlsx(ggi3_2020,"ggi3_2020.xlsx")
write_xlsx(ggi3_2021,"ggi3_2021.xlsx")



#summary
size_summary_by_year <- ggi3 %>%
  group_by(year) %>%
  summarise(
    avg_size_gap = mean(Loan_size_gap, na.rm = TRUE),
    median_size_gap = median(Loan_size_gap, na.rm = TRUE),
    states_female_larger = sum(Loan_size_gap > 0, na.rm = TRUE),
    states_male_larger = sum(Loan_size_gap < 0, na.rm = TRUE),
    states_equal = sum(Loan_size_gap == 0, na.rm = TRUE),
    min_gap = min(Loan_size_gap, na.rm = TRUE),
    max_gap = max(Loan_size_gap, na.rm = TRUE)
  )

print("=== SIZE GAP SUMMARY STATISTICS BY YEAR ===")
print(size_summary_by_year)
write_xlsx(size_summary_by_year,"size_summary.xlsx")
# Find best and worst states for female loan sizes
best_states <- 
  ggi3 %>%
  filter(year == 2020) %>%
  arrange(desc(Loan_size_gap)) %>%
  head(5) %>%
  select(BorrowerState, Loan_size_gap, interpretation)

worst_states <- ggi3 %>%
  filter(year == 2020) %>%
  arrange(Loan_size_gap) %>%
  head(5) %>%
  select(BorrowerState, Loan_size_gap, interpretation)

best_states_2021 <- 
  ggi3 %>%
  filter(year == 2021) %>%
  arrange(desc(Loan_size_gap)) %>%
  head(5) %>%
  select(BorrowerState, Loan_size_gap, interpretation)

worst_states_2021 <- ggi3 %>%
  filter(year == 2021) %>%
  arrange(Loan_size_gap) %>%
  head(5) %>%
  select(BorrowerState, Loan_size_gap, interpretation)

write_xlsx(best_states,"best_2020.xlsx")
write_xlsx(worst_states,"worst_2020.xlsx")
write_xlsx(best_states_2021,"best_2021.xlsx")
write_xlsx(worst_states_2021,"worst_2021.xlsx")

# Index 1: representation gap

census_2020  <- read_excel("Number_of_firms_2020.xlsx")
census_2021 <- read_excel("Number_of_firms_2021.xlsx")

#final census files for 2020
census_2020<- census_2020 %>%
  mutate(TENE_clean = gsub("[^0-9.-]", "", as.character(TENE)),  # Keep only numbers, decimal, minus
         TENE = as.numeric(TENE_clean)) %>%
  select(-TENE_clean)  # Remove temporary column

census_2020_final <- census_2020 %>%
  pivot_wider(
    id_cols = c(BorrowerState, Year),
    names_from = `Meaning of Sex code`,
    values_from = TENE,
    values_fill = 0
  )

# Create total_firms (replace 'Male' and 'Female' with actual column names)
census_2020_main <- census_2020_final %>%
  mutate(
    total_firms = Female + Male  # Adjust these names if different
  ) %>%
  select(BorrowerState, Female, Male, total_firms, Year)

#final census files for 2021
census_2021<- census_2021 %>%
  mutate(TENE_clean = gsub("[^0-9.-]", "", as.character(TENE)),  # Keep only numbers, decimal, minus
         TENE = as.numeric(TENE_clean)) %>%
  select(-TENE_clean)  # Remove temporary column

census_2021_final <- census_2021 %>%
  pivot_wider(
    id_cols = c(BorrowerState, Year),
    names_from = `Meaning of Sex code`,
    values_from = TENE,
    values_fill = 0
  )

# Create total_firms (replace 'Male' and 'Female' with actual column names)
census_2021_main <- census_2021_final %>%
  mutate(
    total_firms = Female + Male  # Adjust these names if different
  ) %>%
  select(BorrowerState, Female, Male, total_firms, Year)

state_codes <- data.frame(
  BorrowerState = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                    "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
                    "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                    "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
                    "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
                    "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
                    "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                    "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming",
                    "District of Columbia"),
  StateCode = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
                "DC")
)

# Join with your census data
census_2020_main <- census_2020_main %>%
  left_join(state_codes, by = "BorrowerState")

census_2021_main <- census_2021_main %>%
  left_join(state_codes, by = "BorrowerState")

#calculate ratio for representation gap
#right hand side of the formula
census_2020_main <- census_2020_main %>%
  mutate(ratio_female = Female / total_firms)

census_2021_main <- census_2021_main %>%
  mutate(ratio_female = Female / total_firms)
#left hand side of the formula
representation_gap <- gender_data_states %>%
  group_by(BorrowerState, Year, Gender) %>%
  summarise(
    loan_count = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(BorrowerState, Year),
    names_from = Gender,
    values_from = loan_count,
    values_fill = 0
  ) %>%
  # Rename columns to remove spaces
  rename(
    Female = `Female Owned`,
    Male = `Male Owned`
  ) %>%
  mutate(
    total_loans = Female + Male,
    female_ppp_ratio = Female / total_loans,
    female_ppp_ratio = ifelse(total_loans == 0, 0, female_ppp_ratio)
  ) %>%
  select(BorrowerState, Year, Female, Male, total_loans, female_ppp_ratio)

representation_gap_2020 <- representation_gap %>%
  filter(Year == 2020)

representation_gap_2020 <- representation_gap_2020 %>%
  rename(StateCode = BorrowerState)

# Filter and save 2021 data
representation_gap_2021 <- representation_gap %>%
  filter(Year == 2021)
representation_gap_2021 <- representation_gap_2021 %>%
  rename(StateCode = BorrowerState)

ggi1_2020 <- census_2020_main %>%
  # Select only needed columns from census file
  select(StateCode, ratio_female, Year) %>%
  # Join with representation_gap_2020
  left_join(
    representation_gap_2020 %>% 
      select(StateCode, female_ppp_ratio),
    by = "StateCode"
  ) %>%
  # Create difference column
  mutate(
    representation_gap = female_ppp_ratio - ratio_female
  ) %>%
  # Rename columns for clarity
  rename(
    census_female_ratio = ratio_female,
    ppp_female_ratio = female_ppp_ratio
  )

ggi1_2021 <- census_2021_main %>%
  # Select only needed columns from census file
  select(StateCode, ratio_female, Year) %>%
  # Join with representation_gap_2021
  left_join(
    representation_gap_2021 %>% 
      select(StateCode, female_ppp_ratio),
    by = "StateCode"
  ) %>%
  # Create difference column
  mutate(
    representation_gap = female_ppp_ratio - ratio_female
  ) %>%
  # Rename columns for clarity
  rename(
    census_female_ratio = ratio_female,
    ppp_female_ratio = female_ppp_ratio
  )

ggi1_2020 <- ggi1_2020 %>%
  mutate(
    interpretation = case_when(
      representation_gap == 0 ~ "Female-owned firms received PPP loans exactly in proportion to their share of firms",
      representation_gap < 0 ~ "Female-owned firms were underrepresented among PPP loan recipients relative to their share of firms",
      representation_gap > 0 ~ "Female-owned firms were overrepresented among PPP loan recipients relative to their share of firms"
    ),
    # Optional: Add a shorter version for easier viewing
    interpretation_short = case_when(
      representation_gap == 0 ~ "Proportional",
      representation_gap < 0 ~ "Underrepresented",
      representation_gap > 0 ~ "Overrepresented"
    )
  )

ggi1_2021 <- ggi1_2021 %>%
  mutate(
    interpretation = case_when(
      representation_gap == 0 ~ "Female-owned firms received PPP loans exactly in proportion to their share of firms",
      representation_gap < 0 ~ "Female-owned firms were underrepresented among PPP loan recipients relative to their share of firms",
      representation_gap > 0 ~ "Female-owned firms were overrepresented among PPP loan recipients relative to their share of firms"
    ),
    # Optional: Add a shorter version for easier viewing
    interpretation_short = case_when(
      representation_gap == 0 ~ "Proportional",
      representation_gap < 0 ~ "Underrepresented",
      representation_gap > 0 ~ "Overrepresented"
    )
  )

write_xlsx(ggi1_2020,"ggi1_2020.xlsx")
write_xlsx(ggi1_2021,"ggi1_2021.xlsx")

ggi1_2020_summary <- ggi1_2020 %>%
  summarise(
    # Basic statistics
    mean_gap = mean(representation_gap, na.rm = TRUE),
    median_gap = median(representation_gap, na.rm = TRUE),
    sd_gap = sd(representation_gap, na.rm = TRUE),
    var_gap = var(representation_gap, na.rm = TRUE),
    min_gap = min(representation_gap, na.rm = TRUE),
    max_gap = max(representation_gap, na.rm = TRUE),
    range_gap = max_gap - min_gap,
    
    
    # Counts
    n_states = n(),
    n_missing = sum(is.na(representation_gap)),
    n_proportional = sum(representation_gap == 0, na.rm = TRUE),
    n_underrepresented = sum(representation_gap < 0, na.rm = TRUE),
    n_overrepresented = sum(representation_gap > 0, na.rm = TRUE),
    
    # Percentages
    pct_underrepresented = (n_underrepresented / n_states) * 100,
    pct_overrepresented = (n_overrepresented / n_states) * 100,
    pct_proportional = (n_proportional / n_states) * 100
  )

ggi1_2021_summary <- ggi1_2021 %>%
  summarise(
    # Basic statistics
    mean_gap = mean(representation_gap, na.rm = TRUE),
    median_gap = median(representation_gap, na.rm = TRUE),
    sd_gap = sd(representation_gap, na.rm = TRUE),
    var_gap = var(representation_gap, na.rm = TRUE),
    min_gap = min(representation_gap, na.rm = TRUE),
    max_gap = max(representation_gap, na.rm = TRUE),
    range_gap = max_gap - min_gap,
    
    
    # Counts
    n_states = n(),
    n_missing = sum(is.na(representation_gap)),
    n_proportional = sum(representation_gap == 0, na.rm = TRUE),
    n_underrepresented = sum(representation_gap < 0, na.rm = TRUE),
    n_overrepresented = sum(representation_gap > 0, na.rm = TRUE),
    
    # Percentages
    pct_underrepresented = (n_underrepresented / n_states) * 100,
    pct_overrepresented = (n_overrepresented / n_states) * 100,
    pct_proportional = (n_proportional / n_states) * 100
  )

write_xlsx(ggi1_2020_summary,"ggi1_2020_summary.xlsx")
write_xlsx(ggi1_2021_summary,"ggi1_2021_summary.xlsx")

#Access Gap
#step 1
ppp_loan_counts <- gender_data_states %>%
  group_by(BorrowerState, Year, Gender) %>%
  summarise(
    ppp_loans = n(),  # Count number of PPP loans
    .groups = "drop"
  ) %>%
  # Pivot to get separate columns for female and male
  pivot_wider(
    id_cols = c(BorrowerState, Year),
    names_from = Gender,
    values_from = ppp_loans,
    values_fill = 0
  )
#step 2
firm_counts_2020 <- census_2020_main %>%
  select(StateCode, Female, Male,Year) %>%
  distinct()  
firm_counts_2021 <- census_2021_main %>%
  select(StateCode, Female, Male,Year) %>%
  distinct() 
census_all <- bind_rows(firm_counts_2020, firm_counts_2021)

# Rename columns (adjust based on actual gender values in your data)
ppp_loan_counts <- ppp_loan_counts %>%
  rename(
    ppp_female = `Female Owned`,  # Change if your gender column has different values
    ppp_male = `Male Owned`       # Change if your gender column has different values
  )
#final calculation
ppp_loan_counts <- ppp_loan_counts %>%
  mutate(Year = as.character(Year))

# Then merge
access_gap_data <- ppp_loan_counts %>%
  # Rename BorrowerState to StateCode for merging
  rename(StateCode = BorrowerState) %>%
  # Join with census data
  left_join(census_all, by = c("StateCode", "Year")) %>%
  # Calculate participation rates and access gap
  mutate(
    # Female participation rate
    female_participation_rate = ppp_female / Female,
    
    # Male participation rate
    male_participation_rate = ppp_male / Male,
    
    # Access Gap (AG) = Female rate - Male rate
    access_gap = female_participation_rate - male_participation_rate,
  ) %>%
  # Rename census columns after calculation
  rename(
    census_female = Female,
    census_male = Male
  ) %>%
  # Add interpretation
  mutate(
    interpretation = case_when(
      access_gap == 0 ~ "Female- and male-owned firms had equal PPP participation rates",
      access_gap < 0 ~ "Female-owned firms had lower access to PPP loans than male-owned firms",
      access_gap > 0 ~ "Female-owned firms had higher access to PPP loans than male-owned firms"
    ),
    
    # Add short interpretation for easier viewing
    interpretation_short = case_when(
      access_gap == 0 ~ "Equal access",
      access_gap < 0 ~ "Female lower access",
      access_gap > 0 ~ "Female higher access"
    )
  )

access_gap_2020 <- access_gap_data %>%
  filter(Year == 2020)


access_gap_2021 <- access_gap_data %>%
  filter(Year == 2021)

write_xlsx(access_gap_2020,"access_gap_2020.xlsx")
write_xlsx(access_gap_2021,"access_gap_2021.xlsx")

#summary of access gap
summary_2020 <- access_gap_2020 %>%
  summarise(
    Year = 2020,
    # Basic statistics
    mean_ag = mean(access_gap, na.rm = TRUE),
    median_ag = median(access_gap, na.rm = TRUE),
    sd_ag = sd(access_gap, na.rm = TRUE),
    var_ag = var(access_gap, na.rm = TRUE),
    min_ag = min(access_gap, na.rm = TRUE),
    max_ag = max(access_gap, na.rm = TRUE),
    range_ag = max_ag - min_ag,
    
    # Counts by interpretation
    n_female_higher = sum(access_gap > 0, na.rm = TRUE),
    n_female_lower = sum(access_gap < 0, na.rm = TRUE),
    n_equal = sum(access_gap == 0, na.rm = TRUE),
    n_total = n(),
    
  )

# Summary statistics for 2021
summary_2021 <- access_gap_2021 %>%
  summarise(
    Year = 2021,
    # Basic statistics
    mean_ag = mean(access_gap, na.rm = TRUE),
    median_ag = median(access_gap, na.rm = TRUE),
    sd_ag = sd(access_gap, na.rm = TRUE),
    var_ag = var(access_gap, na.rm = TRUE),
    min_ag = min(access_gap, na.rm = TRUE),
    max_ag = max(access_gap, na.rm = TRUE),
    range_ag = max_ag - min_ag,
    
    
    # Counts by interpretation
    n_female_higher = sum(access_gap > 0, na.rm = TRUE),
    n_female_lower = sum(access_gap < 0, na.rm = TRUE),
    n_equal = sum(access_gap == 0, na.rm = TRUE),
    n_total = n(),
    
  )

write_xlsx(summary_2020,"summary_2020.xlsx")
write_xlsx(summary_2021,"summary_2021.xlsx")