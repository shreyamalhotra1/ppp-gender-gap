rm(list = ls())

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
setwd("C:\\Users\\shrey\\OneDrive\\Desktop")

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

#missing data analysis
missing_audit <- df_stacked %>%
  filter(year %in% c(2020, 2021)) %>%
  group_by(year) %>%
  summarise(
    total_loans = n(),
    
# Gender missing rate
    gender_missing = sum(is.na(Gender) | Gender == "Unanswered" | Gender == "Not Reported"),
    gender_missing_pct = gender_missing / total_loans * 100,
    
# Race missing rate  
    race_missing = sum(is.na(Race) | Race == "Unanswered" | Race == "Not Reported"),
    race_missing_pct = race_missing / total_loans * 100,
    
    
# NAICS missing rate 
    naics_missing = sum(is.na(naics_2digit)),
    naics_missing_pct = naics_missing / total_loans * 100,
    
    .groups = "drop"
  )
print("Missing Data Audit:")
print(missing_audit)

#missing data for jobs reported
missing_jobs_summary <- df_stacked %>%
  summarise(
    total_rows = n(),
    missing_jobs = sum(is.na(JobsReported) | JobsReported == ""),
    missing_jobs_pct = round(100 * missing_jobs / total_rows, 2),
    non_missing_jobs = sum(!is.na(JobsReported) & JobsReported != ""),
    non_missing_pct = round(100 * non_missing_jobs / total_rows, 2)
  )

print("Missing Data Summary for Jobs Reported:")
print(missing_jobs_summary)


# Isolate small businesses (sole proprietors,independent contractors and self-employed)
small_business_analysis <- df_stacked %>%
  filter(BusinessType %in% c("Sole Proprietorship", "Independent Contractor", "Self-Employed Individuals")) %>%
  group_by(year) %>%
  summarise(
    n_loans = n(),
    avg_loan = mean(CurrentApprovalAmount, na.rm = TRUE),
    median_loan = median(CurrentApprovalAmount, na.rm = TRUE),
    pct_zero_or_low = mean(CurrentApprovalAmount < 10000, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print("small_business_analysis Comparison (most affected by formula change):")
print(small_business_analysis)

# Compare with non-small business as control group
control_group <- df_stacked %>%
  filter(!BusinessType %in% c("Sole Proprietorship", "Independent Contractor", "Self-Employed Individuals")) %>%
  group_by(year) %>%
  summarise(
    n_loans = n(),
    avg_loan = mean(CurrentApprovalAmount, na.rm = TRUE),
    median_loan = median(CurrentApprovalAmount, na.rm = TRUE),
    pct_zero_or_low = mean(CurrentApprovalAmount < 10000, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print("Control Group Comparison (should show smaller changes):")
print(control_group)

#calculate percentage of loans given to small business
small_business_by_year <- df_stacked %>%
  filter(year %in% c(2020, 2021)) %>%
  group_by(year) %>%
  summarise(
    total_loans = n(),
    
    # Count small businesses
    small_business_count = sum(
      BusinessType %in% c(
        "Sole Proprietorship", 
        "Independent Contractor", 
        "Self-Employed Individuals"
      ), 
      na.rm = TRUE
    ),
    
    # Calculate percentage
    small_business_pct = (small_business_count / total_loans) * 100,
    
    # Also track missing data
    missing_type = sum(is.na(BusinessType) | BusinessType == ""),
    missing_pct = missing_type / total_loans * 100
  )

print("Small businesses Percentage by Year:")
print(small_business_by_year)

#basic descriptive comparsion
comparison_summary <- df_stacked %>%
  filter(year %in% c(2020, 2021)) %>%
  group_by(year, naics_2digit, industry_name) %>%
  summarise(
    # Loan counts
    loan_count = n(),
    pct_of_year_loans = n() / n() * 100,  # Will be calculated properly below
    
    # Loan amounts
    total_loan_amount = sum(CurrentApprovalAmount, na.rm = TRUE),
    avg_loan_amount = mean(CurrentApprovalAmount, na.rm = TRUE),
    median_loan_amount = median(CurrentApprovalAmount, na.rm = TRUE),
    sd_loan_amount = sd(CurrentApprovalAmount, na.rm = TRUE),
    min_loan = min(CurrentApprovalAmount, na.rm = TRUE),
    max_loan = max(CurrentApprovalAmount, na.rm = TRUE),
    
    # Jobs metrics (if available)
     total_jobs_reported = sum(JobsReported, na.rm = TRUE),
     avg_jobs_reported = mean(JobsReported, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  # Calculate percentage within each year
  group_by(year) %>%
  mutate(pct_of_year_loans = loan_count / sum(loan_count) * 100) %>%
  ungroup()

#visualisation
top_industries <- comparison_summary %>%
  group_by(year) %>%
  slice_max(loan_count, n = 10) %>%
  pull(naics_2digit) %>%
  unique()

comparison_summary %>%
  filter(naics_2digit %in% top_industries) %>%
  ggplot(aes(x = reorder(industry_name, loan_count), y = loan_count, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Top Industries by PPP Loan Count: 2020 vs 2021",
    x = "Industry Sector",
    y = "Number of Loans",
    fill = "Year",
    caption = "Based on 2-digit NAICS codes"
  ) +
  scale_fill_manual(values = c("2020" = "steelblue", "2021" = "darkred")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Calculate changes between years
yoy_changes <- comparison_summary %>%
  select(year, naics_2digit, industry_name, loan_count, total_loan_amount, 
         avg_loan_amount, pct_of_year_loans) %>%
  pivot_wider(
    id_cols = c(naics_2digit, industry_name),
    names_from = year,
    values_from = c(loan_count, total_loan_amount, avg_loan_amount, pct_of_year_loans),
    values_fill = 0
  ) %>%
  mutate(
    # Absolute changes
    loan_count_change = loan_count_2021 - loan_count_2020,
    loan_amount_change = total_loan_amount_2021 - total_loan_amount_2020,
    avg_loan_change = avg_loan_amount_2021 - avg_loan_amount_2020,
    market_share_change = pct_of_year_loans_2021 - pct_of_year_loans_2020,
    
    # Percentage changes
    loan_count_pct_change = (loan_count_2021 - loan_count_2020) / loan_count_2020 * 100,
    loan_amount_pct_change = (total_loan_amount_2021 - total_loan_amount_2020) / total_loan_amount_2020 * 100,
    avg_loan_pct_change = (avg_loan_amount_2021 - avg_loan_amount_2020) / avg_loan_amount_2020 * 100
  ) %>%
  arrange(desc(abs(loan_count_change)))

#Show which industries gained/lost market share (visualisation)
yoy_changes %>%
  filter(abs(market_share_change) > 0.5) %>%  # Filter significant changes
  ggplot(aes(x = reorder(industry_name, market_share_change), 
             y = market_share_change, 
             fill = market_share_change > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red"),
                    labels = c("TRUE" = "Gained Share in 2021", 
                               "FALSE" = "Lost Share in 2021")) +
  labs(
    title = "Market Share Shifts by Industry: 2020 to 2021",
    x = "Industry Sector",
    y = "Percentage Point Change in Loan Share",
    fill = "Direction",
    caption = "Positive values indicate higher share of loans in 2021"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")

#heatmap of industry concentration
comparison_summary %>%
  select(industry_name, year, pct_of_year_loans) %>%
  ggplot(aes(x = factor(year), y = reorder(industry_name, pct_of_year_loans), 
             fill = pct_of_year_loans)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue", 
                      labels = scales::percent_format(scale = 1)) +
  labs(
    title = "PPP Loan Concentration by Industry: 2020 vs 2021",
    x = "Year",
    y = "Industry Sector",
    fill = "% of Year's Loans"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12))

#Create a comprehensive comparison table
comparison_dashboard <- yoy_changes %>%
  select(
    industry_name,
    `2020 Loans` = loan_count_2020,
    `2021 Loans` = loan_count_2021,
    `Change` = loan_count_change,
    `% Change` = loan_count_pct_change,
    `2020 Avg Loan` = avg_loan_amount_2020,
    `2021 Avg Loan` = avg_loan_amount_2021,
    `Avg Change` = avg_loan_change,
    `Market Share 2020` = pct_of_year_loans_2020,
    `Market Share 2021` = pct_of_year_loans_2021,
    `Share Change` = market_share_change
  ) %>%
  mutate_if(is.numeric, ~round(., 2))

# View top gainers and losers
top_gainers <- comparison_dashboard %>% arrange(desc(`% Change`)) %>% head(10)
top_losers <- comparison_dashboard %>% arrange(`% Change`) %>% head(10)

print("Top 10 Industries with Largest Growth:")
print(top_gainers %>% select(industry_name, `2020 Loans`, `2021 Loans`, `% Change`))

print("Top 10 Industries with Largest Decline:")
print(top_losers %>% select(naics_sector, `2020 Loans`, `2021 Loans`, `% Change`))

#to see how two waves were different in terms of gender

#missing data diagnostic check

gender_missing_diagnostic <- df_stacked %>%
  filter(year %in% c(2020, 2021)) %>%
  group_by(year) %>%
  summarise(
    total_loans = n(),
    
    # Missing gender breakdown
    gender_na = sum(is.na(Gender)),
    gender_blank = sum(Gender == "", na.rm = TRUE),
    gender_not_reported = sum(Gender == "Unanswered", na.rm = TRUE),
    gender_missing_total = gender_na + gender_blank + gender_not_reported,
    gender_missing_pct = gender_missing_total / total_loans * 100,
    
    # Reported gender
    gender_reported = sum(Gender %in% c("Male Owned", "Female Owned"), na.rm = TRUE),
    gender_reported_pct = gender_reported / total_loans * 100,
    
    # Among reported, distribution
    male_count = sum(Gender == "Male Owned", na.rm = TRUE),
    female_count = sum(Gender == "Female Owned", na.rm = TRUE),
    male_pct_of_reported = male_count / gender_reported * 100,
    female_pct_of_reported = female_count / gender_reported * 100
  )

print("Missing Gender Data Diagnostic:")
print(gender_missing_diagnostic)

#unique gender values
unique_gender_values <- df_stacked %>%
  filter(year %in% c(2020, 2021)) %>%
  distinct(Gender) %>%
  pull()

print("Unique gender values in your data:")
print(unique_gender_values)

#Gender analysis (male, female and unanswered)
gender_analysis <- df_stacked %>%
  filter(year %in% c(2020, 2021)) %>%
  group_by(year, Gender) %>%
  summarise(
    loan_count = n(),
    total_amount = sum(CurrentApprovalAmount, na.rm = TRUE),
    avg_loan = mean(CurrentApprovalAmount, na.rm = TRUE),
    median_loan = median(CurrentApprovalAmount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(
    pct_of_total_loans = loan_count / sum(loan_count) * 100,
    pct_of_total_dollars = total_amount / sum(total_amount) * 100
  ) %>%
  ungroup()

print("Analysis Including All Gender Categories:")
print(gender_analysis)

#Analysis using only reported gender
gender_complete_case <- df_stacked %>%
  filter(year %in% c(2020, 2021)) %>%
  # Keep only reported gender (exclude Unanswered)
  filter(Gender %in% c("Male Owned", "Female Owned")) %>%
  group_by(year, Gender) %>%
  summarise(
    loan_count = n(),
    total_amount = sum(CurrentApprovalAmount, na.rm = TRUE),
    avg_loan = mean(CurrentApprovalAmount, na.rm = TRUE),
    median_loan = median(CurrentApprovalAmount, na.rm = TRUE),
    sd_loan = sd(CurrentApprovalAmount, na.rm = TRUE),
    min_loan = min(CurrentApprovalAmount, na.rm = TRUE),
    max_loan = max(CurrentApprovalAmount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(
    # Percentage within reported cases only
    pct_of_reported_loans = loan_count / sum(loan_count) * 100,
    pct_of_reported_dollars = total_amount / sum(total_amount) * 100
  ) %>%
  ungroup()

print("Complete Case Analysis (Reported Gender Only):")
print(gender_complete_case)
 
#industry-wise comparison for reported gender

# Calculate gender by industry statistics
gender_by_industry <- df_stacked %>%
  filter(year %in% c(2020, 2021)) %>%
  filter(Gender %in% c("Male Owned", "Female Owned")) %>%
  group_by(year, naics_2digit, industry_name, Gender) %>%
  summarise(
    loan_count = n(),
    avg_loan = mean(CurrentApprovalAmount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year, naics_2digit, industry_name) %>%
  mutate(
    industry_total_reported = sum(loan_count),
    female_share = sum(loan_count[Gender == "Female Owned"]) / industry_total_reported * 100,
    male_share = sum(loan_count[Gender == "Male Owned"]) / industry_total_reported * 100,
    female_loans = sum(loan_count[Gender == "Female Owned"]),
    male_loans = sum(loan_count[Gender == "Male Owned"])
  ) %>%
  filter(industry_total_reported >= 30) %>%  # Filter for reliability
  ungroup()

# ============================================================================
# TOP INDUSTRIES FOR FEMALE OWNERSHIP
# ============================================================================

top_female_industries <- gender_by_industry %>%
  filter(Gender == "Female Owned") %>%
  group_by(year) %>%
  slice_max(female_share, n = 5, with_ties = FALSE) %>%
  select(
    year, 
    naics_2digit, 
    industry_name, 
    female_share, 
    female_loans,
    industry_total_reported,
    avg_loan
  ) %>%
  arrange(year, desc(female_share))

cat("\n==================================================\n")
cat("TOP 5 INDUSTRIES FOR FEMALE OWNERSHIP (Reported Only):\n")
cat("==================================================\n")
print(top_female_industries)

# ============================================================================
# TOP INDUSTRIES FOR MALE OWNERSHIP
# ============================================================================

top_male_industries <- gender_by_industry %>%
  filter(Gender == "Male Owned") %>%
  group_by(year) %>%
  slice_max(male_share, n = 5, with_ties = FALSE) %>%
  select(
    year, 
    naics_2digit, 
    industry_name, 
    male_share, 
    male_loans,
    industry_total_reported,
    avg_loan
  ) %>%
  arrange(year, desc(male_share))

cat("\n==================================================\n")
cat("TOP 5 INDUSTRIES FOR MALE OWNERSHIP (Reported Only):\n")
cat("==================================================\n")
print(top_male_industries)

# ============================================================================
# BOTTOM INDUSTRIES FOR FEMALE OWNERSHIP (where women are underrepresented)
# ============================================================================

bottom_female_industries <- gender_by_industry %>%
  filter(Gender == "Female Owned") %>%
  group_by(year) %>%
  slice_min(female_share, n = 5, with_ties = FALSE) %>%
  select(
    year, 
    naics_2digit, 
    industry_name, 
    female_share, 
    female_loans,
    industry_total_reported,
    avg_loan
  ) %>%
  arrange(year, female_share)

cat("\n==================================================\n")
cat("BOTTOM 5 INDUSTRIES FOR FEMALE OWNERSHIP (Most Male-Dominated):\n")
cat("==================================================\n")
print(bottom_female_industries)

#gender gap analysis
gender_gap_analysis <- gender_by_industry %>%
  distinct(year, naics_2digit, industry_name, .keep_all = TRUE) %>%
  select(-Gender, -loan_count, -avg_loan) %>%
  mutate(
    gender_gap = male_share - female_share,
    female_representation = case_when(
      female_share >= 50 ~ "Female Majority",
      female_share >= 40 ~ "Near Parity",
      female_share >= 30 ~ "Moderate Female",
      TRUE ~ "Male Dominated"
    )
  )

# Industries with largest gender gap (most male-dominated)
largest_gap <- gender_gap_analysis %>%
  group_by(year) %>%
  slice_max(gender_gap, n = 5, with_ties = FALSE) %>%
  select(year, industry_name, male_share, female_share, gender_gap, industry_total_reported)

cat("\n==================================================\n")
cat("TOP 5 INDUSTRIES WITH LARGEST GENDER GAP (Most Male-Dominated):\n")
cat("==================================================\n")
print(largest_gap)

# Industries with smallest gender gap (most balanced)
smallest_gap <- gender_gap_analysis %>%
  group_by(year) %>%
  slice_min(abs(gender_gap), n = 5, with_ties = FALSE) %>%
  select(year, industry_name, male_share, female_share, gender_gap, industry_total_reported)

cat("\n==================================================\n")
cat("TOP 5 MOST GENDER-BALANCED INDUSTRIES (Smallest Gap):\n")
cat("==================================================\n")
print(smallest_gap)

# ============================================================================
# SUMMARY STATISTICS BY YEAR
# ============================================================================

gender_summary_by_year <- gender_by_industry %>%
  group_by(year, Gender) %>%
  summarise(
    total_loans = sum(loan_count),
    weighted_avg_loan = weighted.mean(avg_loan, loan_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Gender,
    values_from = c(total_loans, weighted_avg_loan),
    values_fill = 0
  )

cat("\n==================================================\n")
cat("SUMMARY STATISTICS BY YEAR:\n")
cat("==================================================\n")
print(gender_summary_by_year)

#visualisation


# Combine top female and male industries for visualization
top_female_for_viz <- top_female_industries %>%
  mutate(category = "Female Top 5", share = female_share)

top_male_for_viz <- top_male_industries %>%
  mutate(category = "Male Top 5", share = male_share)

top_combined <- bind_rows(top_female_for_viz, top_male_for_viz)

# Create faceted bar chart
ggplot(top_combined, aes(x = reorder(industry_name, share), y = share, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~year, ncol = 2) +
  scale_fill_manual(values = c("Female Top 5" = "coral", "Male Top 5" = "steelblue")) +
  labs(
    title = "Top Industries by Gender Ownership (Reported Only)",
    subtitle = "Percentage of loans within each industry",
    x = "Industry",
    y = "Percentage of Industry Loans",
    fill = "Category",
    caption = "Based on borrowers who disclosed gender"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
