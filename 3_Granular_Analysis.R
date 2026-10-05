rm(list = ls())
#Step 1
# Load necessary libraries
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("stringr")
install.packages("scales")
install.packages("ggrepel")
install.packages("broom")
install.packages("knitr")
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(tidyverse)
library(scales) # For formatting numbers on plots
library(ggrepel) # For clean labels on plots
library(broom) # For tidying model outputs
library(knitr) # For making tables

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


#creating NAICS_2 digit variable (adds 1 variable to df_stacked)
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

#creating year variable (total 56 variables in df_stacked)
df_stacked$year <- format(as.Date(df_stacked$DateApproved, format="%m/%d/%Y"),"%Y")

#save RDS file for future use
saveRDS(df_stacked, "df_stacked.rds")

# To load back into R
df_stacked <- readRDS("df_stacked.rds")

#Step 2 filter for male and female
gender_data <- df_stacked %>%
  filter(Gender %in% c("Male Owned", "Female Owned")) %>%
  mutate(
    year = as.factor(year),
    gender = as.factor(Gender)
  )
# Add log-transformed loan amounts to the original gender_data
gender_data <- gender_data %>%
  mutate(
    log_loan_amount = log(CurrentApprovalAmount + 1),  # +1 to handle zero values
  )


 #calculate summary 

state_gender_summary_log <- gender_data %>%
  group_by(BorrowerState, year, gender) %>%
  summarise(
    total_loans = n(),
    total_amount = sum(CurrentApprovalAmount, na.rm = TRUE),
    avg_loan_amount = mean(CurrentApprovalAmount, na.rm = TRUE),
    median_loan_amount = median(CurrentApprovalAmount, na.rm = TRUE),
    # Log-transformed statistics
    avg_log_loan = mean(log_loan_amount, na.rm = TRUE),
    median_log_loan = median(log_loan_amount, na.rm = TRUE),
    sd_log_loan = sd(log_loan_amount, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate comprehensive gender gaps for each state and year
gender_gaps_comprehensive <- state_gender_summary_log %>%
  group_by(BorrowerState, year) %>%
  summarise(
    # Loan count metrics
    male_loans = sum(total_loans[gender == "Male Owned"]),
    female_loans = sum(total_loans[gender == "Female Owned"]),
    
    # Loan amount metrics (original scale)
    male_amount = sum(total_amount[gender == "Male Owned"]),
    female_amount = sum(total_amount[gender == "Female Owned"]),
    male_avg = mean(avg_loan_amount[gender == "Male Owned"]),
    female_avg = mean(avg_loan_amount[gender == "Female Owned"]),
    male_median = mean(median_loan_amount[gender == "Male Owned"]),
    female_median = mean(median_loan_amount[gender == "Female Owned"]),
    
    # Log-transformed metrics
    male_avg_log = mean(avg_log_loan[gender == "Male Owned"]),
    female_avg_log = mean(avg_log_loan[gender == "Female Owned"]),
    male_median_log = mean(median_log_loan[gender == "Male Owned"]),
    female_median_log = mean(median_log_loan[gender == "Female Owned"]),
    
    # Calculate gaps (original scale)
    loan_count_gap = male_loans - female_loans,
    loan_amount_gap = male_amount - female_amount,
    avg_amount_gap = male_avg - female_avg,
    median_amount_gap = male_median - female_median,
    
    # Calculate ratios (original scale)
    loan_count_ratio = male_loans / female_loans,
    loan_amount_ratio = male_amount / female_amount,
    avg_amount_ratio = male_avg / female_avg,
    median_amount_ratio = male_median / female_median,
    
    # Calculate log-based gaps (these represent proportional differences)
    avg_log_gap = male_avg_log - female_avg_log,
    median_log_gap = male_median_log - female_median_log,
    
    # Convert log gaps to percentage differences
    avg_percent_diff = (exp(avg_log_gap) - 1) * 100,
    median_percent_diff = (exp(median_log_gap) - 1) * 100,
    
    # Female representation metrics
    female_percentage = (female_loans / (male_loans + female_loans)) * 100,
    female_amount_percentage = (female_amount / (male_amount + female_amount)) * 100,
    
    .groups = 'drop'
  )

#Top 10 states with largest loan count gap (most male-dominated)
top10_count_gap <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(desc(loan_count_gap)) %>%
  slice_head(n = 10) %>%
  select(BorrowerState, year, loan_count_gap, male_loans, female_loans, female_percentage)

print("=== TOP 10 STATES - LARGEST LOAN COUNT GAP ===")
print(top10_count_gap)

# Top 10 states with largest loan amount gap
top10_amount_gap <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(desc(loan_amount_gap)) %>%
  slice_head(n = 10) %>%
  select(BorrowerState, year, loan_amount_gap, male_amount, female_amount, female_amount_percentage)

print("=== TOP 10 STATES - LARGEST LOAN AMOUNT GAP ===")
print(top10_amount_gap)

# Top 10 states with largest avg loan amount gap (original scale)
top10_avg_gap <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(desc(avg_amount_gap)) %>%
  slice_head(n = 10) %>%
  select(BorrowerState, year, avg_amount_gap, male_avg, female_avg)

print("=== TOP 10 STATES - LARGEST AVERAGE LOAN AMOUNT GAP ===")
print(top10_avg_gap)

# Top 10 states with largest proportional gap (based on log transformation)
top10_proportional_gap <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(desc(avg_percent_diff)) %>%
  slice_head(n = 10) %>%
  select(BorrowerState, year, avg_percent_diff, avg_log_gap, male_avg, female_avg)

print("=== TOP 10 STATES - LARGEST PROPORTIONAL GAP (%) ===")
print(top10_proportional_gap)

# Bottom 10 states with smallest loan count gap
bottom10_count_gap <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(loan_count_gap) %>%
  slice_head(n = 10) %>%
  select(BorrowerState, year, loan_count_gap, male_loans, female_loans, female_percentage)

print("=== BOTTOM 10 STATES - SMALLEST LOAN COUNT GAP ===")
print(bottom10_count_gap)

# Bottom 10 states with smallest loan amount gap
bottom10_amount_gap <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(loan_amount_gap) %>%
  slice_head(n = 10) %>%
  select(BorrowerState, year, loan_amount_gap, male_amount, female_amount, female_amount_percentage)

print("=== BOTTOM 10 STATES - SMALLEST LOAN AMOUNT GAP ===")
print(bottom10_amount_gap)

# Bottom 10 states with smallest avg loan amount gap
bottom10_avg_gap <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(avg_amount_gap) %>%
  slice_head(n = 10) %>%
  select(BorrowerState, year, avg_amount_gap, male_avg, female_avg)

print("=== BOTTOM 10 STATES - SMALLEST AVERAGE LOAN AMOUNT GAP ===")
print(bottom10_avg_gap)

# Bottom 10 states with smallest proportional gap (negative means female avg > male avg)
bottom10_proportional_gap <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(avg_percent_diff) %>%
  slice_head(n = 10) %>%
  select(BorrowerState, year, avg_percent_diff, avg_log_gap, male_avg, female_avg)

print("=== BOTTOM 10 STATES - SMALLEST PROPORTIONAL GAP (or female-favored) ===")
print(bottom10_proportional_gap)

# Top 10 states with highest female representation (by loan count)
top10_female_rep_count <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(desc(female_percentage)) %>%
  slice_head(n = 10) %>%
  select(BorrowerState, year, female_percentage, female_loans, male_loans)

print("=== TOP 10 STATES - HIGHEST FEMALE REPRESENTATION (% of loans) ===")
print(top10_female_rep_count)

# Top 10 states with highest female representation (by loan amount)
top10_female_rep_amount <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(desc(female_amount_percentage)) %>%
  slice_head(n = 10) %>%
  select(BorrowerState, year, female_amount_percentage, female_amount, male_amount)

print("=== TOP 10 STATES - HIGHEST FEMALE REPRESENTATION (% of total amount) ===")
print(top10_female_rep_amount)

# Bottom 10 states with lowest female representation (by loan count)
bottom10_female_rep_count <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(female_percentage) %>%
  slice_head(n = 10) %>%
  select(BorrowerState, year, female_percentage, female_loans, male_loans)

print("=== BOTTOM 10 STATES - LOWEST FEMALE REPRESENTATION (% of loans) ===")
print(bottom10_female_rep_count)

# Bottom 10 states with lowest female representation (by loan amount)
bottom10_female_rep_amount <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(female_amount_percentage) %>%
  slice_head(n = 10) %>%
  select(BorrowerState, year, female_amount_percentage, female_amount, male_amount)

print("=== BOTTOM 10 STATES - LOWEST FEMALE REPRESENTATION (% of total amount) ===")
print(bottom10_female_rep_amount)

# Create a composite score for gender gap (lower score = smaller gap)
gender_gaps_scored <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  mutate(
    # Normalize scores (0-100 scale)
    count_gap_score = (loan_count_gap - min(loan_count_gap)) / (max(loan_count_gap) - min(loan_count_gap)) * 100,
    amount_gap_score = (loan_amount_gap - min(loan_amount_gap)) / (max(loan_amount_gap) - min(loan_amount_gap)) * 100,
    prop_gap_score = (avg_percent_diff - min(avg_percent_diff)) / (max(avg_percent_diff) - min(avg_percent_diff)) * 100,
    
    # Composite score (average of all gap metrics)
    composite_gap_score = (count_gap_score + amount_gap_score + prop_gap_score) / 3,
    
    # Rank states
    rank_composite = rank(composite_gap_score),
    rank_composite_desc = rank(desc(composite_gap_score))
  ) %>%
  ungroup()

# Top 10 states with highest composite gap
top10_composite <- gender_gaps_scored %>%
  group_by(year) %>%
  arrange(desc(composite_gap_score)) %>%
  slice_head(n = 10) %>%
  select(BorrowerState, year, composite_gap_score, female_percentage, 
         loan_count_gap, avg_percent_diff, rank_composite)

print("=== TOP 10 STATES - HIGHEST COMPOSITE GENDER GAP ===")
print(top10_composite)

# Bottom 10 states with lowest composite gap
bottom10_composite <- gender_gaps_scored %>%
  group_by(year) %>%
  arrange(composite_gap_score) %>%
  slice_head(n = 10) %>%
  select(BorrowerState, year, composite_gap_score, female_percentage, 
         loan_count_gap, avg_percent_diff, rank_composite_desc)

print("=== BOTTOM 10 STATES - LOWEST COMPOSITE GENDER GAP ===")
print(bottom10_composite)

# Calculate changes from 2020 to 2021
gender_gaps_changes <- gender_gaps_comprehensive %>%
  select(BorrowerState, year, female_percentage, avg_percent_diff, 
         loan_count_ratio, loan_amount_ratio) %>%
  pivot_wider(
    id_cols = BorrowerState,
    names_from = year,
    values_from = c(female_percentage, avg_percent_diff, loan_count_ratio, loan_amount_ratio),
    names_sep = "_"
  ) %>%
  mutate(
    # Calculate changes
    female_pct_change = female_percentage_2021 - female_percentage_2020,
    gap_change = avg_percent_diff_2021 - avg_percent_diff_2020,
    count_ratio_change = loan_count_ratio_2021 - loan_count_ratio_2020,
    
    # States with most improvement (negative gap_change means gap decreased)
    improved_gap = gap_change < 0,
    improved_representation = female_pct_change > 0
  )

# Top 10 states with most improvement (largest decrease in gap)
top10_improved <- gender_gaps_changes %>%
  arrange(gap_change) %>%  # Most negative first (biggest improvement)
  head(10) %>%
  select(BorrowerState, gap_change, female_pct_change, 
         female_percentage_2020, female_percentage_2021)

print("=== TOP 10 STATES - MOST IMPROVED GENDER GAP (2020 to 2021) ===")
print(top10_improved)

# Top 10 states with most worsening (largest increase in gap)
top10_worsened <- gender_gaps_changes %>%
  arrange(desc(gap_change)) %>%  # Most positive first (biggest worsening)
  head(10) %>%
  select(BorrowerState, gap_change, female_pct_change, 
         female_percentage_2020, female_percentage_2021)

print("=== TOP 10 STATES - MOST WORSENED GENDER GAP (2020 to 2021) ===")
print(top10_worsened)

#visualisations

#Load required libraries
install.packages("viridis")
install.packages("patchwork")
install.packages("ggpubr")
library(viridis)
library(patchwork)
library(ggpubr)

# 1.1 Distribution of Gender Gaps Across States (Histogram)
gap_distribution_plot <- gender_gaps_comprehensive %>%
  ggplot(aes(x = avg_percent_diff, fill = as.factor(year))) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20, color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.8) +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.7) +
  labs(title = "Distribution of Gender Gaps Across States",
       subtitle = "Positive values indicate male-favored gaps",
       x = "Gender Gap (% difference: Male vs Female Avg Loan)",
       y = "Number of States",
       fill = "Year") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom")

print(gap_distribution_plot)

#2.1 Top 10 States with Largest Gender Gap (Bar Plot)
top10_gap_plot <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(desc(avg_percent_diff)) %>%
  slice_head(n = 10) %>%
  mutate(BorrowerState = reorder(BorrowerState, avg_percent_diff)) %>%
  ggplot(aes(x = BorrowerState, y = avg_percent_diff, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(avg_percent_diff, 1), "%")),
            position = position_dodge(width = 0.7), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.7) +
  labs(title = "Top 10 States with Largest Gender Gap",
       subtitle = "Positive values indicate male loans > female loans",
       x = "State", y = "Gender Gap (%)",
       fill = "Year") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom")

print(top10_gap_plot)

# 2.2 Bottom 10 States with Smallest Gender Gap
bottom10_gap_plot <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(avg_percent_diff) %>%
  slice_head(n = 10) %>%
  mutate(BorrowerState = reorder(BorrowerState, desc(avg_percent_diff))) %>%
  ggplot(aes(x = BorrowerState, y = avg_percent_diff, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(avg_percent_diff, 1), "%")),
            position = position_dodge(width = 0.7), hjust = 1.1, size = 3) +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.7) +
  labs(title = "Bottom 10 States with Smallest Gender Gap",
       subtitle = "Negative values indicate female loans > male loans",
       x = "State", y = "Gender Gap (%)",
       fill = "Year") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom")

print(bottom10_gap_plot)


# 3.1 Top 10 States by Female Representation
top10_female_plot <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(desc(female_percentage)) %>%
  slice_head(n = 10) %>%
  mutate(BorrowerState = reorder(BorrowerState, female_percentage)) %>%
  ggplot(aes(x = BorrowerState, y = female_percentage, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(female_percentage, 1), "%")),
            position = position_dodge(width = 0.7), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.7) +
  labs(title = "Top 10 States with Highest Female Representation",
       subtitle = "Percentage of loans to female-owned businesses",
       x = "State", y = "Female Representation (%)",
       fill = "Year") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom")

print(top10_female_plot)

# 3.2 Bottom 10 States by Female Representation
bottom10_female_plot <- gender_gaps_comprehensive %>%
  group_by(year) %>%
  arrange(female_percentage) %>%
  slice_head(n = 10) %>%
  mutate(BorrowerState = reorder(BorrowerState, desc(female_percentage))) %>%
  ggplot(aes(x = BorrowerState, y = female_percentage, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(female_percentage, 1), "%")),
            position = position_dodge(width = 0.7), hjust = 1.1, size = 3) +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.7) +
  labs(title = "Bottom 10 States with Lowest Female Representation",
       subtitle = "Percentage of loans to female-owned businesses",
       x = "State", y = "Female Representation (%)",
       fill = "Year") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom")

print(bottom10_female_plot)

# 4.1 Relationship between Loan Count and Gender Gap
scatter_plot <- gender_gaps_comprehensive %>%
  mutate(total_loans = male_loans + female_loans) %>%  # Create the variable first
  ggplot(aes(x = total_loans, 
             y = avg_percent_diff, 
             color = as.factor(year),
             size = female_percentage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.7) +
  scale_size_continuous(range = c(2, 8)) +
  labs(title = "Relationship between Loan Volume and Gender Gap",
       subtitle = "Size of points indicates female representation",
       x = "Total Number of Loans", 
       y = "Gender Gap (%)",
       color = "Year",
       size = "Female %") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
# Display the plot
print(scatter_plot)

#loan amount vs gender gap
amount_scatter <- gender_gaps_comprehensive %>%
  ggplot(aes(x = log10(male_amount + female_amount), 
             y = avg_percent_diff, 
             color = as.factor(year),
             size = female_percentage)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.7) +
  scale_size_continuous(range = c(2, 8)) +  # Added missing size scale
  labs(title = "Total Loan Amount vs Gender Gap",
       subtitle = "X-axis is log10 transformed | Point size = Female %",
       x = "Log10(Total Loan Amount)", 
       y = "Gender Gap (%)",
       color = "Year",
       size = "Female %") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

# 5.1 Change in Gender Gap from 2020 to 2021
gap_change_plot <- gender_gaps_changes %>%
  arrange(desc(abs(gap_change))) %>%
  head(20) %>%
  ggplot(aes(x = reorder(BorrowerState, gap_change), y = gap_change, fill = gap_change > 0)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(gap_change, 1), "%")), 
            hjust = ifelse(gender_gaps_changes %>% 
                             arrange(desc(abs(gap_change))) %>% 
                             head(20) %>% .$gap_change > 0, -0.1, 1.1),
            size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = "darkgreen"),
                    labels = c("Improved", "Worsened")) +
  labs(title = "Top 20 States: Change in Gender Gap (2020 to 2021)",
       subtitle = "Negative values indicate improvement (gap decreased)",
       x = "State", y = "Change in Gender Gap (percentage points)",
       fill = "Change Direction") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom")

print(gap_change_plot)

# 5.2 Before-After Comparison for Selected States
selected_states <- c(gender_gaps_changes %>% arrange(gap_change) %>% head(5) %>% .$BorrowerState,
                     gender_gaps_changes %>% arrange(desc(gap_change)) %>% head(5) %>% .$BorrowerState)

before_after_plot <- gender_gaps_comprehensive %>%
  filter(BorrowerState %in% selected_states) %>%
  ggplot(aes(x = as.factor(year), y = avg_percent_diff, group = BorrowerState, color = BorrowerState)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = paste0(round(avg_percent_diff, 1), "%")), 
                  box.padding = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Gender Gap Changes: Most Improved vs Most Worsened States",
       subtitle = "Comparing 2020 and 2021",
       x = "Year", y = "Gender Gap (%)",
       color = "State") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "right")

print(before_after_plot)
print(amount_scatter)

#6.1 Heatmap of Gender Gaps Across States
heatmap_data <- gender_gaps_comprehensive %>%
  select(BorrowerState, year, avg_percent_diff, female_percentage) %>%
  mutate(gap_category = case_when(
    avg_percent_diff > 50 ~ "Very High Male Favored",
    avg_percent_diff > 20 ~ "High Male Favored",
    avg_percent_diff > 5 ~ "Moderate Male Favored",
    avg_percent_diff > -5 ~ "Near Equal",
    avg_percent_diff > -20 ~ "Moderate Female Favored",
    TRUE ~ "High Female Favored"
  ))

heatmap_plot <- ggplot(heatmap_data, 
                       aes(x = as.factor(year), 
                           y = reorder(BorrowerState, avg_percent_diff), 
                           fill = avg_percent_diff)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred", 
                       midpoint = 0, name = "Gap %") +
  labs(title = "Heatmap of Gender Gaps Across States",
       subtitle = "Green = Female-favored, Red = Male-favored",
       x = "Year", y = "State") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 8),
        legend.position = "right")

print(heatmap_plot)

# 7.1 Composite Gap Score - Top and Bottom
composite_plot <- gender_gaps_scored %>%
  group_by(year) %>%
  filter(rank_composite <= 10 | rank_composite_desc <= 10) %>%
  mutate(group = ifelse(rank_composite <= 10, "Bottom 10 (Smallest Gap)", "Top 10 (Largest Gap)")) %>%
  ggplot(aes(x = reorder(BorrowerState, composite_gap_score), 
             y = composite_gap_score, 
             fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year) +
  coord_flip() +
  scale_fill_manual(values = c("Top 10 (Largest Gap)" = "darkred", 
                               "Bottom 10 (Smallest Gap)" = "darkgreen")) +
  labs(title = "Composite Gender Gap Score: Top 10 vs Bottom 10 States",
       subtitle = "Higher score indicates larger gender gap",
       x = "State", y = "Composite Gap Score",
       fill = "Category") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom")

print(composite_plot)

#plot gender gap on a US map

#looking at missing data of borrower state
# Calculate percentage of missing BorrowerState values
total_rows <- nrow(gender_data)
missing_state <- sum(is.na(gender_data$BorrowerState) | gender_data$BorrowerState == "" | gender_data$BorrowerState == " ")
missing_percentage <- (missing_state / total_rows) * 100

print(paste("Total rows:", total_rows))
print(paste("Missing BorrowerState:", missing_state))
print(paste("Percentage missing:", round(missing_percentage, 2), "%"))

# Missing data breakdown by year
missing_by_year <- gender_data %>%
  group_by(year) %>%
  summarise(
    total_rows = n(),
    missing_state = sum(is.na(BorrowerState) | BorrowerState == "" | BorrowerState == " "),
    missing_pct = (missing_state / total_rows) * 100
  )

print("Missing data by year:")
print(missing_by_year)

#the percentage is missing data for borrower state is very low(0.00079%), therefore not recalculating gender gap after excluding missing states
# Clean the data by removing rows with missing BorrowerState
gender_data_clean <- gender_data %>%
  filter(!is.na(BorrowerState) & BorrowerState != "" & BorrowerState != " ")

# Verify the impact
total_original <- nrow(gender_data)
total_clean <- nrow(gender_data_clean)
removed <- total_original - total_clean
removed_pct <- (removed / total_original) * 100

print(paste("Original rows:", total_original))
print(paste("Clean rows:", total_clean))
print(paste("Rows removed:", removed))
print(paste("Percentage removed:", round(removed_pct, 2), "%"))

#plot gender gap on US map

# Load required libraries

install.packages("maps")
install.packages("mapdata")
library(maps)
library(mapdata)

# Get US states map data
us_states <- map_data("state")

# Create a state mapping dataframe (to match your state abbreviations to full names)
state_mapping <- data.frame(
  state_abbr = state.abb,
  state_full = tolower(state.name)
)

# Define valid US state abbreviations (50 states + DC)
valid_states <- c(state.abb, "DC")

# Check what territories/other values exist in your data
all_states_in_data <- unique(gender_data$BorrowerState)
territories_found <- setdiff(all_states_in_data[!is.na(all_states_in_data)], valid_states)

print("Territories and invalid codes found in data:")
print(territories_found)

# Remove territories and keep only valid US states + DC
gender_data_main <- gender_data %>%
  filter(BorrowerState %in% valid_states)

# Check how many rows were removed
rows_before <- nrow(gender_data)
rows_after <- nrow(gender_data_main)
rows_removed <- rows_before - rows_after
removed_percent <- (rows_removed / rows_before) * 100

print(paste("Rows before removing territories:", rows_before))
print(paste("Rows after removing territories:", rows_after))
print(paste("Rows removed:", rows_removed))
print(paste("Percentage removed:", round(removed_percent, 2), "%"))
#after removing territories data, 1% of the data is removed

# Add DC if it's in your data
state_mapping <- rbind(state_mapping, data.frame(state_abbr = "DC", state_full = "district of columbia"))

state_summary <- gender_data_main %>%
  group_by(BorrowerState, year, gender) %>%
  summarise(
    total_loans = n(),
    total_amount = sum(CurrentApprovalAmount, na.rm = TRUE),
    avg_loan_amount = mean(CurrentApprovalAmount, na.rm = TRUE),
    median_loan_amount = median(CurrentApprovalAmount, na.rm = TRUE),
    avg_log_loan = mean(log_loan_amount, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate gender gaps for each state and year
gender_gaps1 <- state_summary %>%
  group_by(BorrowerState, year) %>%
  summarise(
    # Loan counts
    male_loans = sum(total_loans[gender == "Male Owned"]),
    female_loans = sum(total_loans[gender == "Female Owned"]),
    
    # Loan amounts
    male_amount = sum(total_amount[gender == "Male Owned"]),
    female_amount = sum(total_amount[gender == "Female Owned"]),
    
    # Log-transformed averages (for proportional differences)
    male_avg_log = mean(avg_log_loan[gender == "Male Owned"]),
    female_avg_log = mean(avg_log_loan[gender == "Female Owned"]),
    
    # Calculate gaps
    loan_count_gap = male_loans - female_loans,
    loan_amount_gap = male_amount - female_amount,
    loan_count_ratio = male_loans / female_loans,
    loan_amount_ratio = male_amount / female_amount,
    
    # Gender gap percentage (positive = male-favored, negative = female-favored)
    avg_percent_diff = (exp(male_avg_log - female_avg_log) - 1) * 100,
    
    # Female representation
    female_percentage = (female_loans / (male_loans + female_loans)) * 100,
    female_amount_percentage = (female_amount / (male_amount + female_amount)) * 100,
    
    .groups = 'drop'
  )

# View the first few rows
head(gender_gaps1)

#prepare map data
map_data <- gender_gaps1 %>%
  left_join(state_mapping, by = c("BorrowerState" = "state_abbr"))

# Check for any states that didn't match
unmatched_states <- map_data %>% 
  filter(is.na(state_full)) %>% 
  distinct(BorrowerState)

if(nrow(unmatched_states) > 0) {
  print("Warning: These states didn't match map data:")
  print(unmatched_states)
}

# Merge with map polygon data for each year
map_data_2020 <- map_data %>% 
  filter(year == "2020") %>%
  select(state_full, avg_percent_diff, female_percentage, loan_count_ratio)

map_data_2021 <- map_data %>% 
  filter(year == "2021") %>%
  select(state_full, avg_percent_diff, female_percentage, loan_count_ratio)

# Join with map coordinates
choropleth_2020 <- us_states %>%
  left_join(map_data_2020, by = c("region" = "state_full"))

choropleth_2021 <- us_states %>%
  left_join(map_data_2021, by = c("region" = "state_full"))

install.packages("mapproj")
library(mapproj)

gap_map_2020 <- ggplot() +
  geom_polygon(data = choropleth_2020, 
               aes(x = long, y = lat, group = group, 
                   fill = avg_percent_diff), 
               color = "white", size = 0.2) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred", 
                       midpoint = 0, 
                       name = "Gender Gap (%)",
                       na.value = "grey90") +
  labs(title = "Gender Gap in PPP Loans (2020)",
       subtitle = "Positive % = Male-favored | Negative % = Female-favored",
       caption = "Gray states = No data") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
        legend.position = "bottom",
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.width = unit(1.5, "cm"),
        plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50"))

print(gap_map_2020)

# 3.2 Gender Gap Map - 2021
gap_map_2021 <- ggplot() +
  geom_polygon(data = choropleth_2021, 
               aes(x = long, y = lat, group = group, 
                   fill = avg_percent_diff), 
               color = "white", size = 0.2) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred", 
                       midpoint = 0, 
                       name = "Gender Gap (%)",
                       na.value = "grey90") +
  labs(title = "Gender Gap in PPP Loans (2021)",
       subtitle = "Positive % = Male-favored | Negative % = Female-favored",
       caption = "Gray states = No data") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
        legend.position = "bottom",
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.width = unit(1.5, "cm"),
        plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50"))

print(gap_map_2021)

# Female representation map - 2020
female_map_2020 <- ggplot() +
  geom_polygon(data = choropleth_2020, 
               aes(x = long, y = lat, group = group, 
                   fill = female_percentage), 
               color = "white", size = 0.2) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient(low = "lightpink", high = "darkred", 
                      name = "Female Representation (%)",
                      na.value = "grey90",
                      labels = scales::percent_format(scale = 1)) +
  labs(title = "Female Representation in PPP Loans (2020)",
       subtitle = "Percentage of loans to female-owned businesses",
       caption = "Gray states = No data | Territories excluded") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
        legend.position = "bottom",
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.width = unit(1.5, "cm"),
        plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50"))

print(female_map_2020)

# Female representation map - 2021
female_map_2021 <- ggplot() +
  geom_polygon(data = choropleth_2021, 
               aes(x = long, y = lat, group = group, 
                   fill = female_percentage), 
               color = "white", size = 0.2) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient(low = "lightpink", high = "darkred", 
                      name = "Female Representation (%)",
                      na.value = "grey90",
                      labels = scales::percent_format(scale = 1)) +
  labs(title = "Female Representation in PPP Loans (2021)",
       subtitle = "Percentage of loans to female-owned businesses",
       caption = "Gray states = No data | Territories excluded") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
        legend.position = "bottom",
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.width = unit(1.5, "cm"),
        plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50"))

print(female_map_2021)

# Calculate change in gender gap
gap_change <- gender_gaps1 %>%
  select(BorrowerState, year, avg_percent_diff) %>%
  pivot_wider(id_cols = BorrowerState, 
              names_from = year, 
              values_from = avg_percent_diff,
              names_prefix = "gap_") %>%
  mutate(
    gap_change = gap_2021 - gap_2020,
    change_category = case_when(
      gap_change < -5 ~ "Major Improvement (gap decreased >5%)",
      gap_change < -1 ~ "Moderate Improvement (1-5% decrease)",
      gap_change <= 1 ~ "Stable (within ±1%)",
      gap_change <= 5 ~ "Moderate Worsening (1-5% increase)",
      TRUE ~ "Major Worsening (gap increased >5%)"
    )
  ) %>%
  left_join(state_mapping, by = c("BorrowerState" = "state_abbr"))

# Merge with map
choropleth_change <- us_states %>%
  left_join(gap_change, by = c("region" = "state_full"))

# Change map
change_map <- ggplot() +
  geom_polygon(data = choropleth_change, 
               aes(x = long, y = lat, group = group, 
                   fill = change_category), 
               color = "white", size = 0.2) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_manual(
    values = c(
      "Major Improvement (gap decreased >5%)" = "darkgreen",
      "Moderate Improvement (1-5% decrease)" = "lightgreen",
      "Stable (within ±1%)" = "white",
      "Moderate Worsening (1-5% increase)" = "orange",
      "Major Worsening (gap increased >5%)" = "darkred"
    ),
    name = "Change in Gender Gap",
    na.value = "grey90"
  ) +
  labs(title = "Change in Gender Gap (2020 to 2021)",
       subtitle = "How the male-female loan gap evolved",
       caption = "Gray states = No data for one or both years | Territories excluded") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
        legend.position = "bottom",
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 9),
        plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50"))

print(change_map)


#Gender Gaps with Industry-Level Analysis
#ensure we have log-transformed amounts
gender_data_main <- gender_data_main %>%
  mutate(log_loan_amount = log(CurrentApprovalAmount + 1))

# Check what industry variables are available
print("Available industry-related columns:")
names(gender_data_main)[grepl("industry|naics|sector", names(gender_data_main), ignore.case = TRUE)]

#industry gaps
industry_gaps <- gender_data_main %>%
  group_by(BorrowerState, year, industry_name, gender) %>%
  summarise(
    total_loans = n(),
    total_amount = sum(CurrentApprovalAmount, na.rm = TRUE),
    avg_amount = mean(CurrentApprovalAmount, na.rm = TRUE),
    avg_log = mean(log_loan_amount, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Pivot to get male/female side by side
  pivot_wider(
    id_cols = c(BorrowerState, year, industry_name),
    names_from = gender,
    values_from = c(total_loans, total_amount, avg_amount, avg_log),
    names_sep = "_"
  )

# Check the column names after pivot_wider to see exact names
print("Column names after pivot_wider:")
print(names(industry_gaps))

# Now calculate gaps using correct column name references
industry_gaps <- industry_gaps %>%
  mutate(
    # Loan count gap - use backticks for column names with spaces
    male_loans = `total_loans_Male Owned`,
    female_loans = `total_loans_Female Owned`,
    
    # Amount metrics
    male_amount = `total_amount_Male Owned`,
    female_amount = `total_amount_Female Owned`,
    male_avg_log = `avg_log_Male Owned`,
    female_avg_log = `avg_log_Female Owned`,
    
    # Calculate gaps
    loan_count_gap = male_loans - female_loans,
    loan_count_ratio = male_loans / female_loans,
    
    # Amount gap (log-based for proportional difference)
    avg_log_gap = male_avg_log - female_avg_log,
    avg_percent_diff = (exp(avg_log_gap) - 1) * 100,
    
    # Female representation
    female_percentage = (female_loans / (male_loans + female_loans)) * 100,
    female_amount_percentage = (female_amount / (male_amount + female_amount)) * 100,
    
    # Total loans in industry
    total_industry_loans = male_loans + female_loans
  ) %>%
  filter(!is.na(industry_name))  # Remove rows with missing industry

# View summary
print("Gender gaps by state and industry:")
glimpse(industry_gaps)

# Get top industries by total loans
top_industries <- industry_gaps %>%
  group_by(industry_name) %>%
  summarise(total_loans = sum(total_industry_loans, na.rm = TRUE)) %>%
  arrange(desc(total_loans)) %>%
  head(8) %>%
  pull(industry_name)
view(top_industries)

# Create heatmap of gender gaps by state and industry (2020)
heatmap_2020 <- industry_gaps %>%
  filter(year == 2020, industry_name %in% top_industries) %>%
  ggplot(aes(x = industry_name, y = reorder(BorrowerState, avg_percent_diff), 
             fill = avg_percent_diff)) +
  geom_tile(color = "white", size = 0.2) +
  scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred", 
                       midpoint = 0, 
                       name = "Gender Gap (%)",
                       na.value = "grey90") +
  labs(title = "Gender Gap by State and Industry (2020)",
       subtitle = "Positive = Male-favored | Negative = Female-favored",
       x = "Industry Name", y = "State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))

print(heatmap_2020)

# Heatmap for 2021
heatmap_2021 <- industry_gaps %>%
  filter(year == 2021, industry_name %in% top_industries) %>%
  ggplot(aes(x = industry_name, y = reorder(BorrowerState, avg_percent_diff), 
             fill = avg_percent_diff)) +
  geom_tile(color = "white", size = 0.2) +
  scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred", 
                       midpoint = 0, 
                       name = "Gender Gap (%)",
                       na.value = "grey90") +
  labs(title = "Gender Gap by State and Industry (2021)",
       subtitle = "Positive = Male-favored | Negative = Female-favored",
       x = "Industry Sector", y = "State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))

print(heatmap_2021)

