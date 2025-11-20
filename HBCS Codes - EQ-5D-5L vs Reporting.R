# Load required packages
library(dplyr)
library(ggplot2)

# Ensure your dataset is loaded
# Assuming your dataset is named HBCS_data_20251002

# Create a cleaned dataset, excluding NA and 999 values
cleaned_data <- HBCS_data_20251002 %>%
  filter(!is.na(E1aa) & E1aa != 999 & !is.na(fall))  # Exclude rows with NA in E1aa or fall, and exclude 999 from E1aa

# Generate frequency table for E1aa (mobility score) vs fall
table_mobility_fall <- table(cleaned_data$E1aa, cleaned_data$fall)
print(table_mobility_fall)

# Chi-squared test for E1aa vs fall
chisq_mobility_fall <- chisq.test(table_mobility_fall)
print(chisq_mobility_fall)

# Visualization of the relationship
ggplot(data = cleaned_data, aes(x = factor(E1aa), fill = factor(fall))) +
  geom_bar(position = "dodge") +
  labs(title = "Comparison of Mobility Score (E1aa) and Falls",
       x = "Mobility Score (E1aa)",
       y = "Count") +
  theme_minimal()


# Load required packages
library(dplyr)
library(ggplot2)

# Create a cleaned dataset, excluding NA and 999 values for E1ad
cleaned_data <- HBCS_data_20251002 %>%
  filter(!is.na(E1ad) & E1ad != 999 & !is.na(pain))  # Exclude rows with NA in E1ad or pain, and exclude 999 from E1ad

# Generate frequency table for E1ad (pain score) vs pain variable
table_pain_comparison <- table(cleaned_data$E1ad, cleaned_data$pain)
print(table_pain_comparison)

# Chi-squared test for E1ad vs pain
chisq_pain_comparison <- chisq.test(table_pain_comparison)
print(chisq_pain_comparison)

# Visualization of the relationship
ggplot(data = cleaned_data, aes(x = factor(E1ad), fill = factor(pain))) +
  geom_bar(position = "dodge") +
  labs(title = "Comparison of EQ-5D-5L Pain Score (E1ad) and Reported Pain",
       x = "EQ-5D-5L Pain Score (E1ad)",
       y = "Count") +
  theme_minimal()

# Load required packages
library(dplyr)
library(ggplot2)

# Create a cleaned dataset, excluding NA and 999 values for E1ad
cleaned_data <- HBCS_data_20251002 %>%
  filter(!is.na(E1ad) & E1ad != 999 & !is.na(Mentalissue))  # Exclude rows with NA in E1ad or Mentalissue, and exclude 999 from E1ad

# Generate frequency table for E1ad (pain score) vs Mentalissue
table_pain_mentalissue <- table(cleaned_data$E1ad, cleaned_data$Mentalissue)
print(table_pain_mentalissue)

# Chi-squared test for E1ad vs Mentalissue
chisq_pain_mentalissue <- chisq.test(table_pain_mentalissue)
print(chisq_pain_mentalissue)

# Visualization of the relationship
ggplot(data = cleaned_data, aes(x = factor(E1ad), fill = factor(Mentalissue))) +
  geom_bar(position = "dodge") +
  labs(title = "Comparison of EQ-5D-5L Pain Score (E1ad) and Mental Health Issues",
       x = "EQ-5D-5L Pain Score (E1ad)",
       y = "Count") +
  theme_minimal()


# Load required packages
library(dplyr)
library(ggplot2)

# Create a cleaned dataset, excluding NA values for E1ac and cognitive
cleaned_data <- HBCS_data_20251002 %>%
  filter(!is.na(E1ac) & !is.na(cognitve))  # Exclude rows with NA in E1ac or cognitive

# Generate frequency table for E1ac (usual activities score) vs cognitive
table_usual_activities_cognitive <- table(cleaned_data$E1ac, cleaned_data$cognitve)
print(table_usual_activities_cognitive)

# Visualization: Bar plot to show the frequency distribution
ggplot(data = cleaned_data, aes(x = factor(E1ac), fill = factor(cognitve))) +
  geom_bar(position = "dodge") +
  labs(title = "Comparison of EQ-5D-5L Usual Activities Score (E1ac) and Cognitive Function",
       x = "EQ-5D-5L Usual Activities Score (E1ac)",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("Normal Cognitive Function", "Cognitive Impairment"),
                    name = "Cognitive Status")


# Load required packages
library(dplyr)
library(ggplot2)

# Create a cleaned dataset, excluding NA and 999 values for E1ac
cleaned_data <- HBCS_data_20251002 %>%
  filter(!is.na(E1ac) & E1ac != 999 & !is.na(`functional issues(1=impaired, 0=normal)`))  # Use backticks for the column name

# Generate frequency table for E1ac (usual activities score) vs functional issues
table_usual_activities <- table(cleaned_data$E1ac, cleaned_data$`functional issues(1=imparied, 0=normal)`)
print(table_usual_activities)

# Chi-squared test for E1ac vs functional issues
chisq_usual_activities <- chisq.test(table_usual_activities)
print(chisq_usual_activities)

# Visualization of the relationship
ggplot(data = cleaned_data, aes(x = factor(E1ac), fill = factor(`functional issues(1=imparied, 0=normal)`))) +
  geom_bar(position = "dodge") +
  labs(title = "Comparison of EQ-5D-5L Usual Activities Score (E1ac) and Functional Issues",
       x = "EQ-5D-5L Usual Activities Score (E1ac)",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("Normal Function", "Impaired Function"),
                    name = "Functional Status")

# Load required packages
library(dplyr)
library(ggplot2)

# Create a cleaned dataset, excluding NA and 999 values for E1ad
cleaned_data <- HBCS_data_20251002 %>%
  filter(!is.na(E1ad) & E1ad != 999 & !is.na(illness_count))  # Exclude rows with NA in E1ad or pain, and exclude 999 from E1ad

# Generate frequency table for E1ad (pain score) vs pain variable
table_pain_comparison <- table(cleaned_data$E1ad, cleaned_data$illness_count)
print(table_pain_comparison)

# Chi-squared test for E1ad vs pain
chisq_pain_comparison <- chisq.test(table_pain_comparison)
print(chisq_pain_comparison)

# Visualization of the relationship
ggplot(data = cleaned_data, aes(x = factor(E1ad), fill = factor(illness_count))) +
  geom_bar(position = "dodge") +
  labs(title = "Comparison of EQ-5D-5L Pain Score (E1ad) and Reported Pain",
       x = "EQ-5D-5L Pain Score (E1ad)",
       y = "Count") +
  theme_minimal()