library(dplyr)

# Step 1: Identify duplicate rows by B1
dup_rows <- HBCS_data_20251002 %>%
  filter(duplicated(B1) | duplicated(B1, fromLast = TRUE))

# Step 2: For each group of duplicates, keep the row with smallest A3a, then A3b, then A3c
smallest_rows <- dup_rows %>%
  group_by(B1) %>%
  arrange(A3a, A3b, A3c) %>%
  slice(1) %>%   # keep the first (smallest) row
  ungroup()

# Step 3: The remaining duplicates (excluding the smallest row per group)
other_rows <- dup_rows %>%
  anti_join(smallest_rows, by = names(dup_rows))

# For smallest_rows dataset
smallest_EHC_IHF <- smallest_rows %>%
  filter(substr(A1, 1, 3) %in% c("EHC", "IHF"))

smallest_HSS <- smallest_rows %>%
  filter(substr(A1, 1, 3) == "HSS")

# For other_rows dataset
other_EHC_IHF <- other_rows %>%
  filter(substr(A1, 1, 3) %in% c("EHC", "IHF"))

other_HSS <- other_rows %>%
  filter(substr(A1, 1, 3) == "HSS")

# Mean and standard deviation for each dataset

# Smallest duplicates: EHC/IHF
mean_smallest_EHC_IHF <- mean(smallest_EHC_IHF$illness_count, na.rm = TRUE)
sd_smallest_EHC_IHF   <- sd(smallest_EHC_IHF$illness_count, na.rm = TRUE)

# Smallest duplicates: HSS
mean_smallest_HSS <- mean(smallest_HSS$illness_count, na.rm = TRUE)
sd_smallest_HSS   <- sd(smallest_HSS$illness_count, na.rm = TRUE)

# Other duplicates: EHC/IHF
mean_other_EHC_IHF <- mean(other_EHC_IHF$illness_count, na.rm = TRUE)
sd_other_EHC_IHF   <- sd(other_EHC_IHF$illness_count, na.rm = TRUE)

# Other duplicates: HSS
mean_other_HSS <- mean(other_HSS$illness_count, na.rm = TRUE)
sd_other_HSS   <- sd(other_HSS$illness_count, na.rm = TRUE)

# Print results neatly
data.frame(
  Dataset = c("smallest_EHC_IHF", "smallest_HSS", "other_EHC_IHF", "other_HSS"),
  Mean    = c(mean_smallest_EHC_IHF, mean_smallest_HSS, mean_other_EHC_IHF, mean_other_HSS),
  SD      = c(sd_smallest_EHC_IHF, sd_smallest_HSS, sd_other_EHC_IHF, sd_other_HSS)
)

# Frequency tables for column E1w in each dataset

freq_smallest_EHC_IHF <- table(smallest_EHC_IHF$E1w)
freq_smallest_HSS     <- table(smallest_HSS$E1w)
freq_other_EHC_IHF    <- table(other_EHC_IHF$E1w)
freq_other_HSS        <- table(other_HSS$E1w)

# Print results
freq_smallest_EHC_IHF
freq_smallest_HSS
freq_other_EHC_IHF
freq_other_HSS


# Frequency tables for column Mentalissue in each dataset

freq_smallest_EHC_IHF <- table(smallest_EHC_IHF$Mentalissue)
freq_smallest_HSS     <- table(smallest_HSS$Mentalissue)
freq_other_EHC_IHF    <- table(other_EHC_IHF$Mentalissue)
freq_other_HSS        <- table(other_HSS$Mentalissue)

# Print results
freq_smallest_EHC_IHF
freq_smallest_HSS
freq_other_EHC_IHF
freq_other_HSS


# Frequency tables for column sleep_issues in each dataset

freq_smallest_EHC_IHF <- table(smallest_EHC_IHF$sleepissues)
freq_smallest_HSS     <- table(smallest_HSS$sleepissues)
freq_other_EHC_IHF    <- table(other_EHC_IHF$sleepissues)
freq_other_HSS        <- table(other_HSS$sleepissues)

# Print results
freq_smallest_EHC_IHF
freq_smallest_HSS
freq_other_EHC_IHF
freq_other_HSS

# Frequency tables for column G1a in each dataset

freq_smallest_EHC_IHF <- table(smallest_EHC_IHF$G1a)
freq_smallest_HSS     <- table(smallest_HSS$G1a)
freq_other_EHC_IHF    <- table(other_EHC_IHF$G1a)
freq_other_HSS        <- table(other_HSS$G1a)

# Print results
freq_smallest_EHC_IHF
freq_smallest_HSS
freq_other_EHC_IHF
freq_other_HSS


# Frequency tables for column cognitive in each dataset

freq_smallest_EHC_IHF <- table(smallest_EHC_IHF$cognitve)
freq_smallest_HSS     <- table(smallest_HSS$cognitve)
freq_other_EHC_IHF    <- table(other_EHC_IHF$cognitve)
freq_other_HSS        <- table(other_HSS$cognitve)

# Print results
freq_smallest_EHC_IHF
freq_smallest_HSS
freq_other_EHC_IHF
freq_other_HSS


# Mean and SD of L1g for each dataset, excluding 998 and 999

mean_smallest_EHC_IHF <- mean(smallest_EHC_IHF$L1g[!smallest_EHC_IHF$L1g %in% c(998, 999)], na.rm = TRUE)
sd_smallest_EHC_IHF   <- sd(smallest_EHC_IHF$L1g[!smallest_EHC_IHF$L1g %in% c(998, 999)], na.rm = TRUE)

mean_smallest_HSS <- mean(smallest_HSS$L1g[!smallest_HSS$L1g %in% c(998, 999)], na.rm = TRUE)
sd_smallest_HSS   <- sd(smallest_HSS$L1g[!smallest_HSS$L1g %in% c(998, 999)], na.rm = TRUE)

mean_other_EHC_IHF <- mean(other_EHC_IHF$L1g[!other_EHC_IHF$L1g %in% c(998, 999)], na.rm = TRUE)
sd_other_EHC_IHF   <- sd(other_EHC_IHF$L1g[!other_EHC_IHF$L1g %in% c(998, 999)], na.rm = TRUE)

mean_other_HSS <- mean(other_HSS$L1g[!other_HSS$L1g %in% c(998, 999)], na.rm = TRUE)
sd_other_HSS   <- sd(other_HSS$L1g[!other_HSS$L1g %in% c(998, 999)], na.rm = TRUE)

# Combine results into a summary table
summary_L1g <- data.frame(
  Dataset = c("smallest_EHC_IHF", "smallest_HSS", "other_EHC_IHF", "other_HSS"),
  Mean    = c(mean_smallest_EHC_IHF, mean_smallest_HSS, mean_other_EHC_IHF, mean_other_HSS),
  SD      = c(sd_smallest_EHC_IHF, sd_smallest_HSS, sd_other_EHC_IHF, sd_other_HSS)
)

# Print summary


# Mean and SD of M1e for each dataset, excluding 998 and 999

mean_smallest_EHC_IHF_M1e <- mean(smallest_EHC_IHF$M1e[!smallest_EHC_IHF$M1e %in% c(998, 999)], na.rm = TRUE)
sd_smallest_EHC_IHF_M1e   <- sd(smallest_EHC_IHF$M1e[!smallest_EHC_IHF$M1e %in% c(998, 999)], na.rm = TRUE)

mean_smallest_HSS_M1e <- mean(smallest_HSS$M1e[!smallest_HSS$M1e %in% c(998, 999)], na.rm = TRUE)
sd_smallest_HSS_M1e   <- sd(smallest_HSS$M1e[!smallest_HSS$M1e %in% c(998, 999)], na.rm = TRUE)

mean_other_EHC_IHF_M1e <- mean(other_EHC_IHF$M1e[!other_EHC_IHF$M1e %in% c(998, 999)], na.rm = TRUE)
sd_other_EHC_IHF_M1e   <- sd(other_EHC_IHF$M1e[!other_EHC_IHF$M1e %in% c(998, 999)], na.rm = TRUE)

mean_other_HSS_M1e <- mean(other_HSS$M1e[!other_HSS$M1e %in% c(998, 999)], na.rm = TRUE)
sd_other_HSS_M1e   <- sd(other_HSS$M1e[!other_HSS$M1e %in% c(998, 999)], na.rm = TRUE)

# Combine results into a summary table for M1e
summary_M1e <- data.frame(
  Dataset = c("smallest_EHC_IHF", "smallest_HSS", "other_EHC_IHF", "other_HSS"),
  Mean    = c(mean_smallest_EHC_IHF_M1e, mean_smallest_HSS_M1e, mean_other_EHC_IHF_M1e, mean_other_HSS_M1e),
  SD      = c(sd_smallest_EHC_IHF_M1e, sd_smallest_HSS_M1e, sd_other_EHC_IHF_M1e, sd_other_HSS_M1e)
)

# Print summary
print(summary_M1e)


# Mean and SD of N1i for each dataset, excluding 998 and 999

mean_smallest_EHC_IHF_N1i <- mean(smallest_EHC_IHF$N1i[!smallest_EHC_IHF$N1i %in% c(998, 999)], na.rm = TRUE)
sd_smallest_EHC_IHF_N1i   <- sd(smallest_EHC_IHF$N1i[!smallest_EHC_IHF$N1i %in% c(998, 999)], na.rm = TRUE)

mean_smallest_HSS_N1i <- mean(smallest_HSS$N1i[!smallest_HSS$N1i %in% c(998, 999)], na.rm = TRUE)
sd_smallest_HSS_N1i   <- sd(smallest_HSS$N1i[!smallest_HSS$N1i %in% c(998, 999)], na.rm = TRUE)

mean_other_EHC_IHF_N1i <- mean(other_EHC_IHF$N1i[!other_EHC_IHF$N1i %in% c(998, 999)], na.rm = TRUE)
sd_other_EHC_IHF_N1i   <- sd(other_EHC_IHF$N1i[!other_EHC_IHF$N1i %in% c(998, 999)], na.rm = TRUE)

mean_other_HSS_N1i <- mean(other_HSS$N1i[!other_HSS$N1i %in% c(998, 999)], na.rm = TRUE)
sd_other_HSS_N1i   <- sd(other_HSS$N1i[!other_HSS$N1i %in% c(998, 999)], na.rm = TRUE)

# Combine results into a summary table for N1i
summary_N1i <- data.frame(
  Dataset = c("smallest_EHC_IHF", "smallest_HSS", "other_EHC_IHF", "other_HSS"),
  Mean    = c(mean_smallest_EHC_IHF_N1i, mean_smallest_HSS_N1i, mean_other_EHC_IHF_N1i, mean_other_HSS_N1i),
  SD      = c(sd_smallest_EHC_IHF_N1i, sd_smallest_HSS_N1i, sd_other_EHC_IHF_N1i, sd_other_HSS_N1i)
)

# Print summary
print(summary_N1i)


# Mean and SD of N2k for each dataset, excluding 998 and 999

mean_smallest_EHC_IHF_N2k <- mean(smallest_EHC_IHF$N2k[!smallest_EHC_IHF$N2k %in% c(998, 999)], na.rm = TRUE)
sd_smallest_EHC_IHF_N2k   <- sd(smallest_EHC_IHF$N2k[!smallest_EHC_IHF$N2k %in% c(998, 999)], na.rm = TRUE)

mean_smallest_HSS_N2k <- mean(smallest_HSS$N2k[!smallest_HSS$N2k %in% c(998, 999)], na.rm = TRUE)
sd_smallest_HSS_N2k   <- sd(smallest_HSS$N2k[!smallest_HSS$N2k %in% c(998, 999)], na.rm = TRUE)

mean_other_EHC_IHF_N2k <- mean(other_EHC_IHF$N2k[!other_EHC_IHF$N2k %in% c(998, 999)], na.rm = TRUE)
sd_other_EHC_IHF_N2k   <- sd(other_EHC_IHF$N2k[!other_EHC_IHF$N2k %in% c(998, 999)], na.rm = TRUE)

mean_other_HSS_N2k <- mean(other_HSS$N2k[!other_HSS$N2k %in% c(998, 999)], na.rm = TRUE)
sd_other_HSS_N2k   <- sd(other_HSS$N2k[!other_HSS$N2k %in% c(998, 999)], na.rm = TRUE)

# Combine results into a summary table for N2k
summary_N2k <- data.frame(
  Dataset = c("smallest_EHC_IHF", "smallest_HSS", "other_EHC_IHF", "other_HSS"),
  Mean    = c(mean_smallest_EHC_IHF_N2k, mean_smallest_HSS_N2k, mean_other_EHC_IHF_N2k, mean_other_HSS_N2k),
  SD      = c(sd_smallest_EHC_IHF_N2k, sd_smallest_HSS_N2k, sd_other_EHC_IHF_N2k, sd_other_HSS_N2k)
)

# Print summary
print(summary_N2k)

# Mean and SD of E1af for each dataset, excluding 998 and 999

mean_smallest_EHC_IHF_E1af <- mean(smallest_EHC_IHF$E1af[!smallest_EHC_IHF$E1af %in% c(998, 999)], na.rm = TRUE)
sd_smallest_EHC_IHF_E1af   <- sd(smallest_EHC_IHF$E1af[!smallest_EHC_IHF$E1af %in% c(998, 999)], na.rm = TRUE)

mean_smallest_HSS_E1af <- mean(smallest_HSS$E1af[!smallest_HSS$E1af %in% c(998, 999)], na.rm = TRUE)
sd_smallest_HSS_E1af   <- sd(smallest_HSS$E1af[!smallest_HSS$E1af %in% c(998, 999)], na.rm = TRUE)

mean_other_EHC_IHF_E1af <- mean(other_EHC_IHF$E1af[!other_EHC_IHF$E1af %in% c(998, 999)], na.rm = TRUE)
sd_other_EHC_IHF_E1af   <- sd(other_EHC_IHF$E1af[!other_EHC_IHF$E1af %in% c(998, 999)], na.rm = TRUE)

mean_other_HSS_E1af <- mean(other_HSS$E1af[!other_HSS$E1af %in% c(998, 999)], na.rm = TRUE)
sd_other_HSS_E1af   <- sd(other_HSS$E1af[!other_HSS$E1af %in% c(998, 999)], na.rm = TRUE)

# Combine results into a summary table for E1af
summary_E1af <- data.frame(
  Dataset = c("smallest_EHC_IHF", "smallest_HSS", "other_EHC_IHF", "other_HSS"),
  Mean    = c(mean_smallest_EHC_IHF_E1af, mean_smallest_HSS_E1af, mean_other_EHC_IHF_E1af, mean_other_HSS_E1af),
  SD      = c(sd_smallest_EHC_IHF_E1af, sd_smallest_HSS_E1af, sd_other_EHC_IHF_E1af, sd_other_HSS_E1af)
)

# Print summary
print(summary_E1af)


# Frequency of IADL for each dataset, excluding 998 and 999

# Function to calculate frequency
calculate_frequency <- function(data) {
  table(data$IADL[!data$IADL %in% c(998, 999)], useNA = "ifany")
}

# Frequencies for each dataset
freq_smallest_EHC_IHF <- calculate_frequency(smallest_EHC_IHF)
freq_smallest_HSS <- calculate_frequency(smallest_HSS)
freq_other_EHC_IHF <- calculate_frequency(other_EHC_IHF)
freq_other_HSS <- calculate_frequency(other_HSS)

# Combine results into a list for easy viewing
frequency_summary <- list(
  smallest_EHC_IHF = freq_smallest_EHC_IHF,
  smallest_HSS = freq_smallest_HSS,
  other_EHC_IHF = freq_other_EHC_IHF,
  other_HSS = freq_other_HSS
)

# Print frequency summaries
print(frequency_summary)


# Frequency of ADL for each dataset, excluding 998 and 999

# Function to calculate frequency
calculate_frequency_ADL <- function(data) {
  table(data$ADL[!data$ADL %in% c(998, 999)], useNA = "ifany")
}

# Frequencies for each dataset
freq_smallest_EHC_IHF_ADL <- calculate_frequency_ADL(smallest_EHC_IHF)
freq_smallest_HSS_ADL <- calculate_frequency_ADL(smallest_HSS)
freq_other_EHC_IHF_ADL <- calculate_frequency_ADL(other_EHC_IHF)
freq_other_HSS_ADL <- calculate_frequency_ADL(other_HSS)

# Combine results into a list for easy viewing
frequency_summary_ADL <- list(
  smallest_EHC_IHF = freq_smallest_EHC_IHF_ADL,
  smallest_HSS = freq_smallest_HSS_ADL,
  other_EHC_IHF = freq_other_EHC_IHF_ADL,
  other_HSS = freq_other_HSS_ADL
)

# Print frequency summaries for ADL
print(frequency_summary_ADL)


# Frequency of cognitve for each dataset, excluding 998 and 999

# Function to calculate frequency
calculate_frequency_cognitve <- function(data) {
  table(data$cognitve[!data$cognitve %in% c(998, 999)], useNA = "ifany")
}

# Frequencies for each dataset
freq_smallest_EHC_IHF_cognitve <- calculate_frequency_cognitve(smallest_EHC_IHF)
freq_smallest_HSS_cognitve <- calculate_frequency_cognitve(smallest_HSS)
freq_other_EHC_IHF_cognitve <- calculate_frequency_cognitve(other_EHC_IHF)
freq_other_HSS_cognitve <- calculate_frequency_cognitve(other_HSS)

# Combine results into a list for easy viewing
frequency_summary_cognitve <- list(
  smallest_EHC_IHF = freq_smallest_EHC_IHF_cognitve,
  smallest_HSS = freq_smallest_HSS_cognitve,
  other_EHC_IHF = freq_other_EHC_IHF_cognitve,
  other_HSS = freq_other_HSS_cognitve
)

# Print frequency summaries for cognitve
print(frequency_summary_cognitve)


# Frequency of social_isolated for each dataset, excluding 998 and 999

# Function to calculate frequency
calculate_frequency_social_isolated <- function(data) {
  table(data$social_isolated[!data$social_isolated %in% c(998, 999)], useNA = "ifany")
}

# Frequencies for each dataset
freq_smallest_EHC_IHF_social_isolated <- calculate_frequency_social_isolated(smallest_EHC_IHF)
freq_smallest_HSS_social_isolated <- calculate_frequency_social_isolated(smallest_HSS)
freq_other_EHC_IHF_social_isolated <- calculate_frequency_social_isolated(other_EHC_IHF)
freq_other_HSS_social_isolated <- calculate_frequency_social_isolated(other_HSS)

# Combine results into a list for easy viewing
frequency_summary_social_isolated <- list(
  smallest_EHC_IHF = freq_smallest_EHC_IHF_social_isolated,
  smallest_HSS = freq_smallest_HSS_social_isolated,
  other_EHC_IHF = freq_other_EHC_IHF_social_isolated,
  other_HSS = freq_other_HSS_social_isolated
)

# Print frequency summaries for social_isolated
print(frequency_summary_social_isolated)


# Frequency of fall for each dataset, excluding 998 and 999

# Function to calculate frequency
calculate_frequency_fall <- function(data) {
  table(data$fall[!data$fall %in% c(998, 999)], useNA = "ifany")
}

# Frequencies for each dataset
freq_smallest_EHC_IHF_fall <- calculate_frequency_fall(smallest_EHC_IHF)
freq_smallest_HSS_fall <- calculate_frequency_fall(smallest_HSS)
freq_other_EHC_IHF_fall <- calculate_frequency_fall(other_EHC_IHF)
freq_other_HSS_fall <- calculate_frequency_fall(other_HSS)

# Combine results into a list for easy viewing
frequency_summary_fall <- list(
  smallest_EHC_IHF = freq_smallest_EHC_IHF_fall,
  smallest_HSS = freq_smallest_HSS_fall,
  other_EHC_IHF = freq_other_EHC_IHF_fall,
  other_HSS = freq_other_HSS_fall
)

# Print frequency summaries for fall
print(frequency_summary_fall)