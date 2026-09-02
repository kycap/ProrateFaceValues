# Setup ----

base::remove(list = ls())
library(tidyverse)
library(ggplot2)

# Load files ----

monthly_values <- read_csv("./referenceFiles/NAVR_2017_2024_DemandDataset_MonthlyValues.csv")

# Testing a method on a single application ----

appl_1 <- monthly_values |> filter(APPLICATION_NUMBER == "A009618")

months <- toupper(month.abb)

appl_1_pivot <- appl_1 |>
  select(-contains("DIRECT_DIVERSION"), -contains("STORAGE_DIVERSION")) |>
  pivot_longer(contains("_TOTAL_DIVERSION"), names_to = "MONTH_TOTAL", values_to = "WATER_VOLUME")
# pivot the original df. The column w/ <MONTH>_TOTAL_DIVERSION is renamed "name" by default; I set the column name to "MONTH_TOTAL".
# The column w/ the monthly values is renamed "value" by default; I set the column name to "WATER_VOLUME".

# add column w/ months as factor to the long data frame

appl_1_pivot$MONTH_TOTAL |> 
  str_to_title() |>
  str_extract("^.{3}")

#appl_1_pivot |>
#  mutate(MONTH = factor(MONTH_TOTAL, levels = months)) # the MONTH column fills w/ NA b/c characters in MONTH_TOTAL do not match months level
# if a value doesn't match the level, R makes it NA
# fix by doing the following mutate and ggplot

appl_1_pivot_mutate <- appl_1_pivot |>
  mutate(
    MONTH_STR = str_extract(MONTH_TOTAL, paste(months, collapse = "|")),
    MONTH = factor(MONTH_STR, levels = months),
    YEAR = as.factor(YEAR)
  )
# must do year as factor, otherwise the values for the years stack on top of each other for their corresponding month
ggplot(appl_1_pivot_mutate, aes(x = MONTH, y = WATER_VOLUME, fill = YEAR)) +
  geom_col(position = "dodge") +
  labs(x = "",
       y = "Water Volume (AF)",
       fill = "Year") +
  theme_minimal()



# Scale up for entire watershed ----

# Sum the monthly value in a year across all rights (i.e., JAN_2017_TOTAL, FEB_2017_TOTAL, ..., DEC_2024_TOTAL)
# monthly_values has column of totals (direct diversion + storage) for a month. Need to sum for all rights
# in the same year, repeat for all years.

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2017) {
  JAN_2017 <- sum(monthly_values$JAN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2017) {
    FEB_2017 <- sum(monthly_values$FEB_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2017) {
    MAR_2017 <- sum(monthly_values$MAR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2017) {
    APR_2017 <- sum(monthly_values$APR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2017) {
    MAY_2017 <- sum(monthly_values$MAY_TOTAL_DIVERSION[i])
  }
}


for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2017) {
    JUN_2017 <- sum(monthly_values$JUN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2017) {
    JUL_2017 <- sum(monthly_values$JUL_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2017) {
    AUG_2017 <- sum(monthly_values$AUG_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2017) {
    SEP_2017 <- sum(monthly_values$SEP_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2017) {
    OCT_2017 <- sum(monthly_values$OCT_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2017) {
    NOV_2017 <- sum(monthly_values$NOV_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2017) {
    DEC_2017 <- sum(monthly_values$DEC_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2018) {
    JAN_2018 <- sum(monthly_values$JAN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2018) {
    FEB_2018 <- sum(monthly_values$FEB_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2018) {
    MAR_2018 <- sum(monthly_values$MAR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2018) {
    APR_2018 <- sum(monthly_values$APR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2018) {
    MAY_2018 <- sum(monthly_values$MAY_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2018) {
    JUN_2018 <- sum(monthly_values$JUN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2018) {
    JUL_2018 <- sum(monthly_values$JUL_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2018) {
    AUG_2018 <- sum(monthly_values$AUG_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2018) {
    SEP_2018 <- sum(monthly_values$SEP_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2018) {
    OCT_2018 <- sum(monthly_values$OCT_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2018) {
    NOV_2018 <- sum(monthly_values$NOV_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2018) {
    DEC_2018 <- sum(monthly_values$DEC_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2019) {
    JAN_2019 <- sum(monthly_values$JAN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2019) {
    FEB_2019 <- sum(monthly_values$FEB_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2019) {
    MAR_2019 <- sum(monthly_values$MAR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2019) {
    APR_2019 <- sum(monthly_values$APR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2019) {
    MAY_2019 <- sum(monthly_values$MAY_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2019) {
    JUN_2019 <- sum(monthly_values$JUN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2019) {
    JUL_2019 <- sum(monthly_values$JUL_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2019) {
    AUG_2019 <- sum(monthly_values$AUG_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2019) {
    SEP_2019 <- sum(monthly_values$SEP_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2019) {
    OCT_2019 <- sum(monthly_values$OCT_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2019) {
    NOV_2019 <- sum(monthly_values$NOV_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2019) {
    DEC_2019 <- sum(monthly_values$DEC_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2020) {
    JAN_2020 <- sum(monthly_values$JAN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2020) {
    FEB_2020 <- sum(monthly_values$FEB_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2020) {
    MAR_2020 <- sum(monthly_values$MAR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2020) {
    APR_2020 <- sum(monthly_values$APR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2020) {
    MAY_2020 <- sum(monthly_values$MAY_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2020) {
    JUN_2020 <- sum(monthly_values$JUN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2020) {
    JUL_2020 <- sum(monthly_values$JUL_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2020) {
    AUG_2020 <- sum(monthly_values$AUG_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2020) {
    SEP_2020 <- sum(monthly_values$SEP_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2020) {
    OCT_2020 <- sum(monthly_values$OCT_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2020) {
    NOV_2020 <- sum(monthly_values$NOV_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2020) {
    DEC_2020 <- sum(monthly_values$DEC_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2021) {
    JAN_2021 <- sum(monthly_values$JAN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2021) {
    FEB_2021 <- sum(monthly_values$FEB_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2021) {
    MAR_2021 <- sum(monthly_values$MAR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2021) {
    APR_2021 <- sum(monthly_values$APR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2021) {
    MAY_2021 <- sum(monthly_values$MAY_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2021) {
    JUN_2021 <- sum(monthly_values$JUN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2021) {
    JUL_2021 <- sum(monthly_values$JUL_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2021) {
    AUG_2021 <- sum(monthly_values$AUG_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2021) {
    SEP_2021 <- sum(monthly_values$SEP_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2021) {
    OCT_2021 <- sum(monthly_values$OCT_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2021) {
    NOV_2021 <- sum(monthly_values$NOV_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2021) {
    DEC_2021 <- sum(monthly_values$DEC_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2022) {
    JAN_2022 <- sum(monthly_values$JAN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2022) {
    FEB_2022 <- sum(monthly_values$FEB_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2022) {
    MAR_2022 <- sum(monthly_values$MAR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2022) {
    APR_2022 <- sum(monthly_values$APR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2022) {
    MAY_2022 <- sum(monthly_values$MAY_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2022) {
    JUN_2022 <- sum(monthly_values$JUN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2022) {
    JUL_2022 <- sum(monthly_values$JUL_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2022) {
    AUG_2022 <- sum(monthly_values$AUG_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2022) {
    SEP_2022 <- sum(monthly_values$SEP_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2022) {
    OCT_2022 <- sum(monthly_values$OCT_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2022) {
    NOV_2022 <- sum(monthly_values$NOV_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2022) {
    DEC_2022 <- sum(monthly_values$DEC_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2023) {
    JAN_2023 <- sum(monthly_values$JAN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2023) {
    FEB_2023 <- sum(monthly_values$FEB_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2023) {
    MAR_2023 <- sum(monthly_values$MAR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2023) {
    APR_2023 <- sum(monthly_values$APR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2023) {
    MAY_2023 <- sum(monthly_values$MAY_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2023) {
    JUN_2023 <- sum(monthly_values$JUN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2023) {
    JUL_2023 <- sum(monthly_values$JUL_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2023) {
    AUG_2023 <- sum(monthly_values$AUG_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2023) {
    SEP_2023 <- sum(monthly_values$SEP_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2023) {
    OCT_2023 <- sum(monthly_values$OCT_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2023) {
    NOV_2023 <- sum(monthly_values$NOV_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2023) {
    DEC_2023 <- sum(monthly_values$DEC_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2024) {
    JAN_2024 <- sum(monthly_values$JAN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2024) {
    FEB_2024 <- sum(monthly_values$FEB_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2024) {
    MAR_2024 <- sum(monthly_values$MAR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2024) {
    APR_2024 <- sum(monthly_values$APR_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2024) {
    MAY_2024 <- sum(monthly_values$MAY_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2024) {
    JUN_2024 <- sum(monthly_values$JUN_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2024) {
    JUL_2024 <- sum(monthly_values$JUL_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2024) {
    AUG_2024 <- sum(monthly_values$AUG_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2024) {
    SEP_2024 <- sum(monthly_values$SEP_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2024) {
    OCT_2024 <- sum(monthly_values$OCT_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2024) {
    NOV_2024 <- sum(monthly_values$NOV_TOTAL_DIVERSION[i])
  }
}

for (i in 1:nrow(monthly_values)) {
  if (monthly_values$YEAR[i] == 2024) {
    DEC_2024 <- sum(monthly_values$DEC_TOTAL_DIVERSION[i])
  }
}

# Build tibble with summary values ----

NAVR_YEARS <- 2017:2024

jan_vals <- c(JAN_2017, JAN_2018, JAN_2019, JAN_2020, JAN_2021, JAN_2022, JAN_2023, JAN_2024)
feb_vals <- c(FEB_2017, FEB_2018, FEB_2019, FEB_2020, FEB_2021, FEB_2022, FEB_2023, FEB_2024)
mar_vals <- c(MAR_2017, MAR_2018, MAR_2019, MAR_2020, MAR_2021, MAR_2022, MAR_2023, MAR_2024)
apr_vals <- c(APR_2017, APR_2018, APR_2019, APR_2020, APR_2021, APR_2022, APR_2023, APR_2024)
may_vals <- c(MAY_2017, MAY_2018, MAY_2019, MAY_2020, MAY_2021, MAY_2022, MAY_2023, MAY_2024)
jun_vals <- c(JUN_2017, JUN_2018, JUN_2019, JUN_2020, JUN_2021, JUN_2022, JUN_2023, JUN_2024)
jul_vals <- c(JUL_2017, JUL_2018, JUL_2019, JUL_2020, JUL_2021, JUL_2022, JUL_2023, JUL_2024)
aug_vals <- c(AUG_2017, AUG_2018, AUG_2019, AUG_2020, AUG_2021, AUG_2022, AUG_2023, AUG_2024)
sep_vals <- c(SEP_2017, SEP_2018, SEP_2019, SEP_2020, SEP_2021, SEP_2022, SEP_2023, SEP_2024)
oct_vals <- c(OCT_2017, OCT_2018, OCT_2019, OCT_2020, OCT_2021, OCT_2022, OCT_2023, OCT_2024)
nov_vals <- c(NOV_2017, NOV_2018, NOV_2019, NOV_2020, NOV_2021, NOV_2022, NOV_2023, NOV_2024)
dec_vals <- c(DEC_2017, DEC_2018, DEC_2019, DEC_2020, DEC_2021, DEC_2022, DEC_2023, DEC_2024)

navr_tibble <- tibble(NAVR_YEARS,
                      jan_vals, feb_vals, mar_vals,
                      apr_vals, may_vals, jun_vals,
                      jul_vals, aug_vals, sep_vals,
                      oct_vals, nov_vals, dec_vals)


navr_tibble_pivot <- navr_tibble |>
  pivot_longer(
    contains("_vals"), 
    names_to = "Month", 
    values_to = "Volume",
    names_transform = list(Month = ~ sub("_vals", "", .))
  ) |>
  mutate(Month = factor(Month, levels = c(
    "jan", "feb", "mar", "apr", "may", "jun",
    "jul", "aug", "sep", "oct", "nov", "dec"
  )),
  NAVR_YEARS = as.factor(NAVR_YEARS)
  )

ggplot(navr_tibble_pivot, aes(x = Month, y = Volume, fill = NAVR_YEARS)) +
  geom_col(position = "dodge") +
  labs(x = "",
       y = "Water Volume (AF)",
       fill = "Year") +
  theme_minimal()
