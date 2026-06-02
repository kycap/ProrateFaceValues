### ----
# Protating face values in the Navarro River watershed

### ----

#install.packages("tidyverse")
library(tidyverse)

base::remove(list = ls())

### ----

ewrims_ff <- read_csv("./referenceFiles/ewrims_flat_file_pod.csv", guess_max = 10^6)
"POD_STATUS" %in% names(ewrims_ff)

mdt <- read_csv("./referenceFiles/NV_2017_2024_MDT_2026-03-11.csv") |>
  select(-APPLICATION_NUMBER) |> rename(APPLICATION_NUMBER = ORIGINAL_APPLICATION_NUMBER)

### ----
# Join fields from the two tables

NV_table <- inner_join(ewrims_ff, mdt, by = "APPLICATION_NUMBER")   # returns more rows than exist in MDT
"POD_STATUS" %in% names(NV_table)

# filter to those that have values in any of specified fields
NV_table_filter <-  NV_table %>% filter(!is.na("STORAGE_SEASON_START") | !is.na("STORAGE_SEASON_END") |
                      !is.na("DIRECT_DIV_SEASON_START") | !is.na("DIRECT_DIV_SEASON_END"))    # no change in no.of rows

# filter to only those w/ active POD_STATUS
NV_table_filter <- filter(NV_table_filter, POD_STATUS == "Active")

### ----
NV_table_filter <- NV_table_filter %>% add_column(PRORATE_VAL = 0)

NV_table_filter <- NV_table_filter %>%
  mutate(PRORATE_VAL = FACE_VALUE_AMOUNT_AF / 12) #prorated value (acre-ft) per month

### ----
# Calculate prorated value for rights with specified direct diversion season

# Create new df/tibble with rights that have specified direct diversion season
NV_table_filter_DirDiv <- NV_table_filter %>%
  filter(!is.na(DIRECT_DIV_SEASON_START))

# Create new column (filled w/ zeros) for number of diversion days
NV_table_filter_DirDiv <- NV_table_filter_DirDiv %>% add_column(DIV_DAYS = 0)

# Calculate the number of diversion days based on the DIRECT_DIV_SEASON_START, DIRECT_DIV_SEASON_END fields
NV_table_filter_DirDiv <- NV_table_filter_DirDiv %>%
  mutate(DIV_DAYS = interval(DIRECT_DIV_SEASON_START, DIRECT_DIV_SEASON_END))

# Calculate prorated value for the diversion season (FACE_VALUE_AMOUNT_AF / DIV_DAYS)
