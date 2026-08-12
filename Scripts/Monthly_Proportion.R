# Monthly/Seasonal Analysis of Water Usage

# Setup ----

base::remove(list = ls())
library(tidyverse)
library(ggplot2)

# Load files ----

ewrims_ff <- read_csv("./referenceFiles/ewrims_flat_file_pod.csv", guess_max = 10^6)
"POD_STATUS" %in% names(ewrims_ff)

mdt <- read_csv("./referenceFiles/NV_2017_2024_MDT_2026-03-11.csv")


# Recombine split water rights
mdt <- mdt |>
  group_by(ORIGINAL_APPLICATION_NUMBER) |> # some water rights are split (multiple PODs for a right), so group_by combines them
  summarize(across(contains("_DIV"), ~ sum(., na.rm = TRUE))) 
# across() applies transformation to multiple columns, in this case, those specified w/in contains()any column header that contains "_DIV"; 
# ~ separates left and right side of model formula (left side is optional)
# left side is the columns, right side is the sum, "." is a placeholder representing the input (i.e., the columns w/ "_DIV"), then calc. sum and ignore missing values
# summarize returns one row for grouped variables. it returns one row for each split water right, in which the value in each column that had "_DIV" was summed

mdt <- mdt |>
  rename(APPLICATION_NUMBER = ORIGINAL_APPLICATION_NUMBER)

# Proportional amount of the expected annual diversion in each month
mdt <- mdt |> add_column(
  JAN_PROP = 0, FEB_PROP = 0, MAR_PROP = 0, APR_PROP = 0,
  MAY_PROP = 0, JUN_PROP = 0, JUL_PROP = 0, AUG_PROP = 0,
  SEP_PROP = 0, OCT_PROP = 0, NOV_PROP = 0, DEC_PROP = 0
)

for (i in 1:nrow(mdt)) {
  mdt$JAN_PROP[i] = mdt$JAN_MEAN_DIV[i] / mdt$TOTAL_EXPECTED_ANNUAL_DIVERSION[i]
}

for (i in 1:nrow(mdt)) {
  mdt$FEB_PROP[i] = mdt$FEB_MEAN_DIV[i] / mdt$TOTAL_EXPECTED_ANNUAL_DIVERSION[i]
}

for (i in 1:nrow(mdt)) {
  mdt$MAR_PROP[i] = mdt$MAR_MEAN_DIV[i] / mdt$TOTAL_EXPECTED_ANNUAL_DIVERSION[i]
}

for (i in 1:nrow(mdt)) {
  mdt$APR_PROP[i] = mdt$APR_MEAN_DIV[i] / mdt$TOTAL_EXPECTED_ANNUAL_DIVERSION[i]
}

for (i in 1:nrow(mdt)) {
  mdt$MAY_PROP[i] = mdt$MAY_MEAN_DIV[i] / mdt$TOTAL_EXPECTED_ANNUAL_DIVERSION[i]
}

for (i in 1:nrow(mdt)) {
  mdt$JUN_PROP[i] = mdt$JUN_MEAN_DIV[i] / mdt$TOTAL_EXPECTED_ANNUAL_DIVERSION[i]
}

for (i in 1:nrow(mdt)) {
  mdt$JUL_PROP[i] = mdt$JUL_MEAN_DIV[i] / mdt$TOTAL_EXPECTED_ANNUAL_DIVERSION[i]
}

for (i in 1:nrow(mdt)) {
  mdt$AUG_PROP[i] = mdt$AUG_MEAN_DIV[i] / mdt$TOTAL_EXPECTED_ANNUAL_DIVERSION[i]
}

for (i in 1:nrow(mdt)) {
  mdt$SEP_PROP[i] = mdt$SEP_MEAN_DIV[i] / mdt$TOTAL_EXPECTED_ANNUAL_DIVERSION[i]
}

for (i in 1:nrow(mdt)) {
  mdt$OCT_PROP[i] = mdt$OCT_MEAN_DIV[i] / mdt$TOTAL_EXPECTED_ANNUAL_DIVERSION[i]
}

for (i in 1:nrow(mdt)) {
  mdt$NOV_PROP[i] = mdt$NOV_MEAN_DIV[i] / mdt$TOTAL_EXPECTED_ANNUAL_DIVERSION[i]
}

for (i in 1:nrow(mdt)) {
  mdt$DEC_PROP[i] = mdt$DEC_MEAN_DIV[i] / mdt$TOTAL_EXPECTED_ANNUAL_DIVERSION[i]
}

# Plot ----

# need to pivot longer
mdt_long <- mdt |>
  select(APPLICATION_NUMBER, contains("_PROP")) |> #POD_ID doesn't exist in mdt, would come from joining w/ flat file
  pivot_longer(contains("_PROP"))

propDF <- mdt |>
  select(APPLICATION_NUMBER, contains("_PROP")) |>
  pivot_longer(contains("_PROP"), names_to = "MONTH_PROPORTION", values_to = "PROPORTION")

propDF |>
  mutate(MONTH = MONTH_PROPORTION |> map_dbl(~ which(toupper(str_remove(., "_.+$")) == toupper(month.abb)) |>
                                               arrange(MONTH)))

#MONTH = factor(toupper(month.abb))

#propDF <- propDF |>
  mutate(MONTH = 238 * factor(toupper(month.abb)))

ggplot(propDF, aes(x = MONTH_PROPORTION, y = PROPORTION)) +
  geom_boxplot() +
  labs(x = "Month", y = "Monthly Proportion") # despite NAs, there is no error when plotting so it must ignore NA (Warning message: Removed 708 rows containing non-finite outside the scale range ('stat_boxplot()'))
# in Console: propDF |> count(PROPORTION == 0); among the result is 708 NA

# Another mdt ---

# Jacob shared mdt with a few additional columns, one already calculating the percentage per month

# mdt_jacob <- read_csv("./referenceFiles/NV_MDT_FV_raw.csv")
# mdt_jacob <- mdt_jacob |> # follow approach from NV_prorate.R to recombine split water rights 
#   group_by(APPLICATION_NUMBER) |>
#   summarize(across(contains("MEAN_DIV"), ~ sum(., na.rm = TRUE))) # reduces columns but not rows


# Join and filter ---- 
# Maybe this section is unnecessary?
# Join fields from the two tables

NV_table <- inner_join(ewrims_ff, mdt, by = "APPLICATION_NUMBER")   # returns more rows than exist in MDT
"POD_STATUS" %in% names(NV_table)

# filter to those that have values in any of specified fields
NV_table_filter <-  NV_table %>% filter(!is.na("STORAGE_SEASON_START") | !is.na("STORAGE_SEASON_END") |
                                          !is.na("DIRECT_DIV_SEASON_START") | !is.na("DIRECT_DIV_SEASON_END"))    # no change in no.of rows

# filter to only those w/ active POD_STATUS
NV_table_filter <- filter(NV_table_filter, POD_STATUS == "Active")

