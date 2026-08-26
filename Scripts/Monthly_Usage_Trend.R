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

appl_1_pivot$name |> # change $name to $MONTH_TOTAL
  str_to_title() |>
  str_extract("^.{3}")

appl_1_pivot |>
  mutate(MONTH = factor(MONTH_TOTAL, levels = months)) # the MONTH column fills w/ NA b/c characters in MONTH_TOTAL do not match months level
# if a value doesn't match the level, R makes it NA
# fix by doing the following

appl_1_pivot |>
  mutate(
    MONTH_STR = str_extract(MONTH_TOTAL, paste(months, collapse = "|")),
    MONTH = factor(MONTH_STR, levels = months)
  ) # doesn't save MONTH_STR or MONTH (i.e., when I view the df, those columns aren't there but they show in the console). need to save to new variable

ggplot(appl_1_pivot, aes(x = YEAR, y = value, fill = YEAR)) + 
  geom_col()
  #scale_color_manual(c("Jan" = "red", "Feb" = "#00FFAA"))

# try again: mutate and ggplot
appl_1_pivot_mutate <- appl_1_pivot |>
  mutate(
    MONTH_STR = str_extract(MONTH_TOTAL, paste(months, collapse = "|")),
    MONTH = factor(MONTH_STR, levels = months),
    YEAR = as.factor(YEAR)
  )
# must do year as factor, otherwise the values for the years stack on top of each other for their corresponding month
ggplot(appl_1_pivot_mutate, aes(x = MONTH, y = value, fill = YEAR)) +
  geom_col(position = "dodge") +
  labs(x = "",
       y = "Water Volume (AF)",
       fill = "Year") +
  theme_minimal()
