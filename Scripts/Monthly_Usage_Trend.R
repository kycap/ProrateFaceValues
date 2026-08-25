# Setup ----

base::remove(list = ls())
library(tidyverse)
library(ggplot2)

# Load files ----

monthly_values <- read_csv("./referenceFiles/NAVR_2017_2024_DemandDataset_MonthlyValues.csv")

# Testing a mehod on a single application ----

appl_1 <- monthly_values |> filter(APPLICATION_NUMBER == "A009618")

months <- month.abb

appl_1_pivot <- appl_1 |>
  select(-contains("DIRECT_DIVERSION"), -contains("STORAGE_DIVERSION")) |>
  pivot_longer(contains("_TOTAL_DIVERSION")) 

# add column w/ months as factor to the long data frame

appl_1_pivot$name |> 
  str_to_title() |>
  str_extract("^.{3}")

ggplot(appl_1, aes(x = , fill = YEAR)) + 
  scale_color_manual(c("Jan" = "red", "Feb" = "#00FFAA"))

