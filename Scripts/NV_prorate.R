### ----
# Prorating face values in the Navarro River watershed

### Setup ----

#install.packages("tidyverse")
library(tidyverse)

base::remove(list = ls())

### Load Files ----

ewrims_ff <- read_csv("./referenceFiles/ewrims_flat_file_pod.csv", guess_max = 10^6)
"POD_STATUS" %in% names(ewrims_ff)

mdt <- read_csv("./referenceFiles/NV_2017_2024_MDT_2026-03-11.csv")


# Recombine split water rights
mdt <- mdt |>
  group_by(ORIGINAL_APPLICATION_NUMBER) |>
  summarize(across(contains("MEAN_DIV"), ~ sum(., na.rm = TRUE)))


mdt <- mdt |>
  rename(APPLICATION_NUMBER = ORIGINAL_APPLICATION_NUMBER)
  

### Join and filter ----
# Join fields from the two tables

NV_table <- inner_join(ewrims_ff, mdt, by = "APPLICATION_NUMBER")   # returns more rows than exist in MDT
"POD_STATUS" %in% names(NV_table)

# filter to those that have values in any of specified fields
NV_table_filter <-  NV_table %>% filter(!is.na("STORAGE_SEASON_START") | !is.na("STORAGE_SEASON_END") |
                      !is.na("DIRECT_DIV_SEASON_START") | !is.na("DIRECT_DIV_SEASON_END"))    # no change in no.of rows

# filter to only those w/ active POD_STATUS
NV_table_filter <- filter(NV_table_filter, POD_STATUS == "Active")

# Select necessary columns from NV_table_filter
NV_table_filter <- NV_table_filter %>%
  select(
    contains("MEAN_DIV", ignore.case = TRUE), # selects all columns with "MEAN_DIV" in the name
    # Select specific columns
    "POD_ID",
    "LATITUDE",
    "LONGITUDE",
    "APPLICATION_NUMBER",
    "APPLICATION_PRIMARY_OWNER",
    "WATER_RIGHT_STATUS",
    "SOURCE_NAME",
    "TRIB_DESC",
    "WATERSHED",
    "PARTY_ID",
    "STORAGE_SEASON_START",
    "STORAGE_SEASON_END",
    "DIRECT_DIV_SEASON_START",
    "DIRECT_DIV_SEASON_END",
    "FACE_VALUE_AMOUNT",
    "FACE_VALUE_UNITS",
    "INI_REPORTED_DIV_AMOUNT",
    "INI_REPORTED_DIV_UNIT"
  )

### Prorate value for diversion/storage season ----

# Create new df/tibble with rights that have specified direct diversion and/or storage season
NV_table_filter_DivSto <- NV_table_filter %>%
  filter(!is.na(DIRECT_DIV_SEASON_START) | !is.na(STORAGE_SEASON_START))

# Determine number of days in diversion season

NV_table_filter_DivSto <- NV_table_filter_DivSto |>
  mutate(DIV_START = DIRECT_DIV_SEASON_START |>
           as_date(format = "%m/%d"),
         DIV_END = DIRECT_DIV_SEASON_END |>
           as_date(format = "%m/%d"))

for (i in 1:nrow(NV_table_filter_DivSto)) {
  
  DivStart <- NV_table_filter_DivSto$DIV_START[i] |> yday()
  DivEnd <- NV_table_filter_DivSto$DIV_END[i] |> yday()
  
  if (is.na(DivStart)) {
    
    NV_table_filter_DivSto[["DIV_DAYS_LIST"]][i] <- NA
    NV_table_filter_DivSto[["DIV_DAYS_LENGTH"]][i] <- 0
    
    next
  }
  
    if (DivStart > DivEnd) {
    
    diversionDays <- c(DivStart:366,
                       1:DivEnd)
    
  } else {
    
    diversionDays <- DivStart:DivEnd
    
  }
  
  NV_table_filter_DivSto[["DIV_DAYS_LIST"]][i] <- list(diversionDays)
  
  NV_table_filter_DivSto[["DIV_DAYS_LENGTH"]][i] <- length(diversionDays)
  
}

# Determine number of days in storage season
NV_table_filter_DivSto <- NV_table_filter_DivSto |>
  mutate(STO_START = STORAGE_SEASON_START |>
           as_date(format = "%m/%d") |> yday(),
         STO_END = STORAGE_SEASON_END |>
           as_date(format = "%m/%d") |> yday()) 

for (i in 1:nrow(NV_table_filter_DivSto)) {
  
    StoStart <- NV_table_filter_DivSto$STO_START[i]
  StoEnd <- NV_table_filter_DivSto$STO_END[i]
  
  if (is.na(StoStart)) {
    
    NV_table_filter_DivSto[["STO_DAYS_LIST"]][i] <- NA
    NV_table_filter_DivSto[["STO_DAYS_LENGTH"]][i] <- 0
    
    next
  }
  
  if (StoStart > StoEnd) {
    storageDays <- c(StoStart:366,
                     1:StoEnd)
  } else {
    
    storageDays <- StoStart:StoEnd
  }
  
  NV_table_filter_DivSto[["STO_DAYS_LIST"]][i] <- list(storageDays)
  NV_table_filter_DivSto[["STO_DAYS_LENGTH"]][i] <- length(storageDays)
}

# Conditions to calculate length of superseason ----
NV_table_filter_DivSto <- NV_table_filter_DivSto |> add_column(SUPERSEASON_LENGTH = 0)

for (i in 1:nrow(NV_table_filter_DivSto)) {
  
  
  if (NV_table_filter_DivSto$DIV_DAYS_LENGTH[i] == 0) {
    
    superseasonLength <- NV_table_filter_DivSto$STO_DAYS_LENGTH[i]
  } else if (NV_table_filter_DivSto$STO_DAYS_LENGTH[i] == 0) {
    
    superseasonLength <- NV_table_filter_DivSto$DIV_DAYS_LENGTH[i]
    
  } else {
    superseasonLength <- c(NV_table_filter_DivSto$DIV_DAYS_LIST[[i]],
                           NV_table_filter_DivSto$STO_DAYS_LIST[[i]]) |>
      unique() |>
      length() 
  }
  
  
  NV_table_filter_DivSto$SUPERSEASON_LENGTH[i] <- superseasonLength
}

# Calculate prorated value ----
NV_table_filter_DivSto <- NV_table_filter_DivSto |> add_column(PRORATED_VALUE = 0)

for (i in 1:nrow(NV_table_filter_DivSto)) {
  proratedValue <- NV_table_filter_DivSto$FACE_VALUE_AMOUNT[i] / NV_table_filter_DivSto$SUPERSEASON_LENGTH[i]
  
  NV_table_filter_DivSto$PRORATED_VALUE[i] <- proratedValue
}

NV_table_filter_DivSto <- NV_table_filter_DivSto |> add_column(PRORATED_VALUE_UNITS = "AF per SS DAY")

# check if any prorated values are NA
for (i in 1:nrow(NV_table_filter_DivSto)) {
  
  prorate_NA <- list(is.na(NV_table_filter_DivSto$PRORATED_VALUE[i]))
  
}

# Save as csv ----
write_csv(NV_table_filter_DivSto, "./Outputs/NV_proratedFaceValues.csv")
save(NV_table_filter_DivSto, file = "./Outputs/NV_proratedFaceValues.RData", ascii = TRUE)