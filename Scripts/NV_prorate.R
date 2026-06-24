### ----
# Prorating face values in the Navarro River watershed

### Setup ----

#install.packages("tidyverse")
library(tidyverse)

base::remove(list = ls())

### Load Files ----

ewrims_ff <- read_csv("./referenceFiles/ewrims_flat_file_pod.csv", guess_max = 10^6)
"POD_STATUS" %in% names(ewrims_ff)

mdt <- read_csv("./referenceFiles/NV_2017_2024_MDT_2026-03-11.csv") |>
  select(-APPLICATION_NUMBER) |> rename(APPLICATION_NUMBER = ORIGINAL_APPLICATION_NUMBER) |>
  select("APPLICATION_NUMBER", contains("MEAN_DIV", ignore.case = TRUE))
  

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

### Prorate value across calendar year ----
# Prorated monthly value for active POD over entire year
# NV_table_filter <- NV_table_filter %>% add_column(PRORATE_VAL = 0)
# 
# NV_table_filter <- NV_table_filter %>%
#   mutate(PRORATE_VAL = FACE_VALUE_AMOUNT_AF / 12) #prorated value (acre-ft) per month

### Prorate value for diversion/storage season ----
# Calculate prorated value for rights with specified direct diversion season

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
  
  #StoStart <- NV_table_filter_DivSto$STO_START[i] |> yday()
  #StoEnd <- NV_table_filter_DivSto$STO_END[i] |> yday()
  
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

#NV_table_filter_DivSto <- NV_table_filter_DivSto |> add_column(SS_days = 0) # create column for no. of days in superseason

# 1. If storage season is zero
for (i in 1:nrow(NV_table_filter_DivSto)) {
  if (NV_table_filter_DivSto$STO_DAYS_LENGTH[i] == 0) {
    NV_table_filter_DivSto$SS_days[i] = NV_table_filter_DivSto$DIV_DAYS_LENGTH[i]
  }
}

# 2. If direct diversion season is zero
for (i in 1:nrow(NV_table_filter_DivSto)) {
  if (NV_table_filter_DivSto$DIV_DAYS_LENGTH[i] == 0) {
    NV_table_filter_DivSto$SS_days[i] = NV_table_filter_DivSto$STO_DAYS_LENGTH[i]
  }
}

NV_table_filter_DivSto <- NV_table_filter_DivSto |> add_column(SS_start = 0) # ordinal starting day of superseason
NV_table_filter_DivSto <- NV_table_filter_DivSto |> add_column(SS_end = 0) # ordinal ending day of superseason

# 3. If diversion start date is less than or equal to storage start date, the superseason start equals diversion start
for (i in 1:nrow(NV_table_filter_DivSto)) {
  if (!is.na(NV_table_filter_DivSto$DIV_DAYS_LIST[[i]][1]) <= !is.na(NV_table_filter_DivSto$STO_DAYS_LIST[[i]][1])) {
    NV_table_filter_DivSto$SS_start = NV_table_filter_DivSto$DIV_DAYS_LIST[[i]][1]
  }
}

#4. If diversion start date is greater than storage start date, superseason start equals storage start
for (i in 1:nrow(NV_table_filter_DivSto)) {
  if (!is.na(NV_table_filter_DivSto$DIV_DAYS_LIST[[i]][1] > !is.na(NV_table_filter_DivSto$STO_DAYS_LIST[[i]][1]))) {
    NV_table_filter_DivSto$SS_start = NV_table_filter_DivSto$STO_DAYS_LIST[[i]][1]
  }
}

# 5. If diversion end date is less than or equal to storage end, then superseason end equals storage end


# 6. If diversion end date is greater than storage end, then superseason end equals diversion end





# NV_table_filter_DivSto$STO_START |> yday()
# NV_table_filter_DivSto$STO_END |> yday()
# 
# NV_table_filter_DivSto$STO_DAYS_LIST <- nrow(NV_table_filter_DivSto)
# NV_table_filter_DivSto$STO_DAYS_LENGTH <- nrow(NV_table_filter_DivSto)





# Ultimate length of combined diversion/storage season ----
# This length used for prorated value
# Account for overlaps in the seasons (e.g., div: 12/01 - 03/31, sto: 01/01 - 03/31)

NV_table_filter_DivSto <- NV_table_filter_DivSto %>%
  mutate(
    # raw durations
    diversion_len = as.numeric(difftime(DIV_END, DIV_START, units = "days")),
    storage_len   = as.numeric(difftime(STO_END, STO_START, units = "days")),
    
    # overlap between the two intervals
    overlap = pmax(
      0,
      as.numeric(
        difftime(
          pmin(DIV_END, STO_END),
          pmax(DIV_START, STO_START),
          units = "days"
        )
      )
    ),
    
    # final non-overlapping total
    total_length = diversion_len + storage_len - overlap
  )
# above produced NA when there were NA in any of the date columns

NV_table_filter_DivSto <- NV_table_filter_DivSto %>%
  mutate(
    # durations for diversion and storage
    diversion_len = if_else(
      !is.na(DIV_START) & !is.na(DIV_END),
      as.numeric(DIV_END - DIV_START),
      NA_real_
    ),
    storage_len = if_else(
      !is.na(STO_START) & !is.na(STO_END),
      as.numeric(STO_END - STO_START),
      NA_real_
    ),
    
    # the overlap b/w DIV and STO
    overlap = if_else(
      !is.na(DIV_START) & !is.na(DIV_END) &
      !is.na(STO_START) & !is.na(STO_END),
      pmax(
        0,
        as.numeric(
          pmin(DIV_END, STO_END) - pmax(DIV_START, STO_START)
        )
      ),
      NA_real_
    ),
    
    # Total length (after removing overlap)
    total_length = diversion_len + storage_len - overlap
  )
# above also returned NAs for total length. I already have DIV_DAYS_LENGTH
# and STO_DAYS_LENGTH, so I think I can start from calc. overlap

NV_table_filter_DivSto <- NV_table_filter_DivSto |>
  mutate(
    OVERLAP = if_else(
      !is.na(DIV_START) & !is.na(DIV_END) &
      !is.na(STO_START) & !is.na(STO_END),
      pmax(
        0, as.numeric(
          pmin(DIV_END, STO_END) - pmax(DIV_START, STO_START)
        )
      ),
      NA_real_
    )
  )

NV_table_filter_DivSto <- NV_table_filter_DivSto |>
  mutate(
    DAYS_COUNT = DIV_DAYS_LENGTH + STO_DAYS_LENGTH - OVERLAP
  )



NV_table_filter_DivSto <- NV_table_filter_DivSto |>
  mutate(
    OVERLAP = case_when(
      # If either interval is missing → no overlap
      is.na(DIV_START) | is.na(DIV_END) |
        is.na(STO_START) | is.na(STO_END) ~ 0,
      
      # Otherwise compute overlap
      TRUE ~ pmax(
        0,
        pmin(DIV_END, STO_END) - pmax(DIV_START, STO_START)
      )
    ),
    
    DAYS_COUNT = DIV_DAYS_LENGTH + STO_DAYS_LENGTH - OVERLAP
  )
