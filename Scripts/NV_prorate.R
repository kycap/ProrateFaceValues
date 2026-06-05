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
  select(-APPLICATION_NUMBER) |> rename(APPLICATION_NUMBER = ORIGINAL_APPLICATION_NUMBER)

### Join and filter ----
# Join fields from the two tables

NV_table <- inner_join(ewrims_ff, mdt, by = "APPLICATION_NUMBER")   # returns more rows than exist in MDT
"POD_STATUS" %in% names(NV_table)

# filter to those that have values in any of specified fields
NV_table_filter <-  NV_table %>% filter(!is.na("STORAGE_SEASON_START") | !is.na("STORAGE_SEASON_END") |
                      !is.na("DIRECT_DIV_SEASON_START") | !is.na("DIRECT_DIV_SEASON_END"))    # no change in no.of rows

# filter to only those w/ active POD_STATUS
NV_table_filter <- filter(NV_table_filter, POD_STATUS == "Active")

### Prorate value across calendar year ----
# Prorated monthly value for active POD over entire year
NV_table_filter <- NV_table_filter %>% add_column(PRORATE_VAL = 0)

NV_table_filter <- NV_table_filter %>%
  mutate(PRORATE_VAL = FACE_VALUE_AMOUNT_AF / 12) #prorated value (acre-ft) per month

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

# NV_table_filter_DivSto$STO_START |> yday()
# NV_table_filter_DivSto$STO_END |> yday()
# 
# NV_table_filter_DivSto$STO_DAYS_LIST <- nrow(NV_table_filter_DivSto)
# NV_table_filter_DivSto$STO_DAYS_LENGTH <- nrow(NV_table_filter_DivSto)



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

# NV_table_filter_DivSto |> 
#   #mutate(DIV_DAYS_LENGTH = lengths(DIV_DAYS_LIST)) |>
#   select(DIV_DAYS_LIST, DIV_DAYS_LENGTH)
# 
# DivStart <- NV_table_filter_DivSto$DIRECT_DIV_SEASON_START
# DivEnd <- NV_table_filter_DivSto$DIRECT_DIV_SEASON_END
# 
# StoStart <- NV_table_filter_DivSto$STORAGE_SEASON_START
# StoEnd <- NV_table_filter_DivSto$STORAGE_SEASON_END
# 
# # Takes the above (which are "character" class, converts to date, then gives the number of the day of the year)
# DivStart <- DivStart |>
#   as_date(format = "%m/%d") |>
#   yday()
# DivEnd <- DivEnd |>
#   as_date(format = "%m/%d") |>
#   yday()
# StoStart <- StoStart |>
#   as_date(format = "%m/%d") |>
#   yday()
# StoEnd <- StoEnd |>
#   as_date(format = "%m/%d") |>
#   yday()

# if statement returns error, condition has length > 1
# searching suggests that I should use ifelse for vector longer than length 1
# tried to adapt the if statement into ifelse; it runs but everything is NA
# if (DivStart > DivEnd) {
#   
#   diversionDays <- c(DivStart:366,
#                     1:DivEnd)
#   
# } else {
#   
#   diversionDays <- DivStart:DivEnd
#   
# }
# 
# ifelse(DivStart > DivEnd, diversionDays <- c(DivStart:366, 1:DivEnd), diversionDays <- DivStart:DivEnd)
# 
# if (StoStart > StoEnd) {
#   
#   storageDays <- c(StoStart:366,
#                      1:StoEnd)
#   
# } else {
#   
#   storageDays <- StoStart:StoEnd
#   
# }
# 
# length(diversionDays)
# length(storageDays)
# 
# water_right_season <- c(diversionDays, storageDays) |>
#   unique()
# 
# length(water_right_season)

# Create new column (filled w/ zeros) for number of diversion days
#NV_table_filter_DirDiv <- NV_table_filter_DirDiv %>% add_column(DIV_DAYS = 0)

# Calculate the number of diversion days based on the DIRECT_DIV_SEASON_START, DIRECT_DIV_SEASON_END fields
#NV_table_filter_DirDiv <- NV_table_filter_DirDiv %>%
#  mutate(DIV_DAYS = interval(DIRECT_DIV_SEASON_START, DIRECT_DIV_SEASON_END))

# Calculate prorated value for the diversion season (FACE_VALUE_AMOUNT_AF / DIV_DAYS)
