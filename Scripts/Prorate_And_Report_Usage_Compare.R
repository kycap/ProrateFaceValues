# Comparing monthly prorated usage with reported monthly usage

# Setup ----
base::remove(list = ls())
library(tidyverse)
library(ggplot2)

# load file ----
NV_table <- load("./Outputs/NV_proratedFaceValues.Rdata")

# Calculate number of superseason days in each calendar month ----

unique_days_month <- function(div_list, sto_list, year = 2024) {
  
  all_days <- unique(c(div_list, sto_list)) # takes div_list, sto_list and combines them; then removes duplicates, assigns result to all_days
  all_days <- all_days[!is.na(all_days)] # all_days is evaluated to return those that are not NA
  
  # past year parameter in front of "01-01" and call that the origin. 
  # get the date of all_days that have been formatted as YYYY-MM-DD
  dates <- as.Date(all_days, origin = paste0(year, "-01-01"))
  
  # create table called counts that will count the unique days in each water right's superseason
  counts <- table(month(dates))
  
  # create full_counts where 1:12 are counted then turned into numbers and each number corresponds to a month
  full_counts <- setNames(
    as.numeric(counts[as.character(1:12)] %||% 0), # return 1:12 or else return 0 (if left is not NULL return left)
    c("JAN_COUNT", "FEB_COUNT", "MAR_COUNT", "APR_COUNT", "MAY_COUNT", "JUN_COUNT",
      "JUL_COUNT", "AUG_COUNT", "SEP_COUNT", "OCT_COUNT", "NOV_COUNT", "DEC_COUNT") # 1 corresponds to JAN_COUNT, etc.
  )
  
  full_counts
}

# create new tibble with results of function
# add columns to new tibble with each right's days in superseason month
NV_df <- NV_table_filter_DivSto |> # add column called month_counts
  mutate(month_counts = map2(DIV_DAYS_LIST, STO_DAYS_LIST, unique_days_month)) |> #map2(vector1, vector2, function)
  # map2 iterates over the length of the vectors while executing the function (taking i-th element of each vector)
  unnest_wider(month_counts) # month_counts is a list of length 238; each row in NV_df has column month_counts
  # in month_counts is JAN_COUNT, FEB_COUNT, etc. w/ number of superseason days in each month
  # unnest_wider takes the list in each row, makes a column for each (e.g., JAN_COUNT, FEB_COUNT... column) and stores the number of days 

# Add columns to tibble for monthly prorated face value ----
NV_df <- NV_df |> add_column(
  JAN_PRO_FV = 0, FEB_PRO_FV = 0, MAR_PRO_FV = 0, APR_PRO_FV = 0,
  MAY_PRO_FV = 0, JUN_PRO_FV = 0, JUL_PRO_FV = 0, AUG_PRO_FV = 0,
  SEP_PRO_FV = 0, OCT_PRO_FV = 0, NOV_PRO_FV = 0, DEC_PRO_FV = 0
)

# Calculate monthly prorated face value based on SS days per month ----

# January
for (i in 1:nrow(NV_df)) {
  NV_df$JAN_PRO_FV[i] <- NV_df$JAN_COUNT[i] * NV_df$PRORATED_VALUE[i]
}

# February
for (i in 1:nrow(NV_df)) {
  NV_df$FEB_PRO_FV[i] <- NV_df$FEB_COUNT[i] * NV_df$PRORATED_VALUE[i]
}

# March
for (i in 1:nrow(NV_df)) {
  NV_df$MAR_PRO_FV[i] <- NV_df$MAR_COUNT[i] * NV_df$PRORATED_VALUE[i]
}

# April
for (i in 1:nrow(NV_df)) {
  NV_df$APR_PRO_FV[i] <- NV_df$APR_COUNT[i] * NV_df$PRORATED_VALUE[i]
}

# May
for (i in 1:nrow(NV_df)) {
  NV_df$MAY_PRO_FV[i] <- NV_df$MAY_COUNT[i] * NV_df$PRORATED_VALUE[i]
}

# June
for (i in 1:nrow(NV_df)) {
  NV_df$JUN_PRO_FV[i] <- NV_df$JUN_COUNT[i] * NV_df$PRORATED_VALUE[i]
}

# July
for (i in 1:nrow(NV_df)) {
  NV_df$JUL_PRO_FV[i] <- NV_df$JUL_COUNT[i] * NV_df$PRORATED_VALUE[i]
}

# August
for (i in 1:nrow(NV_df)) {
  NV_df$AUG_PRO_FV[i] <- NV_df$AUG_COUNT[i] * NV_df$PRORATED_VALUE[i]
}

# September
for (i in 1:nrow(NV_df)) {
  NV_df$SEP_PRO_FV[i] <- NV_df$SEP_COUNT[i] * NV_df$PRORATED_VALUE[i]
}

# October
for (i in 1:nrow(NV_df)) {
  NV_df$OCT_PRO_FV[i] <- NV_df$OCT_COUNT[i] * NV_df$PRORATED_VALUE[i]
}

# November
for (i in 1:nrow(NV_df)) {
  NV_df$NOV_PRO_FV[i] <- NV_df$NOV_COUNT[i] * NV_df$PRORATED_VALUE[i]
}

# December
for (i in 1:nrow(NV_df)) {
  NV_df$DEC_PRO_FV[i] <- NV_df$DEC_COUNT[i] * NV_df$PRORATED_VALUE[i]
}

# Replace NA w/ zeros (to rep. no diversion and/or storage in that month) ----
#January
for (i in 1:nrow(NV_df)) {
  NV_df$JAN_PRO_FV[i][is.na(NV_df$JAN_PRO_FV[i])] <- 0
}

# February
for (i in 1:nrow(NV_df)) {
  NV_df$FEB_PRO_FV[i][is.na(NV_df$FEB_PRO_FV[i])] <- 0
}

# March
for (i in 1:nrow(NV_df)) {
  NV_df$MAR_PRO_FV[i][is.na(NV_df$MAR_PRO_FV[i])] <- 0
}

#April
for (i in 1:nrow(NV_df)) {
  NV_df$APR_PRO_FV[i][is.na(NV_df$APR_PRO_FV[i])] <- 0
}

#May
for (i in 1:nrow(NV_df)) {
  NV_df$MAY_PRO_FV[i][is.na(NV_df$MAY_PRO_FV[i])] <- 0
}

# June
for (i in 1:nrow(NV_df)) {
  NV_df$JUN_PRO_FV[i][is.na(NV_df$JUN_PRO_FV[i])] <- 0
}

# July
for (i in 1:nrow(NV_df)) {
  NV_df$JUL_PRO_FV[i][is.na(NV_df$JUL_PRO_FV[i])] <- 0
}

# August
for (i in 1:nrow(NV_df)) {
  NV_df$AUG_PRO_FV[i][is.na(NV_df$AUG_PRO_FV[i])] <- 0
}

# September
for (i in 1:nrow(NV_df)) {
  NV_df$SEP_PRO_FV[i][is.na(NV_df$SEP_PRO_FV[i])] <- 0
}

# October
for (i in 1:nrow(NV_df)) {
  NV_df$OCT_PRO_FV[i][is.na(NV_df$OCT_PRO_FV[i])] <- 0
}

# November
for (i in 1:nrow(NV_df)) {
  NV_df$NOV_PRO_FV[i][is.na(NV_df$NOV_PRO_FV[i])] <- 0
}

# December
for (i in 1:nrow(NV_df)) {
  NV_df$DEC_PRO_FV[i][is.na(NV_df$DEC_PRO_FV[i])] <- 0
}

# Prepare df for plotting ----

# create df, select specified columns, incl. those that contain <something>_COUNT
# pivot_longer increases number of rows, decreases number of columns (takes columns and turns them into rows)
# the cols argument in pivot_longer is those that contain _COUNT
# pivot_longer takes those cols and turns them into rows, creating a new column called MONTH_COUNT
# their values (e.g., JAN_COUNT from NV_df) are put into a new column called COUNT
countDF <- NV_df |>
  select(APPLICATION_NUMBER, POD_ID, contains("_COUNT")) |>
  pivot_longer(contains("_COUNT"), names_to = "MONTH_COUNT", values_to = "COUNT")

proDF <- NV_df |>
  select(APPLICATION_NUMBER, POD_ID, contains("_PRO_FV")) |>
  pivot_longer(contains("_PRO_FV"), names_to = "MONTH_PRORATED", values_to = "PRORATED_FACE_VALUE")

monthlyDF <- NV_df |>
  select(APPLICATION_NUMBER, POD_ID, contains("_MEAN_DIV")) |>
  pivot_longer(contains("_MEAN_DIV"), names_to = "MEAN_MONTHLY", values_to = "MEAN_MONTHLY_VALUE")

# look in MONTH_COUNT column for any three uppercase characters at the start of the cell value
# take those three characters and put them in a new column (due to mutate) called MONTH
countDF <- countDF |>
  mutate(MONTH = str_extract(MONTH_COUNT, "^[A-Z]{3}"))

proDF <- proDF |>
  mutate(MONTH = str_extract(MONTH_PRORATED, "^[A-Z]{3}"))

monthlyDF <- monthlyDF |>
  mutate(MONTH = str_extract(MEAN_MONTHLY, "^[A-Z]{3}"))

# start w/ countDF then look at proDF. Join rows in the two dataframes by the specified fields
# after those two joined, do another join with monthlyDF based on the specified fields
longDF <- countDF |>
  full_join(proDF, by = c("APPLICATION_NUMBER", "POD_ID", "MONTH")) |>
  full_join(monthlyDF, by = c("APPLICATION_NUMBER", "POD_ID", "MONTH"))

# take the month abbreviations and capitalize them
# factor orders the months based on levels, in this case chronological levels
# take existing MONTH column, use factor to order it chronologically based on how month.abb orders the months
# plot w/ MONTH on x-axis, MEAN_MONTHLY_VALUE on y-axis 
longDF |>
  mutate(MONTH = factor(MONTH, toupper(month.abb))) |> # the factor argument: these are the valid month names (MONTH) and they should follow toupper(month.abb)
  ggplot() +
  geom_point(mapping = aes(x = MONTH, y = MEAN_MONTHLY_VALUE)) # x-axis plots in order prescribed by factor argument

# select fields from NV_df that meet the conditions
# pivot columns that contain MEAN_DIV or PRO_FV (turn them into rows, put original column name in MONTHLY_COL and original value into MONTHLY_VALUE)
# extract the first three uppercase letters from MONTHLY_COL and put them in a new column (from mutate) called MONTH
# create MONTH column using mutate, setting conditions w/ factor which determines valid names (from the str_extract) and their order (which follows toupper(month.abb))
# from MONTHLY_COL, extract any that begin with any uppercase letters followed by underscore, followed by uppercase letters to end of string (the $)
# put those in a new column (created by mutate) called TYPE
# plot now colors points based on TYPE (Face Value or Prorated Value)
NV_df |>
  select(APPLICATION_NUMBER, POD_ID, contains("_MEAN_DIV"), contains("_PRO_FV")) |>
  pivot_longer(contains("_MEAN_DIV") | contains("_PRO_FV"), names_to = "MONTHLY_COL", values_to = "MONTHLY_VALUE") |>
  mutate(MONTH = str_extract(MONTHLY_COL, "^[A-Z]{3}") |> factor(toupper(month.abb))) |>
  mutate(TYPE = str_extract(MONTHLY_COL, "[A-Z]+_[A-Z]+$")) |>
  ggplot() +
  geom_point(mapping = aes(x = MONTH, y = MONTHLY_VALUE, colour = TYPE)) +
  xlab("Month") + ylab("Water Use (AF/year)")

# try to break apart the above into steps
NV_df_select <- NV_df |>
  select(APPLICATION_NUMBER, POD_ID, contains("_MEAN_DIV"), contains("_PRO_FV"))

NV_df_long <- NV_df_select |>
  pivot_longer(contains("_MEAN_DIV") | contains("PRO_FV"), names_to = "MONTHLY_COL", values_to = "MONTHLY_VALUE") |>
  mutate(MONTH = str_extract(MONTHLY_COL, "^[A-Z]{3}") |> factor(toupper(month.abb))) |>
  mutate(TYPE = str_extract(MONTHLY_COL, "[A-Z]+_[A-Z]+$"))

plot_all <- ggplot(NV_df_long, mapping = aes(x = MONTH, y = MONTHLY_VALUE, color = TYPE)) +
  geom_point() + # it recreates the above plot!
  labs(x = "", y = "Monthly Value (AF)") +
  scale_color_discrete(labels = c("Reported Mean", "Prorated")) +
  theme_bw()

ggsave("NV_Plot_FaceValue_and_Prorated.jpeg", path = "./Outputs/", width = 1820, height = 720, units = "px", dpi = 300)

# Average all the reported and prorated monthly values ----
months_of_year <- factor(
  toupper(month.abb),
  levels = c("JAN", "FEB", "MAR", "APR",
             "MAY", "JUN", "JUL", "AUG",
             "SEP", "OCT", "NOV", "DEC")
)

# reported values
JAN_MEAN <- mean(NV_df$JAN_MEAN_DIV)
FEB_MEAN <- mean(NV_df$FEB_MEAN_DIV)
MAR_MEAN <- mean(NV_df$MAR_MEAN_DIV)
APR_MEAN <- mean(NV_df$APR_MEAN_DIV)
MAY_MEAN <- mean(NV_df$MAY_MEAN_DIV)
JUN_MEAN <- mean(NV_df$JUN_MEAN_DIV)
JUL_MEAN <- mean(NV_df$JUL_MEAN_DIV)
AUG_MEAN <- mean(NV_df$AUG_MEAN_DIV)
SEP_MEAN <- mean(NV_df$SEP_MEAN_DIV)
OCT_MEAN <- mean(NV_df$OCT_MEAN_DIV)
NOV_MEAN <- mean(NV_df$NOV_MEAN_DIV)
DEC_MEAN <- mean(NV_df$DEC_MEAN_DIV)
MEAN_OF_REPORTED_VALUES <- c(JAN_MEAN, FEB_MEAN, MAR_MEAN, APR_MEAN,
                             MAY_MEAN, JUN_MEAN, JUL_MEAN, AUG_MEAN,
                             SEP_MEAN, OCT_MEAN, NOV_MEAN, DEC_MEAN)


JAN_MEAN_PRORATED <- mean(NV_df$JAN_PRO_FV)
FEB_MEAN_PRORATED <- mean(NV_df$FEB_PRO_FV)
MAR_MEAN_PRORATED <- mean(NV_df$MAR_PRO_FV)
APR_MEAN_PRORATED <- mean(NV_df$APR_PRO_FV)
MAY_MEAN_PRORATED <- mean(NV_df$MAY_PRO_FV)
JUN_MEAN_PRORATED <- mean(NV_df$JUN_PRO_FV)
JUL_MEAN_PRORATED <- mean(NV_df$JUL_PRO_FV)
AUG_MEAN_PRORATED <- mean(NV_df$AUG_PRO_FV)
SEP_MEAN_PRORATED <- mean(NV_df$SEP_PRO_FV)
OCT_MEAN_PRORATED <- mean(NV_df$OCT_PRO_FV)
NOV_MEAN_PRORATED <- mean(NV_df$NOV_PRO_FV)
DEC_MEAN_PRORATED <- mean(NV_df$DEC_PRO_FV)
MEAN_OF_PRORATED_VALUES <- c(JAN_MEAN_PRORATED, FEB_MEAN_PRORATED, MAR_MEAN_PRORATED, APR_MEAN_PRORATED,
                             MAY_MEAN_PRORATED, JUN_MEAN_PRORATED, JUL_MEAN_PRORATED, AUG_MEAN_PRORATED,
                             SEP_MEAN_PRORATED, OCT_MEAN_PRORATED, NOV_MEAN_PRORATED, DEC_MEAN_PRORATED)

Means_For_Comparison <- tibble(months_of_year, MEAN_OF_REPORTED_VALUES, MEAN_OF_PRORATED_VALUES)

ggplot(data = Means_For_Comparison, mapping = aes(x = months_of_year, y = MEAN_OF_REPORTED_VALUES, group = 1)) + # group=1 tells ggplot that the points form one line
  geom_line() +
  geom_point() +
  labs(x = "", y = "MEAN OF VALUES (AF)")

Means_For_Comparison_Long <- Means_For_Comparison |>
  pivot_longer(
    cols = c(MEAN_OF_REPORTED_VALUES, MEAN_OF_PRORATED_VALUES),
    names_to = "Type",
    values_to = "Value"
  )
# cols: the columns in Means_For_Comparison that will become rows
# names_to: creates column that contains the name of the original columns; used in following ggplot to differentiate the data series and color them
# values_to: creates column of user-specified name that contains the values of the original columns

graph_compare_means <- ggplot(data = Means_For_Comparison_Long, aes(x = months_of_year, y = Value, color = Type, group = Type)) +
  geom_line() +
  geom_point() +
  scale_color_discrete(
    labels = c("MEAN_OF_PRORATED_VALUES" = "Prorated",
               "MEAN_OF_REPORTED_VALUES" = "Reported")
  ) +
  labs(x = "", y = "Water Use (AF)", title = "Monthly Mean for All Appropriative Rights") +
  theme_bw()
ggsave("NV_Plot_All_Appropriative_Means.jpeg", path = "./Outputs/", width = 1820, height = 720, units = "px", dpi = 300)

# Compare reported mean and prorated monthly values ----

library(shiny)

NV_long <- NV_df |>
  pivot_longer(
    cols = matches("MEAN_DIV$|PRO_FV$"),
    names_to = c("month", "type"),
    names_sep = "_",
    values_to = "value"
  )

NV_long <- NV_long |>
  mutate(month = factor(month,
                        levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                                   "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")))

ui <- fluidPage(
  titlePanel("Monthly Usage Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("App", "Select Application:",
                  choices = unique(NV_long$APPLICATION_NUMBER))
    ),
    
    mainPanel(
      plotOutput("usagePlot")
    )
  )
)

server <- function(input, output, session) {
  
  filtered <- reactive({
    NV_long |> filter(APPLICATION_NUMBER == input$App)
  })
  
  output$usagePlot <- renderPlot({
    ggplot(filtered(), aes(x = month, y = value, color = type, group = type)) +
      geom_point(size = 2) + 
      labs(
        x = "Month",
        y = "Usage",
        color = "Series",
        title = paste("Monthly Usage for Application", input$App)
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)

# Export ----
write_csv(NV_df_select, "./Outputs/NV_Compare_Usage_Proated_and_Reported.csv")
save(NV_df_select, file = "./Outputs/NV_Compare_Usage_Prorated_and_Reported.RData", ascii = TRUE)

write_csv(NV_df, "./Outputs/NV_Full_Dataframe.csv")
save(NV_df, file = "./Outputs/NV_Full_Dataframe.RData", ascii = TRUE)
