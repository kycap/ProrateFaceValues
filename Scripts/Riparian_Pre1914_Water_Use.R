# Estimating riparian and pre-1914 water use

# Setup ----
base::remove(list = ls())
library(tidyverse)
library(ggplot2)

# load file ----
NV_table <- load("./Outputs/NV_Full_Dataframe.Rdata")

# Select only the statements (APPLICATION_NUMBER begins with S) ----

# returns vector of row indices where APPLICATION_NUMBER starts with S by using regular expression
starts_with_s <- str_extract(NV_df$APPLICATION_NUMBER, "^[S]")
s_rows <- which(!is.na(starts_with_s)) # row indices where APPLICATION_NUMBER starts with S

NV_Statements <- NV_df[s_rows, ]

# select relevant columns
NV_Statements <- NV_Statements |>
  select(APPLICATION_NUMBER, POD_ID, INI_REPORTED_DIV_AMOUNT, INI_REPORTED_DIV_UNIT,
         JAN_MEAN_DIV, FEB_MEAN_DIV, MAR_MEAN_DIV, APR_MEAN_DIV, MAY_MEAN_DIV, JUN_MEAN_DIV,
         JUL_MEAN_DIV, AUG_MEAN_DIV, SEP_MEAN_DIV, OCT_MEAN_DIV, NOV_MEAN_DIV, DEC_MEAN_DIV,
         SUPERSEASON_LENGTH, JAN_COUNT, FEB_COUNT, MAR_COUNT, APR_COUNT, MAY_COUNT, JUN_COUNT,
         JUL_COUNT, AUG_COUNT, SEP_COUNT, OCT_COUNT, NOV_COUNT, DEC_COUNT)

# Add columns to df to begin analysis ----

NV_Statements <- NV_Statements |>
  add_column(SS_DAILY_AMT = 0)

for (i in 1:nrow(NV_Statements)) {
  NV_Statements$SS_DAILY_AMT[i] = NV_Statements$INI_REPORTED_DIV_AMOUNT[i] / NV_Statements$SUPERSEASON_LENGTH[i]
} # the amount diverted, on average, per superseason day (AF / SS day)

# Calculate monthly estimated use ----

# columns for estimated monthly use
NV_Statements <- NV_Statements |>
  add_column(
    JAN_PRO_AMT = 0, FEB_PRO_AMT = 0, MAR_PRO_AMT = 0, APR_PRO_AMT = 0, MAY_PRO_AMT = 0, JUN_PRO_AMT = 0,
    JUL_PRO_AMT = 0, AUG_PRO_AMT = 0, SEP_PRO_AMT = 0, OCT_PRO_AMT = 0, NOV_PRO_AMT = 0, DEC_PRO_AMT = 0
  )

# JAN
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$JAN_PRO_AMT[i] = NV_Statements$SS_DAILY_AMT[i] * NV_Statements$JAN_COUNT[i]
}

# FEB
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$FEB_PRO_AMT[i] = NV_Statements$SS_DAILY_AMT[i] * NV_Statements$FEB_COUNT[i]
}

# MAR
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$MAR_PRO_AMT[i] = NV_Statements$SS_DAILY_AMT[i] * NV_Statements$MAR_COUNT[i]
}

# APR
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$APR_PRO_AMT[i] = NV_Statements$SS_DAILY_AMT[i] * NV_Statements$APR_COUNT[i]
}

# MAY
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$MAY_PRO_AMT[i] = NV_Statements$SS_DAILY_AMT[i] * NV_Statements$MAY_COUNT[i]
}

# JUN
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$JUN_PRO_AMT[i] = NV_Statements$SS_DAILY_AMT[i] * NV_Statements$JUN_COUNT[i]
}

# JUL
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$JUL_PRO_AMT[i] = NV_Statements$SS_DAILY_AMT[i] * NV_Statements$JUL_COUNT[i]
}

# AUG
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$AUG_PRO_AMT[i] = NV_Statements$SS_DAILY_AMT[i] * NV_Statements$AUG_COUNT[i]
}

# SEP
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$SEP_PRO_AMT[i] = NV_Statements$SS_DAILY_AMT[i] * NV_Statements$SEP_COUNT[i]
}

# OCT
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$OCT_PRO_AMT[i] = NV_Statements$SS_DAILY_AMT[i] * NV_Statements$OCT_COUNT[i]
}

# NOV
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$NOV_PRO_AMT[i] = NV_Statements$SS_DAILY_AMT[i] * NV_Statements$NOV_COUNT[i]
}
# DEC
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$DEC_PRO_AMT[i] = NV_Statements$SS_DAILY_AMT[i] * NV_Statements$DEC_COUNT[i]
}

# Replace NA with zeros to indicate no water usage that month

# JAN
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$JAN_PRO_AMT[i][is.na(NV_Statements$JAN_PRO_AMT[i])] <- 0
}

# FEB
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$FEB_PRO_AMT[i][is.na(NV_Statements$FEB_PRO_AMT[i])] <- 0
}

# MAR
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$MAR_PRO_AMT[i][is.na(NV_Statements$MAR_PRO_AMT[i])] <- 0
}

# APR
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$APR_PRO_AMT[i][is.na(NV_Statements$APR_PRO_AMT[i])] <- 0
}

# MAY
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$MAY_PRO_AMT[i][is.na(NV_Statements$MAY_PRO_AMT[i])] <- 0
}

# JUN
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$JUN_PRO_AMT[i][is.na(NV_Statements$JUN_PRO_AMT[i])] <- 0
}

# JUL
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$JUL_PRO_AMT[i][is.na(NV_Statements$JUL_PRO_AMT[i])] <- 0
}

# AUG
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$AUG_PRO_AMT[i][is.na(NV_Statements$AUG_PRO_AMT[i])] <- 0
}

# SEP
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$SEP_PRO_AMT[i][is.na(NV_Statements$SEP_PRO_AMT[i])] <- 0
}

# OCT
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$OCT_PRO_AMT[i][is.na(NV_Statements$OCT_PRO_AMT[i])] <- 0
}

# NOV
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$NOV_PRO_AMT[i][is.na(NV_Statements$NOV_PRO_AMT[i])] <- 0
}

# DEC
for (i in 1:nrow(NV_Statements)) {
  NV_Statements$DEC_PRO_AMT[i][is.na(NV_Statements$DEC_PRO_AMT[i])] <- 0
}

# Plotting ----

# create df, select specified columns, incl. those that contain <something>_COUNT
# pivot_longer increases number of rows, decreases number of columns (takes columns and turns them into rows)
# the cols argument in pivot_longer is those that contain _COUNT
# pivot_longer takes those cols and turns them into rows, creating a new column called MONTH_COUNT
# their values (e.g., JAN_COUNT from NV_df) are put into a new column called COUNT
countDF <- NV_Statements |>
  select(APPLICATION_NUMBER, POD_ID, contains("_COUNT")) |>
  pivot_longer(contains("_COUNT"), names_to = "MONTH_COUNT", values_to = "COUNT")

proDF <- NV_Statements |>
  select(APPLICATION_NUMBER, POD_ID, contains("_PRO_AMT")) |>
  pivot_longer(contains("_PRO_AMT"), names_to = "MONTH_PRORATED", values_to = "PRORATED_FACE_VALUE")

monthlyDF <- NV_Statements |>
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

NV_Statements |>
  select(APPLICATION_NUMBER, POD_ID, contains("_MEAN_DIV"), contains("_PRO_AMT")) |>
  pivot_longer(contains("_MEAN_DIV") | contains("_PRO_AMT"), names_to = "MONTHLY_COL", values_to = "MONTHLY_VALUE") |>
  mutate(MONTH = str_extract(MONTHLY_COL, "^[A-Z]{3}") |> factor(toupper(month.abb))) |>
  mutate(TYPE = str_extract(MONTHLY_COL, "[A-Z]+_[A-Z]+$")) |>
  ggplot() +
  geom_point(mapping = aes(x = MONTH, y = MONTHLY_VALUE, colour = TYPE)) +
  xlab("") + ylab("Water Use (AF)") +
  theme_bw()

# plot on pseudo-log10 scale to more easily see difference
library(scales)
NV_Statements |>
  select(APPLICATION_NUMBER, POD_ID, contains("_MEAN_DIV"), contains("_PRO_AMT")) |>
  pivot_longer(contains("_MEAN_DIV") | contains("_PRO_AMT"), names_to = "MONTHLY_COL", values_to = "MONTHLY_VALUE") |>
  mutate(MONTH = str_extract(MONTHLY_COL, "^[A-Z]{3}") |> factor(toupper(month.abb))) |>
  mutate(TYPE = str_extract(MONTHLY_COL, "[A-Z]+_[A-Z]+$")) |>
  ggplot() +
  geom_point(mapping = aes(x = MONTH, y = MONTHLY_VALUE, colour = TYPE)) +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10), 
    labels = comma
  ) +
  xlab("") + ylab("Water Use (AF)") +
  theme_bw()

# Average all the reported and prorated monthly values ----
months_of_year <- factor(
  toupper(month.abb),
  levels = c("JAN", "FEB", "MAR", "APR",
             "MAY", "JUN", "JUL", "AUG",
             "SEP", "OCT", "NOV", "DEC")
  )

# reported values
JAN_MEAN <- mean(NV_Statements$JAN_MEAN_DIV)
FEB_MEAN <- mean(NV_Statements$FEB_MEAN_DIV)
MAR_MEAN <- mean(NV_Statements$MAR_MEAN_DIV)
APR_MEAN <- mean(NV_Statements$APR_MEAN_DIV)
MAY_MEAN <- mean(NV_Statements$MAY_MEAN_DIV)
JUN_MEAN <- mean(NV_Statements$JUN_MEAN_DIV)
JUL_MEAN <- mean(NV_Statements$JUL_MEAN_DIV)
AUG_MEAN <- mean(NV_Statements$AUG_MEAN_DIV)
SEP_MEAN <- mean(NV_Statements$SEP_MEAN_DIV)
OCT_MEAN <- mean(NV_Statements$OCT_MEAN_DIV)
NOV_MEAN <- mean(NV_Statements$NOV_MEAN_DIV)
DEC_MEAN <- mean(NV_Statements$DEC_MEAN_DIV)
MEAN_OF_REPORTED_VALUES <- c(JAN_MEAN, FEB_MEAN, MAR_MEAN, APR_MEAN,
                             MAY_MEAN, JUN_MEAN, JUL_MEAN, AUG_MEAN,
                             SEP_MEAN, OCT_MEAN, NOV_MEAN, DEC_MEAN)


JAN_MEAN_PRORATED <- mean(NV_Statements$JAN_PRO_AMT)
FEB_MEAN_PRORATED <- mean(NV_Statements$FEB_PRO_AMT)
MAR_MEAN_PRORATED <- mean(NV_Statements$MAR_PRO_AMT)
APR_MEAN_PRORATED <- mean(NV_Statements$APR_PRO_AMT)
MAY_MEAN_PRORATED <- mean(NV_Statements$MAY_PRO_AMT)
JUN_MEAN_PRORATED <- mean(NV_Statements$JUN_PRO_AMT)
JUL_MEAN_PRORATED <- mean(NV_Statements$JUL_PRO_AMT)
AUG_MEAN_PRORATED <- mean(NV_Statements$AUG_PRO_AMT)
SEP_MEAN_PRORATED <- mean(NV_Statements$SEP_PRO_AMT)
OCT_MEAN_PRORATED <- mean(NV_Statements$OCT_PRO_AMT)
NOV_MEAN_PRORATED <- mean(NV_Statements$NOV_PRO_AMT)
DEC_MEAN_PRORATED <- mean(NV_Statements$DEC_PRO_AMT)
MEAN_OF_PRORATED_VALUES <- c(JAN_MEAN_PRORATED, FEB_MEAN_PRORATED, MAR_MEAN_PRORATED, APR_MEAN_PRORATED,
                             MAY_MEAN_PRORATED, JUN_MEAN_PRORATED, JUL_MEAN_PRORATED, AUG_MEAN_PRORATED,
                             SEP_MEAN_PRORATED, OCT_MEAN_PRORATED, NOV_MEAN_PRORATED, DEC_MEAN_PRORATED)

#Means_For_Comparison <- tibble(t(months_of_year), t(MEAN_OF_REPORTED_VALUES), t(MEAN_OF_PRORATED_VALUES)) 

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
  labs(x = "", y = "Mean of Values (AF)") +
  theme_bw()
ggsave("NV_Plot_All_Riparian_Means.jpeg", path = "./Outputs/", width = 1820, height = 720, units = "px", dpi = 300)

# Shiny dashboard----

library(shiny)

NV_long <- NV_Statements |>
  pivot_longer(
    cols = matches("MEAN_DIV$|PRO_AMT$"),
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