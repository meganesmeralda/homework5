
# Meta --------------------------------------------------------------------

## Title:  American Community Survey Data
## Author: Megan Zheng
## Date Created: 4/16/2025
## Date Edited:  4/16/2025


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, tidycensus, readr)


## see list of variable names and tables
variables_2012 <- load_variables(2012, "acs1", cache = TRUE)
filter(variables_2012, str_detect(name, "B27010"))


library(tidycensus)

# Make sure your key is loaded
readRenviron("~/.Renviron")

# Define the variables you want
vars <- c(
  "B27010_018", "B27010_020", "B27010_021", "B27010_022", "B27010_023", "B27010_024", "B27010_025",
  "B27010_033", "B27010_034", "B27010_036", "B27010_037", "B27010_038", "B27010_039", "B27010_040",
  "B27010_041", "B27010_050"
)

# Initialize list to store data
insurance_list <- list()

# Loop over years
for (t in 2012:2019) {
  message(paste("Getting data for", t))

  # Get ACS data at state level
  acs_data <- get_acs(
    geography = "state",
    variables = vars,
    year = t,
    survey = "acs1",
    output = "wide"
  )

  # Create a tibble from the relevant variables
  ins_dat <- acs_data %>%
    select(NAME, ends_with("E")) %>%  # Select only estimate columns
    rename_with(~ gsub("_E", "", .x)) %>%  # Clean column names
    rename(State = NAME) %>%
    rename(
      all_18to34 = B27010_018,
      employer_18to34 = B27010_020,
      direct_18to34 = B27010_021,
      medicare_18to34 = B27010_022,
      medicaid_18to34 = B27010_023,
      tricare_18to34 = B27010_024,
      va_18to34 = B27010_025,
      none_18to34 = B27010_033,
      all_35to64 = B27010_034,
      employer_35to64 = B27010_036,
      direct_35to64 = B27010_037,
      medicare_35to64 = B27010_038,
      medicaid_35to64 = B27010_039,
      tricare_35to64 = B27010_040,
      va_35to64 = B27010_041,
      none_35to64 = B27010_050
    ) %>%
    mutate(year = t)

  insurance_list[[as.character(t)]] <- ins_dat
}

# Combine all years
final.insurance <- bind_rows(insurance_list)

# Final processing
final.insurance <- final.insurance %>%
  mutate(
    adult_pop = all_18to34 + all_35to64,
    ins_employer = employer_18to34 + employer_35to64,
    ins_direct = direct_18to34 + direct_35to64,
    ins_medicare = medicare_18to34 + medicare_35to64,
    ins_medicaid = medicaid_18to34 + medicaid_35to64,
    uninsured = none_18to34 + none_35to64
  ) %>%
  select(State, year, adult_pop, ins_employer, ins_direct,
         ins_medicare, ins_medicaid, uninsured)

# Write to file
write_tsv(final.insurance, 'data/output/acs_insurance.txt')
